-module(ataxic_optimize).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("ataxia/ataxic.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([aggressive/1, light/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec optimize_update_field_sequence
   (
      list(ataxic:basic()),
      list(ataxic:basic())
   )
   -> ataxic:basic().
optimize_update_field_sequence ([], Result) ->
   case Result of
      [A] -> A;
      _ -> ataxic:sequence(Result)
   end;
optimize_update_field_sequence (UnsortedOPs, CurrentResults) ->
   {FieldUpdates, PotentiallyImportantOPs} =
      lists:splitwith(fun (E) -> is_record(E, upfield) end, UnsortedOPs),

   SortedFieldUpdates =
      lists:sort
      (
         fun (A, B) ->
            ((A#upfield.ix) =< (B#upfield.ix))
         end,
         FieldUpdates
      ),

   {LastIX, LastUpdateOPs, OtherMergedFieldUpdates} =
      lists:foldl
      (
         fun (Update, {CurrentIX, CurrentOPs, CurrentResult}) ->
            case (Update#upfield.ix == CurrentIX) of
               true ->
                  {CurrentIX, [Update#upfield.op|CurrentOPs], CurrentResult};

               _ ->
                  {
                     Update#upfield.ix,
                     [Update#upfield.op],
                     (
                        case CurrentOPs of
                           [] -> CurrentResult;
                           [OP] ->
                              [
                                 ataxic:update_field(CurrentIX, OP)
                                 |CurrentResult
                              ];
                           _ ->
                              [
                                 ataxic:update_field
                                 (
                                    CurrentIX,
                                    ataxic:sequence(CurrentOPs)
                                 )
                                 |CurrentResult
                              ]
                        end
                     )
                  }
            end
         end,
         {-1, [], []},
         SortedFieldUpdates
      ),

   MergedFieldUpdates =
      (
         case LastUpdateOPs of
            [] -> OtherMergedFieldUpdates;
            [OP] ->
               [
                  ataxic:update_field(LastIX, OP)
                  |OtherMergedFieldUpdates
               ];
            _ ->
               [
                  ataxic:update_field(LastIX, ataxic:sequence(LastUpdateOPs))
                  |OtherMergedFieldUpdates
               ]
         end
      ),
   {ImportantOPs, PotentialFieldUpdates} =
      lists:splitwith
      (
         fun (E) -> not is_record(E, upfield) end,
         PotentiallyImportantOPs
      ),

   optimize_update_field_sequence
   (
      PotentialFieldUpdates,
      (CurrentResults ++ MergedFieldUpdates ++ ImportantOPs)
   ).

-spec flatten_sequence (list(ataxic:basic())) -> list(ataxic:basic()).
flatten_sequence (OPs) ->
   lists:foldl
   (
      fun (E, CurrentOPs) ->
         case is_record(E, seq) of
            true -> (E#seq.ops ++ CurrentOPs);
            _ -> [E|CurrentOPs]
         end
      end,
      [],
      OPs
   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec aggressive
   (ataxic:basic()) -> ataxic:basic();
   (ataxic:meta()) -> ataxic:meta().
aggressive (In = #field{ op = OP }) ->
   In#field{ op = aggressive(OP) };
aggressive (In = #upfield{ op = S0OP}) ->
   S1OP = aggressive(S0OP),

   case S1OP of
      #current{} -> S1OP;
      _ -> In#upfield{ op = S1OP }
   end;
aggressive (#seq{ ops = S0OPs }) ->
   S1OPs = lists:map(fun aggressive/1, S0OPs),
   S2OPs = flatten_sequence(S1OPs),
   S3OPs = lists:filter(fun (E) -> (not is_record(E, current)) end, S2OPs),
   Result = optimize_update_field_sequence(S3OPs, []),

   case Result#seq.ops of
      [] -> #current{};
      _ -> Result
   end;
aggressive (In = #apply_fun{ params = OPs }) ->
   In#apply_fun
   {
      params = lists:map(fun aggressive/1, OPs)
   };
aggressive (In = #list_cons{ param = OP }) ->
   In#list_cons{ param = lists:map(fun aggressive/1, OP) };
aggressive (In = #read_perm{ op = OP }) ->
   In#read_perm{ op = aggressive(OP) };
aggressive (In = #write_perm{ op = OP }) ->
   In#write_perm{ op = aggressive(OP) };
aggressive (In = #lock{ op = OP }) ->
   In#lock{ op = aggressive(OP) };
aggressive (In = #value{ op = OP }) ->
   In#value{ op = aggressive(OP) };
aggressive (In = #mseq{ ops = OPs }) ->
   In#mseq{ ops = lists:map(fun aggressive/1, OPs) };
aggressive (Other) ->
   Other.

-spec light
   (ataxic:basic()) -> ataxic:basic();
   (ataxic:meta()) -> ataxic:meta().
light (#seq{ ops = S0OPs }) ->
   S1OPs = flatten_sequence(S0OPs),
   S2OPs = lists:filter(fun (E) -> (not is_record(E, current)) end, S1OPs),
   Result = optimize_update_field_sequence(S2OPs, []),

   case Result#seq.ops of
      [] -> #current{};
      _ -> Result
   end;
light (OP) -> OP.
