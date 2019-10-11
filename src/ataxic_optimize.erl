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
% list(..., const(a), ...) -> list(const(a), ...)
-spec remove_overridden_operations
   (
      list(ataxic:basic())
   )
   -> list(ataxic:basic()).
remove_overridden_operations (List) ->
   {_, Result} =
      lists:foldr
      (
         fun (Elem, CurrentResult) ->
            case CurrentResult of
               {done, _} -> CurrentResult;
               {ok, List} ->
                  case Elem of
                     #const{} -> {done, [Elem|List]};
                     _ -> {ok, [Elem|List]}
                  end
            end
         end,
         {ok, []},
         List
      ),
   Result.

-spec optimize_generic_update_sequence
   (
      list(ataxic:basic()),
      list(ataxic:basic()),
      fun((OP) -> boolean()),
      fun((IX, IX) -> boolean()),
      fun((IX, OP) -> Update),
      fun((Update) -> OP),
      fun((Update) -> IX)
   )
   -> ataxic:basic().
optimize_generic_update_sequence
(
   [],
   Result,
   _IsCompatible,
   _IndexIsBefore,
   _NewUpdate,
   _GetOperation,
   _GetIndex
) ->
   case Result of
      [] -> ataxic:current_value();
      [A] -> A;
      _ -> ataxic:sequence(Result)
   end;
optimize_generic_update_sequence
(
   UnsortedOPs,
   CurrentResults,
   IsCompatible,
   IndexIsBefore,
   NewUpdate,
   GetOperation,
   GetIndex
) ->
   % Get all field updates until you encounter something else
   {Updates, PotentiallyImportantOPs} =
      lists:splitwith(IsCompatible, UnsortedOPs),

   % Sort updates by index
   SortedUpdates = lists:sort(IndexIsBefore, Updates),

   % Merge all updates that are for the same index
   % LastIX, LastUpdateOPs correspond to the last updates that should be
   % merged but that were surprised by the sequence ending.
   {LastIX, LastUpdateOPs, OtherMergedUpdates} =
      lists:foldl
      (
         fun (Candidate, {CurrentIX, CurrentOPs, CurrentResult}) ->
            CandidateIX = GetIndex(Candidate),
            case (CandidateIX == CurrentIX) of
               true ->
                  {
                     CurrentIX,
                     [GetOperation(Candidate)|CurrentOPs],
                     CurrentResult
                  };

               _ ->
                  {
                     CandidateIX,
                     [GetOperation(Candidate)],
                     (
                        case CurrentOPs of
                           [] -> CurrentResult;
                           [OP] ->
                              [
                                 NewUpdate(CurrentIX, OP)
                                 |CurrentResult
                              ];
                           _ ->
                              [
                                 NewUpdate
                                 (
                                    CurrentIX,
                                    light
                                    (
                                       ataxic:sequence
                                       (
                                          lists:reverse(CurrentOPs)
                                       )
                                    )
                                 )
                                 |CurrentResult
                              ]
                        end
                     )
                  }
            end
         end,
         {-1, [], []},
         SortedUpdates
      ),

   % Add the merged LastUpdateOPs for LastIX
   MergedUpdates =
      (
         case LastUpdateOPs of
            [] -> OtherMergedUpdates;
            [OP] ->
               [
                  NewUpdate(LastIX, OP)
                  |OtherMergedUpdates
               ];
            _ ->
               [
                  NewUpdate
                  (
                     LastIX,
                     light(ataxic:sequence(lists:reverse(LastUpdateOPs)))
                  )
                  |OtherMergedUpdates
               ]
         end
      ),

   % Skip the OPs until we find another field update
   {ImportantOPs, PotentialUpdates} =
      lists:splitwith
      (
         fun (E) -> not IsCompatible(E) end,
         PotentiallyImportantOPs
      ),

   optimize_generic_update_sequence
   (
      PotentialUpdates,
      (CurrentResults ++ MergedUpdates ++ ImportantOPs),
      IsCompatible,
      IndexIsBefore,
      NewUpdate,
      GetOperation,
      GetIndex
   ).

-spec optimize_update_field_sequence (list(ataxic:basic())) -> ataxic:basic().
optimize_update_field_sequence (List) ->
   optimize_generic_update_sequence
   (
      List,
      [],
      fun (E) -> is_record(E, upfield) end,
      fun (A, B) -> ((A#upfield.ix) =< (B#upfield.ix)) end,
      fun ataxic:update_field/2,
      fun (E) -> E#upfield.op end,
      fun (E) -> E#upfield.ix end
   ).

-spec optimize_update_orddict_sequence (list(ataxic:basic())) -> ataxic:basic().
optimize_update_orddict_sequence (List) ->
   optimize_generic_update_sequence
   (
      List,
      [],
      fun (E) ->
         case E of
            #apply_fun
            {
               module = orddict,
               function = store,
               params = [ConstIX1, OP, #current{}]
            } ->
               case OP of
                  #const{} -> true;
                  #seq
                  {
                     ops =
                        [
                           #apply_fun
                           {
                           module = orddict,
                           function = fetch,
                           params = [ConstIX2, #current{}]
                        },
                        _
                     ]
               } -> (ConstIX1 == ConstIX2);
               _ -> false
            end;

            _ -> false
         end
      end,
      fun (A, B) ->
         [AIX|_] = A#apply_fun.params,
         [BIX|_] = B#apply_fun.params,
         (AIX =< BIX)
      end,
      fun ataxic_sugar:update_orddict_element/2,
      fun (E) ->
         [_,OP|_] = E#apply_fun.params,
         OP
      end,
      fun (E) ->
         [IX|_] = E#apply_fun.params,
         IX
      end
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
   S0Result = optimize_update_field_sequence(S3OPs),
   S1Result =
      case S0Result of
         #seq{ ops = S4OPs } -> optimize_update_orddict_sequence(S4OPs);
         _ -> S0Result
      end,

   S2Result =
      case S1Result of
         #seq{ ops = S5OPs } ->
            #seq{ ops = remove_overridden_operations(S5OPs) };
         _ -> S1Result
      end,

   S2Result;
aggressive (In = #apply_fun{ params = OPs }) ->
   In#apply_fun
   {
      params = lists:map(fun aggressive/1, OPs)
   };
aggressive (In = #list_cons{ param = OP }) ->
   In#list_cons{ param = aggressive(OP) };
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
   S0Result = optimize_update_field_sequence(S2OPs),
   S1Result =
      case S0Result of
         #seq{ ops = S4OPs } -> optimize_update_orddict_sequence(S4OPs);
         _ -> S0Result
      end,

   S2Result =
      case S1Result of
         #seq{ ops = S5OPs } ->
            #seq{ ops = remove_overridden_operations(S5OPs) };
         _ -> S1Result
      end,

   S2Result;
light (OP) -> OP.
