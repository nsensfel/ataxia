%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ATAXIC SUGAR %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Functions that offer shortcuts to write commonly used operations in Ataxic.
%
-module(ataxic_sugar).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("ataxia/ataxic.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      update_array_cell/2,
      update_orddict_element/2,
      update_ordset/2,
      nop/0
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec update_array_cell (non_neg_integer(), ataxic:basic()) -> ataxic:basic().
update_array_cell (_IX, #current{}) -> nop();
update_array_cell (IX, OP) ->
   ataxic:apply_function
   (
      array,
      set,
      [
         ataxic:constant(IX),
         (
            case ataxic:is_constant(OP) of
               true -> OP;
               false ->
                  ataxic:sequence
                  (
                     [
                        ataxic:apply_function
                        (
                           array,
                           get,
                           [
                              ataxic:constant(IX),
                              ataxic:current_value()
                           ]
                        ),
                        OP
                     ]
                  )
            end
         ),
         ataxic:current_value()
      ]
   ).

-spec update_orddict_element
   (
      any(),
      ataxic:basic()
   )
   -> ataxic:basic().
update_orddict_element (_IX, #current{}) -> nop();
update_orddict_element (IX, OP) ->
   ataxic:apply_function
   (
      orddict,
      store,
      [
         ataxic:constant(IX),
         (
            case ataxic:is_constant(OP) of
               true -> OP;
               false ->
                  ataxic:sequence
                  (
                     [
                        ataxic:apply_function
                        (
                           orddict,
                           fetch,
                           [
                              ataxic:constant(IX),
                              ataxic:current_value()
                           ]
                        ),
                        OP
                     ]
                  )
            end
         ),
         ataxic:current_value()
      ]
   ).

-spec nop () -> ataxic:basic().
nop () -> ataxic:sequence([]).

-spec update_ordset
   (
      ordsets:ordset(any()),
      ordsets:ordset(any())
   ) -> ataxic:basic().
update_ordset (Old, New) ->
   Remove = ordsets:subtract(Old, New),
   Add = ordsets:subtract(New, Old),

   case {ordsets:is_empty(Add), ordsets:is_empty(Remove)} of
      {true, true} -> nop();
      {true, false} ->
         ataxic:apply_function
         (
            ordsets,
            substract,
            [
               ataxic:current_value(),
               ataxic:constant(Remove)
            ]
         );
      {false, true} ->
         ataxic:apply_function
         (
            ordsets,
            union,
            [
               ataxic:current_value(),
               ataxic:constant(Add)
            ]
         );
      {false, false} ->
         ataxic:sequence
         (
            [
               ataxic:apply_function
               (
                  ordsets,
                  substract,
                  [
                     ataxic:current_value(),
                     ataxic:constant(Remove)
                  ]
               ),
               ataxic:apply_function
               (
                  ordsets,
                  union,
                  [
                     ataxic:current_value(),
                     ataxic:constant(Add)
                  ]
               )
            ]
         )
   end.
