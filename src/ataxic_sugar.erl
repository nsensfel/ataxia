-module(ataxic_sugar).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      update_array_cell/2,
      update_orddict_element/2,
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
update_array_cell (IX, OP) ->
   ataxic:apply_function
   (
      array,
      set,
      [
         ataxic:constant(IX),
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
         ),
         ataxic:current_value()
      ]
   ).

-spec update_orddict_element
   (
      non_neg_integer(),
      ataxic:basic()
   )
   -> ataxic:basic().
update_orddict_element (IX, OP) ->
   ataxic:apply_function
   (
      orddict,
      store,
      [
         ataxic:constant(IX),
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
         ),
         ataxic:current_value()
      ]
   ).

-spec nop () -> ataxic:basic().
nop () -> ataxic:sequence([]).
