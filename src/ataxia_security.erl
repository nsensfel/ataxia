-module(ataxia_security).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type named_user() :: {'user', any()}.
-type user() :: (named_user() | 'admin' | 'any' | 'janitor').
-type permission() :: ordsets:ordset(user()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type([user/0, permission/0]).

-export([janitor/0, any/0, admin/0, user_from_id/1]).

-export
(
   [
      add_access/2,
      remove_access/2,
      allow_only/1,
      allow/1,
      allow_any/0,
      allow_none/0
   ]
).
-export([can_access/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec add_access (user(), permission()) -> permission().
add_access (User, Permission) ->
   ordsets:add_element(User, Permission).

-spec remove_access (user(), permission()) -> permission().
remove_access (User, Permission) ->
   ordsets:del_element(User, Permission).

-spec allow_only (user()) -> permission().
allow_only (User) ->
   ordsets:add_element(User, ordsets:new()).

-spec allow (list(user())) -> permission().
allow (Users) -> ordsets:from_list(Users).

-spec allow_any () -> permission().
allow_any () ->
   ordsets:add_element(any(), ordsets:new()).

-spec allow_none () -> permission().
allow_none () -> ordsets:new().

-spec user_from_id (any()) -> user().
user_from_id (ID) -> {user, ID}.

-spec janitor () -> user().
janitor () -> janitor.

-spec any () -> user().
any () -> any.

-spec admin () -> user().
admin () -> admin.

-spec can_access (user(), permission()) -> boolean().
can_access (User, Permission) ->
   case User of
      admin -> true;
      janitor -> ordsets:is_element(User, Permission);
      _ ->
         (
            ordsets:is_element(any, Permission)
            or ordsets:is_element(User, Permission)
         )
   end.
