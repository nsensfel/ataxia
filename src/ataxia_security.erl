-module(ataxia_security).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type named_user() :: {'user', any()}.
-type user() :: (named_user() | 'admin' | 'any' | 'janitor').
-type permission() :: ordset:ordset(user()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type([user/0, permission/0]).

-export([janitor/0, any/0, admin/0, user_from_id/1]).

-export([add_access/2, remove_access/2, allow_only/1]).
-export([can_access/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec add_access (user(), permission()) -> permission().
add_access (User, Permission) ->
   ordset:add_element(User, Permission).

-spec remove_access (user(), permission()) -> permission().
remove_access (User, Permission) ->
   ordset:del_element(User, Permission).

-spec allow_only (user()) -> permission().
allow_only (User) ->
   ordset:add_element(User, ordset:new()).

-spec user_from_id (any()) -> user().
user_from_id (ID) -> {user, ID}.

-spec janitor () -> user().
janitor () -> janitor.

-spec any () -> user().
any () -> any.

-spec admin () -> user().
admin () -> admin.

-spec can_access (permission(), user()) -> boolean().
can_access (Permission, User) ->
   case User of
      admin -> true;
      _ ->
         (
            ordset:is_element(any, Permission)
            or ordset:is_element(User, Permission)
         )
   end.
