/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2020, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(impl_profile_redis, []).
:- use_module(library(settings)).
:- use_module(library(apply)).
:- use_module(library(redis)).
:- use_module(library(lists)).
:- use_module(library(option)).

/** <module> User Profile backend

This module is a backend for   user_profile.pl, realising the presistent
store using a Redis db.  The properties of this profile are:

  - Depends on an accessible Redis database.
  - Profiles may be accessed from multiple nodes in a cluster.
*/

:- setting(user_profile:redis_server, atom, default,
	   "Redis server alias").
:- setting(user_profile:redis_prefix, atom, profiles,
	   "Common prefix for all profile keys").

:- public
    impl_profile_open_db/1,
    impl_profile_create/2,
    impl_current_profile/1,
    impl_current_profile/2,
    impl_profile_property/2,
    impl_set_profile/3,
    impl_profile_remove/1,
    impl_profile_remove/2,
    impl_profile_add_session/3,
    impl_profile_refresh_session/2,
    impl_profile_remove_session/2,
    impl_profile_session/2.

:- dynamic
    profile_db/2.                       % Server, Prefix

%!	impl_profile_open_db(+Options)

impl_profile_open_db(_Options) :-
    setting(user_profile:redis_server, ServerName),
    setting(user_profile:redis_prefix, Prefix),
    asserta(profile_db(ServerName, Prefix)).

%!	impl_profile_create(+ProfileID, +CanAttributes)

impl_profile_create(ProfileID, CanAttributes) :-
    profile_db(Server, Prefix),
    atom_concat(Prefix, ':users', Key),
    redis(Server, sadd(Key, ProfileID), _New),
    maplist(impl_set_profile(ProfileID), CanAttributes).

%!	impl_current_profile(?ProfileID)

impl_current_profile(ProfileID) :-
    profile_db(Server, Prefix),
    atom_concat(Prefix, ':users', Key),
    (   var(ProfileID)
    ->  redis_sscan(Server, Key, List, []),
        member(ProfileIDS, List),
        atom_string(ProfileID, ProfileIDS)
    ;   redis(Server, sismember(Key, ProfileID), 1)
    ).

%!	impl_current_profile(?ProfileID, ?Attributes)

impl_current_profile(ProfileID, Attributes) :-
    impl_current_profile(ProfileID),
    profile_db(Server, Prefix),
    atomic_list_concat([Prefix, user, ProfileID], :, Key),
    redis_get_hash(Server, Key, Attributes).

%!	impl_profile_property(?ProfileID, ?Attribute)

impl_profile_property(ProfileID, Attribute) :-
    impl_current_profile(ProfileID),
    profile_db(Server, Prefix),
    atomic_list_concat([Prefix, user, ProfileID], :, Key),
    (   callable(Attribute)
    ->  Attribute =.. [Name,Value],
        redis(Server, hget(Key, Name), Value)
    ;   redis_hscan(Server, Key, Pairs, []),
        member(NameS-Value, Pairs),
        atom_string(Name, NameS),
        Attribute =.. [Name,Value]
    ).

%!	impl_set_profile(+ProfileID, +CanAttribute, -Modified)

impl_set_profile(ProfileID, CanAttribute) :-
    impl_set_profile(ProfileID, CanAttribute, _).
impl_set_profile(ProfileID, CanAttribute, Modified) :-
    profile_db(Server, Prefix),
    CanAttribute =.. [Name,Value],
    atomic_list_concat([Prefix, user, ProfileID], :, Key),
    (   redis(Server, hget(Key, Name), Value)
    ->  Modified = false
    ;   redis(Server, hset(Key, Name, prolog(Value)))
    ).

%!  impl_profile_remove(+ProfileID)

impl_profile_remove(ProfileID) :-
    profile_db(Server, Prefix),
    atomic_list_concat([Prefix, user, ProfileID], :, Key),
    redis(Server, del(Key), _).

%!  impl_profile_remove(+ProfileID, +Attribute)

impl_profile_remove(ProfileID, Attribute) :-
    profile_db(Server, Prefix),
    atomic_list_concat([Prefix, user, ProfileID], :, Key),
    redis(Server, hdel(Key, Attribute), _).


		 /*******************************
		 *           SESSIONS		*
		 *******************************/

%!  impl_profile_add_session(+ProfileID, +SessionID, +Options)

impl_profile_add_session(ProfileID, SessionID, Options) :-
    option(timeout(Timeout), Options),
    Ex is integer(Timeout),
    profile_db(Server, Prefix),
    atomic_list_concat([Prefix, session, SessionID], :, Key),
    redis(Server, set(Key, ProfileID, ex, Ex), _).

%!  impl_profile_refresh_session(+ProfileID, +SessionID)

impl_profile_refresh_session(_ProfileID, _SessionID).

%!  impl_profile_remove_session(+ProfileID, +SessionID)

impl_profile_remove_session(_ProfileID, SessionID) :-
    profile_db(Server, Prefix),
    atomic_list_concat([Prefix, session, SessionID], :, Key),
    redis(Server, del(Key), _).

%!  impl_profile_session(?ProfileID, ?SessionID)

impl_profile_session(ProfileID, SessionID) :-
    nonvar(SessionID),
    !,
    profile_db(Server, Prefix),
    atomic_list_concat([Prefix, session, SessionID], :, Key),
    redis(Server, get(Key), ProfileIDS),
    atom_string(ProfileID, ProfileIDS).
impl_profile_session(ProfileID, SessionID) :-
    profile_db(Server, Prefix),
    atomic_list_concat([Prefix, session, *], :, Pattern),
    redis_scan(Server, Keys, [match(Pattern)]),
    member(Key, Keys),
    atom_string(SessionID, Key),
    redis(Server, get(Key), ProfileIDS),
    atom_string(ProfileID, ProfileIDS).
