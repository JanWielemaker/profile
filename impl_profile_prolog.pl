/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, VU University Amsterdam
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

:- module(impl_profile_prolog, []).
:- use_module(library(persistency)).
:- use_module(library(settings)).
:- use_module(library(option)).

/** <module> User Profile backend as pure Prolog

This module is a backend for   user_profile.pl, realising the persistent
store as a set of Prolog files.  The properties of this profile are:

  - No dependencies and installation.
  - Scalabe to about 100,000 profiles.  Much larger sets will result
    in prohibitively long server startup time and memory usage.
  - Profiles can only be modified on one server.  Databases may
    be distributed and used on different servers.
*/

:- setting(user_profile:profile_db, atom, 'profiles.db',
	   "File holding profiles").
:- setting(user_profile:session_db, atom, 'sessions.db',
	   "File holding active sessions").

:- persistent
	impl_profile_prolog_profile:
	(   profile(atom),
	    profile_attribute(atom, callable)
	),
	impl_profile_prolog_session:
	(   session(atom, atom, number)
	).

:- public
	impl_profile_open_db/1,
	impl_profile_create/2,
	impl_current_profile/1,
	impl_current_profile/2,
	impl_profile_property/2,
	impl_set_profile/2,
	impl_profile_remove/1,
	impl_profile_remove/2,
	impl_profile_add_session/3,
	impl_profile_refresh_session/2,
	impl_profile_remove_session/2,
	impl_profile_session/2.

%%	impl_profile_open_db(+Options)

impl_profile_open_db(Options) :-
	setting(user_profile:profile_db, ProfileDB),
	setting(user_profile:session_db, SessionDB),
	db_attach(impl_profile_prolog_profile:ProfileDB, Options),
	db_attach(impl_profile_prolog_session:SessionDB, Options).

%%	impl_profile_create(+ProfileID, +CanAttributes)

impl_profile_create(ProfileID, CanAttributes) :-
	impl_profile_prolog_profile:assert_profile(ProfileID),
	maplist(impl_set_profile(ProfileID), CanAttributes).

%%	impl_current_profile(?ProfileID)

impl_current_profile(ProfileID) :-
	impl_profile_prolog_profile:profile(ProfileID).

%%	impl_current_profile(?ProfileID, ?Attributes)

impl_current_profile(ProfileID, Attributes) :-
	findall(Name-Value,
		impl_profile_prolog_profile:
		    profile_attribute(ProfileID, Name, Value),
		Pairs),
	dict_pairs(Attributes, user_profile, Pairs).

%%	impl_profile_property(?ProfileID, ?Attribute)

impl_profile_property(ProfileID, Attribute) :-
	callable(Attribute), !,
	Attribute =.. [Name,Value],
	impl_profile_prolog_profile:
	    profile_attribute(ProfileID, Name, Value).
impl_profile_property(ProfileID, Attribute) :-
	impl_profile_prolog_profile:
	    profile_attribute(ProfileID, Name, Value),
	Attribute =.. [Name,Value].

%%	impl_set_profile(+ProfileID, +CanAttribute)

impl_set_profile(ProfileID, CanAttribute) :-
	CanAttribute =.. [Name,Value],
	impl_profile_prolog_profile:
	    assert_profile_attribute(ProfileID, Name, Value).

%%	impl_profile_remove(+ProfileID)

impl_profile_remove(ProfileID) :-
	impl_profile_prolog_profile:
	    retractall_profile(ProfileID),
	impl_profile_prolog_profile:
	    retractall_profile_attribute(ProfileID, _, _).

%%	impl_profile_remove(+ProfileID, +Attribute)

impl_profile_remove(ProfileID, Attribute) :-
	impl_profile_prolog_profile:
	    retractall_profile_attribute(ProfileID, Attribute, _).


		 /*******************************
		 *	      SESSIONS		*
		 *******************************/

:- dynamic
	tmp_session/3,			% ProfileID, SessionID, DeadLine
	session_last_usage/2.		% SessionID, Time

%%	impl_profile_add_session(+ProfileID, +SessionID, +Options)

impl_profile_add_session(ProfileID, SessionID, Options) :-
	option(timeout(Timeout), Options),
	get_time(Now),
	(   option(persistent(true), Options)
	->  Deadline is Now+Timeout,
	    impl_profile_prolog_session:
		assert_session(ProfileID, SessionID, Deadline)
	;   asserta(tmp_session(ProfileID, SessionID, Timeout)),
	    asserta(session_last_usage(SessionID, Now))
	).

%%	impl_profile_refresh_session(+ProfileID, +SessionID)

impl_profile_refresh_session(ProfileID, SessionID) :-
	tmp_session(ProfileID, SessionID, _Timeout), !,
	get_time(Now),
	retractall(session_last_usage(SessionID, _)),
	asserta(session_last_usage(SessionID, Now)).
impl_profile_refresh_session(_ProfileID, _SessionID).

%%	impl_profile_remove_session(+ProfileID, +SessionID)

impl_profile_remove_session(ProfileID, SessionID) :-
	retract(tmp_session(ProfileID, SessionID, _)), !.
impl_profile_remove_session(ProfileID, SessionID) :-
	impl_profile_prolog_session:
	    retractall_session(tmp_session(ProfileID, SessionID, _)).

%%	impl_profile_session(?ProfileID, ?SessionID) is nondet.

impl_profile_session(ProfileID, SessionID) :-
	var(ProfileID), var(SessionID), !,
	current_session(SessionID),
	impl_profile_session(ProfileID, SessionID).
impl_profile_session(ProfileID, SessionID) :-
	tmp_session(ProfileID, SessionID, TimeOut), !,
	session_last_usage(SessionID, LastUsage),
	get_time(Now),
	(   LastUsage+TimeOut < Now
	->  true
	;   retractall(tmp_session(ProfileID, SessionID, _)),
	    retractall(session_last_usage(SessionID, _)),
	    fail
	).
impl_profile_session(ProfileID, SessionID) :-
	get_time(Now),
	impl_profile_prolog_session:
	    tmp_session(ProfileID, SessionID, Deadline),
	(   Deadline < Now
	->  true
	;   impl_profile_prolog_session:
		retractall_session(tmp_session(ProfileID, SessionID, _)),
	    fail
	).

current_session(SessionID) :-
	tmp_session(_, SessionID, _).
current_session(SessionID) :-
	impl_profile_prolog_session:
	    tmp_session(_, SessionID, _).
