/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, CWI Amsterdam
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

:- module(user_profile,
	  [ profile_open_db/1,		% +Options

	    profile_create/2,		% ?ProfileId, +Attributes
	    current_profile/1,		% ?ProfileId
	    current_profile/2,		% ?ProfileId, -Attributes
	    profile_property/2,		% ?ProfileId, ?Attribute
	    set_profile/2,		% +ProfileId, +Property
	    profile_remove/2,		% +ProfileId, +Property
	    profile_remove/1,		% +ProfileId

	    profile_add_session/3,	% +ProfileId, +SessionID, +Options
	    profile_remove_session/2,	% +ProfileId, +SessionID
	    profile_session/2,		% ?ProfileId, ?SessionID
	    profile_refresh_session/2	% +ProfileId, +SessionID
	  ]).
:- use_module(library(uuid)).
:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(settings)).

/** <module> User Profile Management

This module implements  user  profile   management,  in  particular  for
managing authentication and authorization for   HTTP  servers. It mainly
defines the interface that can be used within an HTTP application.

The  actual  storage  is  left  to    a  plugin  providing  the  backend
implementation. Backend choices may  depend   on  integration needs with
other services, scale of the site  (number of users), distribution, ease
of installation.

The typical setup sequence is

```
:- use_module(library(http/user_profile)).
:- use_module(library(http/impl/profile_prolog)).
:- set_setting(user_profile:backend, impl_profile_prolog).

:- multifile
	user_profile:attribute_type/2.

user_profile:attribute_type(name, string).
...

```
*/

:- multifile
	attribute_type/2.			% ?Attribute, ?Type

:- setting(backend, atom, user_profile_prolog,
	   "Backend to use (name of the module").
:- setting(session_timeout, number, 900,
	   "Default timeout for session based logins").
:- setting(session_persistency, boolean, false,
	   "Default session persistency handling").


		 /*******************************
		 *	      DATABASE		*
		 *******************************/

%%	profile_open_db(+Options) is det.
%
%	Open the profile database. Must  be   called  before  any of the
%	other  profile  API  predicates.  Options  depend  on  the  used
%	backend.

profile_open_db(Options) :-
	setting(backend, Backend),
	Backend:impl_profile_open_db(Options).


		 /*******************************
		 *	       CREATE		*
		 *******************************/

%%	profile_create(?ProfileID, +Attributes) is det.
%
%	Create a new user profile with the given initial attributes.
%
%	@arg	Attributes is a list of Name(Value) terms.

profile_create(ProfileID, Attributes) :-
	instantiate_profile_id(ProfileID),
	maplist(typecheck_attribute, Attributes, CanAttributes),
	(   current_profile(ProfileID)
	->  permission_error(redefine, user_profile, ProfileID)
	;   true
	),
	setting(backend, Backend),
	Backend:impl_profile_create(ProfileID, CanAttributes).

instantiate_profile_id(ProfileID) :-
	var(ProfileID), !,
	uuid(ProfileID).
instantiate_profile_id(ProfileID) :-
	must_be(atom, ProfileID).

typecheck_attribute(Term, Canonical) :-
	attribute_nv(Term, Name, Value0),
	(   attribute_type(Name, Type)
	->  (   convert_attribute_value(Type, Value0, Value)
	    ->	true
	    ;	must_be(Type, Value)
	    ),
	    Canonical =.. [Name,Value]
	;   existence_error(prolog_attribute_declaration, Name)
	).

convert_attribute_value(string, Atom, String) :-
	atom(Atom),
	atom_string(Atom, String).
convert_attribute_value(float, Int, Float) :-
	integer(Int),
	Float is float(Int).

attribute_nv(Term, _Name, _Value) :-
	var(Term), !,
	instantiation_error(Term).
attribute_nv(Term, Name, Value) :-
	compound(Term),
	compound_name_arguments(Term, Name, [Value]), !.
attribute_nv(Name = Value, Name, Value) :-
	must_be(atom, Name).
attribute_nv(Term, _Name, _Value) :-
	type_error(name_value, Term).


		 /*******************************
		 *	       QUERY		*
		 *******************************/

%%	current_profile(?ProfileID) is nondet.
%
%	True when ProfileID is a currently known user profile.

current_profile(ProfileID) :-
	setting(backend, Backend),
	Backend:impl_current_profile(ProfileID).

%%	current_profile(?ProfileID, -Attributes:dict) is nondet.
%
%	True when ProfileID is a currently   known user profile with the
%	given attributes.

current_profile(ProfileID, Attributes) :-
	setting(backend, Backend),
	Backend:impl_current_profile(ProfileID, Attributes).

%%	current_profile(?ProfileID, -Attributes:dict) is nondet.
%
%	True when ProfileID is a currently   known user profile with the
%	given attributes.

profile_property(ProfileID, Property) :-
	setting(backend, Backend),
	Backend:impl_profile_property(ProfileID, Attributes),
	(   compound(Property)
	->  Property =.. [Name,Value],
	    get_dict(Name, Attributes, Value)
	;   get_dict(Name, Attributes, Value),
	    Property =.. [Name,Value]
	).

		 /*******************************
		 *	       UPDATE		*
		 *******************************/

%%	set_profile(+ProfileID, +Attribute) is det.
%
%	Set an attribute of the profile.

set_profile(ProfileID, Attribute) :-
	must_be(atom, ProfileID),
	typecheck_attribute(Attribute, CanAttribute),
	setting(backend, Backend),
	Backend:impl_set_profile(ProfileID, CanAttribute).

%%	profile_remove(+ProfileID) is det.
%
%	Completely destroy a profile.

profile_remove(ProfileID) :-
	must_be(atom, ProfileID),
	setting(backend, Backend),
	Backend:impl_profile_remove(ProfileID).

%%	profile_remove(+ProfileID, +Attribute) is det.
%
%	Remove an attribute from a profile.

profile_remove(ProfileID, Attribute) :-
	must_be(atom, ProfileID),
	must_be(atom, Attribute),
	setting(backend, Backend),
	Backend:impl_profile_remove(ProfileID, Attribute).


		 /*******************************
		 *	SESSION MANAGEMENT	*
		 *******************************/

%%	profile_add_session(+ProfileID, +SessionID, +Options) is det.
%
%	Associate a profile with a session (login). Options defined are:
%
%	  - timeout(+Seconds)
%	  Max idle time for the session.
%	  - persistent(+Boolean)
%	  If `true`, store the session association persistently, such
%	  that a server restart maintains the login.

profile_add_session(ProfileID, SessionID, Options) :-
	must_be(atom, ProfileID),
	must_be(atom, SessionID),
	setting(session_timeout, DefTimeOut),
	setting(session_persistency, DefPresistency),
	option(timeout(TimeOut), Options, DefTimeOut),
	option(persistent(Persistent), Options, DefPresistency),
	local_add_session(ProfileID, SessionID,
			  [ timeout(TimeOut),
			    persistent(Persistent)
			  ]).

%%	profile_refresh_session(+ProfileID, +SessionID) is det.
%
%	Update the last access time for the indicated session.

profile_refresh_session(ProfileID, SessionID) :-
	must_be(atom, ProfileID),
	must_be(atom, SessionID),
	local_refresh_session(ProfileID, SessionID).

%%	profile_remove_session(+ProfileID, +SessionID) is det.
%
%	Remove the association of a profile with a session (logout).

profile_remove_session(ProfileID, SessionID) :-
	must_be(atom, ProfileID),
	must_be(atom, SessionID),
	local_remove_session(ProfileID, SessionID).

%%	profile_session(?ProfileID, ?SessionID) is nondet.
%
%	True when ProfileID is associated (logged in) with SessionID.

profile_session(ProfileID, SessionID) :-
	local_session(ProfileID, SessionID).


		 /*******************************
		 *	  LOCAL SESSIONS	*
		 *******************************/

:- dynamic
	tmp_session/3,			% ProfileID, SessionID, DeadLine
	session_last_usage/2.		% SessionID, Time
:- volatile
	tmp_session/3,
	session_last_usage/2.

local_add_session(ProfileID, SessionID, Options) :-
	option(persistent(false), Options), !,
	option(timeout(Timeout), Options),
	get_time(Now),
	asserta(tmp_session(ProfileID, SessionID, Timeout)),
	asserta(session_last_usage(SessionID, Now)).
local_add_session(ProfileID, SessionID, Options) :-
	setting(backend, Backend),
	Backend:impl_profile_add_session(ProfileID, SessionID, Options).

local_refresh_session(ProfileID, SessionID) :-
	tmp_session(ProfileID, SessionID, _Timeout), !,
	get_time(Now),
	retractall(session_last_usage(SessionID, _)),
	asserta(session_last_usage(SessionID, Now)).
local_refresh_session(ProfileID, SessionID) :-
	setting(backend, Backend),
	Backend:impl_profile_refresh_session(ProfileID, SessionID).

local_remove_session(ProfileID, SessionID) :-
	retract(tmp_session(ProfileID, SessionID, _)), !.
local_remove_session(ProfileID, SessionID) :-
	setting(backend, Backend),
	Backend:impl_profile_remove_session(ProfileID, SessionID).

local_session(ProfileID, SessionID) :-
	var(ProfileID), var(SessionID), !,
	(   tmp_session(_, SessionID, _),
	    local_session(ProfileID, SessionID)
	;   setting(backend, Backend),
	    Backend:impl_profile_session(ProfileID, SessionID)
	).
local_session(ProfileID, SessionID) :-
	tmp_session(ProfileID, SessionID, TimeOut), !,
	session_last_usage(SessionID, LastUsage),
	get_time(Now),
	(   LastUsage+TimeOut < Now
	->  true
	;   retractall(tmp_session(ProfileID, SessionID, _)),
	    retractall(session_last_usage(SessionID, _)),
	    fail
	).
local_session(ProfileID, SessionID) :-
	setting(backend, Backend),
	Backend:impl_profile_session(ProfileID, SessionID).


		 /*******************************
		 *	      HOOKS		*
		 *******************************/

%%	attribute_type(?Attribute, ?Type) is nondet.
%
%	Multifile hook that defines that the profile attribute Attribute
%	must have the type Type. Type are  types as defined by must_be/2
%	from library(error).
