:- use_module(library(settings)).
:- use_module(library(user_profile)).
:- use_module(library(profile/backend/profile_prolog)).

:- set_setting(user_profile:backend, impl_profile_prolog).

user_profile:attribute_type(name, string).
user_profile:attribute_type(email, string).

:- initialization
	profile_open_db([]).
