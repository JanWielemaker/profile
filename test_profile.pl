:- use_module(library(settings)).
:- use_module(user_profile).
:- use_module(impl_profile_prolog).

:- set_setting(user_profile:backend, impl_profile_prolog).

user_profile:attribute_type(name, string).

:- initialization
	profile_open_db([]).