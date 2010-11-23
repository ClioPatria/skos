:- module(conf_skos, []).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_library)).

/** <module> Provide SKOS schema and namespace

This module provides the SKOS schema and   the  prefix =skos= for use in
Prolog.
*/

:- rdf_register_ns(skos, 'http://www.w3.org/2004/02/skos/core#').
:- rdf_attach_library(skos(rdf)).
:- rdf_load_library(skos).
