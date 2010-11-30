:- module(skos_xl_schema, []).
:- use_module(library(skos_schema)).
:- use_module(library(semweb/rdf_library)).
:- use_module(library(semweb/rdf_db)).

/** <module> Provide SKOS schema, namespace and visualization hooks.

This module provides the SKOS schema and   the  prefix =skos= for use in
Prolog.
*/

:- rdf_register_ns(skosxl, 'http://www.w3.org/2008/05/skos-xl#').

:- rdf_attach_library(skos(rdf)).
:- rdf_load_library(skosxl).
