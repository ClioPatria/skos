:- module(conf_skos, []).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_library)).
:- use_module(library(semweb/rdfs)).
:- use_module(cliopatria(hooks)).

/** <module> Provide SKOS schema, namespace and visualization hooks.

This module provides the SKOS schema and   the  prefix =skos= for use in
Prolog.
*/

:- rdf_register_ns(skos,   'http://www.w3.org/2004/02/skos/core#').
:- rdf_register_ns(skosxl, 'http://www.w3.org/2008/05/skos-xl#').

:- rdf_attach_library(skos(rdf)).
:- rdf_attach_library(cliopatria(rdf/base)).

:- rdf_load_library(skos).
:- rdf_load_library(skosxl).
:- rdf_load_library(owl).
:- rdf_load_library(dcterms).

:- use_module(components(skos/components)).
:- use_module(components(skos/graph)).

cliopatria:display_link(R, Options) -->
	skos_display_link(R, Options).

cliopatria:context_graph(R, RDF, Options) :-
	skos_context_graph(R, RDF, Options).

cliopatria:node_shape(URI, Shape, Options) :-
	skos_node_shape(URI, Shape, Options).

cliopatria:list_resource(SKOS) -->
	{ rdfs_individual_of(SKOS, skos:'Concept') },
	skos_concept_view(SKOS, []).
