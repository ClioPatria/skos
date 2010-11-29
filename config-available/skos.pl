:- module(conf_skos, []).

/** <module> Provide SKOS visualization hooks.

This module provides hooks for showing SKOS entities
*/

:- use_module(cliopatria(hooks)).
:- use_module(library(skos_schema)).
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
