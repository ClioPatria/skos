:- module(skos_graph,
	  [ skos_context_graph/3,	% +URI, -Graph, +Options
	    skos_node_shape/3		% +URI, -Shape, +Options
	  ]).
:- use_module(cliopatria(hooks)).
:- use_module(library(uri)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_abstract)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(components(label)).
:- use_module(library(settings)).
:- use_module(library(count)).

/** <module> SKOS Context graphs

This module customises context graphs, both in how they are computed and
in the rendering of the SKOS classes.

@see cliopatria(hooks) for a description of the hooks.
*/

% Use SVG context graphs

:- set_setting_default(graphviz:format, svg).

%%	skos_context_graph(+URI, -Graph, +Options)
%
%	Compute the EDM context graph. This is currently defined to do a
%	two-step breadth-first expansion of the graph from URI using the
%	known EDM properties. Branching from a single node is limited to
%	20 and the total graph is not expanded beyond 100 nodes.

:- rdf_meta
	skos_relation(r),
	skos_class(r).

skos_context_graph(R, RDF, Options) :-
	option(style(skos), Options),
	bf_graph(R, 2, 100, 20, RDF0),
	minimise_graph(RDF0, RDF1),		% remove inverse/symmetric/...
	bagify_graph(RDF1, RDF2, Bags, []), 	% Create bags of similar resources
	append(RDF2, Bags, RDF),
	graph_resources(RDF, Resources, _Preds, _Types),
	include(skos_resource, Resources, EDMResources),
	EDMResources = [_,_|_].

%%	bf_graph(+Start, +MaxDist, +MaxEdges, +MaxBranch, -Graph)

bf_graph(Start, MaxDist, MaxEdges, MaxBranch, Graph) :-
	bf_graph_2([0-Start], MaxDist, MaxEdges, MaxBranch, [], Graph).

bf_graph_2([], _, _, _, G, G) :- !.
bf_graph_2([D-_|_], MaxDist, _, _, G, G) :-
	D >= MaxDist, !.
bf_graph_2(AG0, MaxDist, MaxEdges, MaxBranch, G0, G) :-
	bf_expand(AG0, AG, MaxBranch, G1),
	(   G1 == []
	->  bf_graph_2(AG, MaxDist, MaxEdges, MaxBranch, G0, G)
	;   append(G1, G0, G2),
	    sort(G2, G3),
	    length(G3, Edges),
	    (   Edges >= MaxEdges
	    ->  G = G0
	    ;   bf_graph_2(AG, MaxDist, MaxEdges, MaxBranch, G3, G)
	    )
	).

bf_expand([D-F|AG0], AG, MaxBranch, Triples) :-
	D1 is D + 1,
	Key = D1-Dst,
	answer_set(Key-Triple, related(F, Dst, Triple), MaxBranch, Pairs),
	pairs_keys_values(Pairs, Dsts, Triples),
	append(AG0, Dsts, AG).

related(S, O, rdf(S,P,O)) :-
	skos_relation(Rel),
	rdf_has(S, Rel, O, P).
related(O, S, rdf(S,P,O)) :-
	skos_relation(Rel),
	rdf_has(S, Rel, O, P).

skos_relation(skos:semanticRelation).


skos_resource(R) :-
	skos_class(Class),
	rdfs_individual_of(R, Class), !.

skos_class(skos:'Concept').


%%	skos_node_shape(+URI, -Shape, +Options)
%
%	Realise   EDM-specific   vizualisation   of     nodes   in   the
%	context-graph.

skos_node_shape(URI, Shape, Options) :-
	option(style(skos), Options),
	node_shape(URI, Shape, Options).

node_shape(URI, Shape, Options) :-
	memberchk(start(URI), Options),
	Shape = [shape(tripleoctagon),style(filled),fillcolor('#ff85fd')].
