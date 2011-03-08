:- module(skos,
	  [
	  ]).

:- use_module(library(semweb/rdf_label)).

/** <module> Domain-specific components for SKOS models
*/

%%	rdf_label:label_hook(+URI, -Literal) is nondet.
%
%	Implement SKOS-XL handling. In SKOS-XL, a   label  is a uri with
%	one or more skosxl:literalForm attributes.

rdf_label:label_hook(SKOS, Literal) :-
        rdf_has(SKOS, skosxl:prefLabel, SKOSXL),
        rdf_has(SKOSXL, skosxl:literalForm, Literal).

