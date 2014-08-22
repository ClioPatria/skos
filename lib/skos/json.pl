:- module(skos_json, [
	      json_all_literal_propvalues/3]).

:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).

:- rdf_meta
	json_all_literal_propvalues(r, r, -).

%%	json_all_literal_propvalues(URI, Property, Dict) is det.
%
%	Dict is a dictionairy with language codes as keys an literal
%	values for data property Property as values
json_all_literal_propvalues(R,P,Definitions) :-
	findall(Lang-Definition,
		(   rdf_has(R, P, DefLit),
		    rdf_is_literal(DefLit),
		    literal_text(DefLit,Definition),
		    (	DefLit = literal(lang(Lang, _))
		    ->	true
		    ;	Lang=lang_undefined
		    )
		), Pairs),
	keysort(Pairs, Sorted),
	group_pairs_by_key(Sorted, Grouped),
	dict_pairs(Definitions, lang,  Grouped).
