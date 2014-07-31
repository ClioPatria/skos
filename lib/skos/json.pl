:- module(skos_json, [
	      json_all_literal_propvalues/3]).

:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdf_label')).

:- rdf_meta
	json_all_literal_propvalues(r, r, -).

%%	json_all_literal_propvalues(URI, Property, Dict) is det.
%
%	Dict is a dictionairy with language codes as keys an literal
%	values for data property Property as values
json_all_literal_propvalues(R,P,Definitions) :-
	findall(Lang-Definition,
		(   rdf_has(R, P, DefLit),
		    literal_text(DefLit,Definition),
		    (	DefLit = literal(lang(Lang, _))
		    ->	true
		    ;	Lang=lang_undefined
		    )
		), Pairs),
	dict_pairs(Definitions, lang, Pairs).
