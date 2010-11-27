:- module(skos,
	  [ skos_display_link//2,	% +URI, +Options
	    skos_concept_view//2		% +URI, +Options
	  ]).

:- use_module(library(http/page_info)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).

:- use_module(library(sgml)).

:- use_module(components(label)).
:- use_module(user(preferences)).
:- use_module(cliopatria(hooks)).

/** <module> Domain-specific components for SKOS models
*/

%%	skos_display_link(+URI, +Options)// is semidet.
%

skos_display_link(SKOSXL, Options) -->
	{ \+ memberchk(skos(false), Options),
	   rdfs_individual_of(SKOSXL, 'http://www.w3.org/2008/05/skos-xl#Label'),
	   rdf_has(SKOSXL, 'http://www.w3.org/2008/05/skos-xl#literalForm', Literal),
	   resource_link(SKOSXL, HREF)
	},
        html(a([class(r_def), href(HREF)], ['xl: ', \turtle_label(Literal)])).

%%	edm_proxy_view(+URI, +Options)// is det.
%
%	Provide a _|local view|_ for an EDM ore:Proxy object. The caller
%	must ensure that URI is indeed of type ore:Proxy.

skos_concept_view(URI, _Options) -->
	{ fail, type_styles(URI, Styles),
	  http_current_request(Request),
	  http_reload_with_parameters(Request, [raw(true)], FullHREF)
	},
	html_requires(css('edm.css')),
	html(div(class(Styles),
		 [ \values(div, URI, dcterms:title),
		   \values(div, URI, ore:proxyIn -> ens:hasThumbnail),
		   \values(div, URI, dcterms:creator),
		   div(class(created), \values(span, URI, dcterms:created)),
		   div(class(extent), \values(span, URI, dcterms:extent)),
		   \values(div, URI, dcterms:description),
		   div(class(owner),
		       [ \values(span, URI, dcterms:rights),
			 \values(span, URI, dcterms:identifier)
		       ]),
		   br(clear(all)),
		   div(class(fullview), a(href(FullHREF), 'Full view'))
		 ])).

type_styles(URI, Styles) :-
	findall(Style, type_style(URI, Style), Styles).

type_style(URI, Class) :-
	rdf_has(URI, rdf:type, Type),
	uri_css_class(Type, Class).

values(Element, URI, Path) -->
	{ has_values(URI, Path, Pairs)
	},
	values(Pairs, Element).

values([], _) --> [].
values([V-Classes|T], Element) -->
	{ HTML =.. [Element, class(Classes), \value(V)] },
	html(HTML),
	values(T, Element).


%%	has_values(+URI, +Path, -Pairs) is det.
%
%	Pairs is a list of Value-Classes pairs.

has_values(URI, Path, Pairs) :-
	findall(Value-Classes, has_value(Path, URI, Classes, Value), Pairs0),
	partition(pair_preferred_lang, Pairs0, Preferred, NonPreferred),
	(   Preferred == []
	->  Pairs = NonPreferred
	;   Pairs = Preferred
	).

pair_preferred_lang(Value-_CSS) :-
	preferred_lang(Value).

has_value(P0->P, URI, Classes, Value) :- !,
	has_value(P0, URI, Classes0, Value0),
	has_value(P, Value0, Classes1, Value),
	append(Classes0, Classes1, Classes).
has_value(NS:Local, URI, Class, Value) :- !,
	rdf_global_id(NS:Local, P),
	has_value(P, URI, Class, Value).
has_value(P, URI, Classes, Value) :-
	rdf_has(URI, P, Value, RP),
	p_classes(RP, P, Classes).

%%	p_classes(+FoundPred, +QueryPred, -CSSClasses) is det.
%
%	@tbd	Find intermediate properties

p_classes(RP, RP, [RC]) :- !,
	uri_css_class(RP, RC).
p_classes(RP, P, [RC, PC]) :-
	uri_css_class(P, PC),
	uri_css_class(RP, RC).

uri_css_class(URI, Class) :-
	iri_xml_namespace(URI, _, Class).


%%	value(+Value)// is det.
%
%	Show the actual value

value(Literal) -->
	{ rdf_is_literal(Literal), !,
	  literal_text(Literal, Text)
	},
	html(Text).
value(R) -->
	rdf_link(R).

%%	preferred_lang(+Object)
%
%	True if Object is stated in the   preferred language of the user
%	or language-neutral.

preferred_lang(Literal) :-
	literal_lang(Literal, Lang), !,
	user_preference(user:lang, literal(PrefLang)),
	lang_matches(PrefLang, Lang).
preferred_lang(R) :-			% see bnode_label//1.
	rdf_label(R, Value),
	literal_lang(Value, Lang), !,
	user_preference(user:lang, literal(PrefLang)),
	lang_matches(PrefLang, Lang).
preferred_lang(BNode) :-		% see bnode_label//1.
	rdf_is_bnode(BNode),
	rdf_has(BNode, rdf:value, Value),
	literal_lang(Value, Lang), !,
	user_preference(user:lang, literal(PrefLang)),
	lang_matches(PrefLang, Lang).
preferred_lang(_).

literal_lang(literal(lang(Lang, _)), Lang).

rdf_label:display_label_hook(SKOS, Lang, Literal) :-
        ( ground(Lang) ; Lang = en),
        rdfs_individual_of(SKOS, skos:'Concept'),
        rdf_has(SKOS, 'http://www.w3.org/2008/05/skos-xl#prefLabel', SKOSXL),
        rdf_has(SKOSXL, 'http://www.w3.org/2008/05/skos-xl#literalForm', literal(lang(Lang,Literal))).

