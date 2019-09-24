:- module(gemeentegeschiedenis, [run/0]).

/** <module> Gemeentegeschiedenis ETL

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(apply)).
:- use_module(library(settings)).

:- use_module(library(atom_ext)).
:- use_module(library(file_ext)).
:- use_module(library(http/http_client2)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_clean)).
:- use_module(library(semweb/rdf_deref)).
:- use_module(library(semweb/rdf_export)).
:- use_module(library(semweb/rdf_mem)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_term)).

:- curl.

:- maplist(rdf_register_prefix, [
     graph-'https://www.gemeentegeschiedenis.nl/graph/',
     resource-'https://www.gemeentegeschiedenis.nl/',
     skos
   ]).

:- set_setting(rdf_term:bnode_prefix_authority, 'www.gemeentegeschiedenis.nl').
:- set_setting(rdf_term:bnode_prefix_scheme, https).

:- thread_local
     visited/1.

run :-
  % www → .nq.gz
  rdf_equal(G, graph:data),
  scrape(mem(G)),
  rdf_save_file('data.nq.gz', [graph(G)]).

scrape(B) :-
  retractall(visited(_)),
  maplist(
    deref_province(B),
    [
      'Groningen',
      'Friesland',
      'Drenthe',
      'Overijssel',
      'Flevoland',
      'Gelderland',
      'Utrecht',
      'Noord-Holland',
      'Zuid-Holland',
      'Zeeland',
      'Noord-Brabant',
      'Limburg'
    ]
  ).

deref_province(B, Local) :-
  rdf_prefix_iri(resource, [provincie,Local], S),
  deref_instance(B, S).

deref_instance(_, S) :-
  visited(S), !.
deref_instance(B, S) :-
  assert(visited(S)),
  rdf_deref_uri(
    S,
    deref_triples(B),
    [media_type(media(application/'rdf+xml',[]))]
  ).

deref_triples(B, _, Triples, _) :-
  maplist(deref_triple(B), Triples).

deref_triple(B, Triple) :-
  rdf_clean_triple('https://www.gemeentegeschiedenis.nl', Triple, tp(S,P,O)),
  deref_triple(B, S, P, O).

% Assert + dereference
deref_triple(B, S, P, O) :-
  rdf_prefix_memberchk(P, [skos:broader,skos:narrower]), !,
  assert_triple(B, S, P, O),
  deref_instance(B, O).
% Assert
deref_triple(B, S, P, O0) :-
  (   O0 = literal(type(D,Lex0))
  ->  atom_strip(Lex0, Lex),
      O = literal(type(D,Lex))
  ;   O0 = literal(lang(LTag,Lex0))
  ->  atom_strip(Lex0, Lex),
      O = literal(lang(LTag,Lex))
  ;   O = O0
  ),
  assert_triple(B, S, P, O).

/*
upload(DataFile) :-
  rdf_bnode_prefix(BNodePrefix),
  Properties = _{
    accessLevel: public,
    avatar: 'avatar.png',
    description: "Nederland telt bijna 400 gemeenten.  In 1812 waren dat er meer dan 1100.  In twee eeuwen vol herindelingen, annexaties en fusies heeft Nederland bijna 1700 gemeenten gekend.  De data van gemeentegeschiedenis is gebouwd op twee datasets: het Repertorium van Nederlandse gemeenten vanaf 1812 van Ad van der Meer en Onno Boonstra en de bij DANS gedeponeerde NLGis shapefiles van Onno Boonstra, waarin van elk jaar van 1812 tot 1997 de gemeentegrenzen zijn vastgelegd.",
    exampleResources: [
      municipality-'Almenum',
      municipality-'Amsterdam',
      municipality-'Roermond',
      municipality-'Swalmen',
      municipality-'Lelystad',
      municipality-'Middelstum'
    ],
    files: [DataFile,'meta.trig'],
    prefixes: [
      bnode-BNodePrefix,
      amco-'http://www.gemeentegeschiedenis.nl/amco/',
      cbs-'http://www.gemeentegeschiedenis.nl/cbscode/',
      departement-'http://www.gemeentegeschiedenis.nl/departement/',
      'GeoNames'-'http://sws.geonames.org/',
      graph,
      municipality-'http://www.gemeentegeschiedenis.nl/gemeentenaam/',
      'nl.dbr'-'http://nl.dbpedia.org/resource/',
      'nl.wiki'-'https://nl.wikipedia.org/wiki/',
      province-'http://www.gemeentegeschiedenis.nl/provincie/',
      vocab-'http://www.gemeentegeschiedenis.nl/gg-schema#'
    ]
  },
  dataset_upload(druid, nlgis, gemeentegeschiedenis, Properties),
  delete_file(DataFile).
*/
