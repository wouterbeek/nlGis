/* Gemeentegeschiedenis scrape

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(apply)).
:- use_module(library(settings)).

:- use_module(library(atom_ext)).
:- use_module(library(file_ext)).
:- use_module(library(http/http_client2)).
:- use_module(library(semweb/rdf_clean)).
:- use_module(library(semweb/rdf_deref)).
:- use_module(library(semweb/rdf_export)).
:- use_module(library(semweb/rdf_mem)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_term)).
:- use_module(library(tapir/tapir_api)).

:- curl.

:- maplist(rdf_register_prefix, [
     graph-'http://www.gemeentegeschiedenis.nl/graph/',
     resource-'http://www.gemeentegeschiedenis.nl/',
     skos
   ]).

:- set_setting(rdf_term:bnode_prefix_authority, 'www.gemeentegeschiedenis.nl').
:- set_setting(rdf_term:bnode_prefix_scheme, https).

:- thread_local
     visited/1.

etl :-
  % www → .nq.gz
  rdf_equal(G, graph:data),
  scrape(G),
  DataFile = 'data.nq.gz',
  rdf_save_file(DataFile, [graph(G)]),
  % Upload to Druid.
  upload(DataFile).



%! scrape(+G:rdf_graph) is det.

scrape(G) :-
  retractall(visited(_)),
  maplist(
    {G}/[Local]>>deref_province(Local, G),
    [
      'Groningen', 'Friesland', 'Drenthe', 'Overijssel', 'Flevoland',
      'Gelderland', 'Utrecht', 'Noord-Holland', 'Zuid-Holland', 'Zeeland',
      'Noord-Brabant', 'Limburg'
    ]
  ).

deref_province(Local, G) :-
  rdf_create_iri(resource, [provincie,Local], Province),
  deref_instance(Province, G).

deref_instance(S, _) :-
  visited(S), !.
deref_instance(S, G) :-
  assert(visited(S)),
  rdf_deref_uri(S, deref_triples(G), [media_type(media(application/'rdf+xml',[]))]).

deref_triples(G, _, Triples, _) :-
  maplist(deref_triple(G), Triples).

deref_triple(G, Triple) :-
  rdf_bnode_prefix(BNodePrefix),
  rdf_clean_triple(BNodePrefix, Triple, rdf(S,P,O)),
  deref_triple(S, P, O, G).

% Assert + dereference
deref_triple(S, P, O, G) :-
  rdf_prefix_memberchk(P, [skos:broader,skos:narrower]), !,
  rdf_assert_triple(S, P, O, G),
  deref_instance(O, G).
% Assert
deref_triple(S, P, O0, G) :-
  (   O0 = literal(type(D,Lex0))
  ->  atom_strip(Lex0, Lex),
      O = literal(type(D,Lex))
  ;   O0 = literal(lang(LTag,Lex0))
  ->  atom_strip(Lex0, Lex),
      O = literal(lang(LTag,Lex))
  ;   O = O0
  ),
  rdf_assert_triple(S, P, O, G).



%! upload(+DataFile:atom) is det.

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
