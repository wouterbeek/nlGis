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
  scrape,
  DataFile = 'data.nq.gz',
  write_to_file(DataFile, rdf_write_quads),
  % Upload to Druid.
  upload(DataFile).



%! scrape is det.

scrape :-
  retractall(visited(_)),
  maplist(
    deref_province,
    [
      'Groningen', 'Friesland', 'Drenthe', 'Overijssel', 'Flevoland',
      'Gelderland', 'Utrecht', 'Noord-Holland', 'Zuid-Holland', 'Zeeland',
      'Noord-Brabant', 'Limburg'
    ]
  ).

deref_province(Local) :-
  rdf_create_iri(resource, [provincie,Local], Province),
  deref_instance(Province).

deref_instance(S) :-
  visited(S), !.
deref_instance(S) :-
  assert(visited(S)),
  rdf_deref_uri(S, deref_triples, [media_type(media(application/'rdf+xml',[]))]).

deref_triples(_, Triples, _) :-
  maplist(deref_triple, Triples).

deref_triple(Triple) :-
  rdf_bnode_prefix(BNodePrefix),
  rdf_clean_triple(BNodePrefix, Triple, rdf(S,P,O)),
  deref_triple(S, P, O).

% Assert + dereference
deref_triple(S, P, O) :-
  rdf_prefix_memberchk(P, [skos:broader,skos:narrower]), !,
  rdf_assert_triple(S, P, O, graph:data),
  deref_instance(O).
% Assert
deref_triple(S, P, O0) :-
  (   O0 = literal(type(D,Lex0))
  ->  atom_strip(Lex0, Lex),
      O = literal(type(D,Lex))
  ;   O0 = literal(lang(LTag,Lex0))
  ->  atom_strip(Lex0, Lex),
      O = literal(lang(LTag,Lex))
  ;   O = O0
  ),
  rdf_assert_triple(S, P, O, graph:data).



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
  delete_file('data.nq.gz').
