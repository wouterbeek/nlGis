:- module(gemeentegeschiedenis, [run/0]).

/** <module> Gemeentegeschiedenis

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(apply)).
:- use_module(library(settings)).
:- use_module(library(zlib)).

:- use_module(library(atom_ext)).
:- use_module(library(sw/rdf_clean)).
:- use_module(library(sw/rdf_deref)).
:- use_module(library(sw/rdf_export)).
:- use_module(library(sw/rdf_mem)).
:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_term)).
:- use_module(library(tapir)).

:- thread_local
     visited/1.

:- maplist(rdf_assert_prefix, [
     graph-'http://www.gemeentegeschiedenis.nl/graph/',
     resource-'http://www.gemeentegeschiedenis.nl/',
     skos
   ]).

:- set_setting(rdf_term:bnode_prefix_authority, 'www.gemeentegeschiedenis.nl').
:- set_setting(rdf_term:bnode_prefix_scheme, https).

run :-
  % www → .nq.gz
  retractall(visited(_)),
  maplist(
    deref_province,
    [
      'Groningen', 'Friesland', 'Drenthe', 'Overijssel', 'Flevoland',
      'Gelderland', 'Utrecht', 'Noord-Holland', 'Zuid-Holland', 'Zeeland',
      'Noord-Brabant', 'Limburg'
    ]
  ),
  setup_call_cleanup(
    gzopen('data.nq.gz', write, Out),
    forall(
      rdf_triple(S, P, O, G),
      rdf_write_quad(Out, S, P, O, G)
    ),
    close(Out)
  ),

  % upload to Triply
  rdf_bnode_iri(BNodePrefix),
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
    files: ['data.nq.gz','meta.trig.gz'],
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
  dataset_upload(gemeentegeschiedenis, Properties),

  % cleanup temporary files
  delete_file('data.nq.gz'),
  halt.

deref_province(Local) :-
  rdf_create_iri(resource, [provincie,Local], Province),
  deref_instance(Province).

deref_instance(S) :-
  visited(S), !.
deref_instance(S) :-
  assert(visited(S)),
  rdf_deref_uri(S, deref_triples, [media_type(media(application/'rdf+xml',[]))]).

deref_triples(BNodePrefix, Triples, _) :-
  maplist(deref_triple(BNodePrefix), Triples).

deref_triple(BNodePrefix, Triple) :-
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
