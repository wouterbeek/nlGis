:- module(gemeentegeschiedenis, [run/0]).

/** <module> Gemeentegeschiedenis

@author Wouter Beek
@version 2017/04-2017/12
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_export)).
:- use_module(library(tapir)).
:- use_module(library(zlib)).

:- thread_local
     visited/1.

:- maplist(rdf_create_prefix, [
     bnode-'https://iisg.amsterdam/.well-known/genid/',
     graph-'http://www.gemeentegeschiedenis.nl/graph/',
     resource-'http://www.gemeentegeschiedenis.nl/'
   ]).

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
    rdf_save2(Out),
    close(Out)
  ),

  % upload to Triply
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
      bnode,
      amco-'http://www.gemeentegeschiedenis.nl/amco/',
      cbs-'http://www.gemeentegeschiedenis.nl/cbscode/',
      departement-'http://www.gemeentegeschiedenis.nl/departement/',
      'GeoNames'-'http://sws.geonames.org/',
      graph,
      municipality-'http://www.gemeentegeschiedenis.nl/gemeentenaam/',
      'nl.dbr',
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
  rdf_prefix_iri(bnode, BNodePrefix),
  rdf_deref_uri(
    S,
    deref_triples,
    [bnode_prefix(BNodePrefix),format(media(application/'rdf+xml',[]))]
  ).

deref_triples(Triples, _) :-
  maplist(deref_triple, Triples).

deref_triple(Triple) :-
  rdf_clean_triple(Triple, rdf(S,P,O)),
  deref_triple(S, P, O).

% Assert + dereference
deref_triple(S, P, O) :-
  rdf_prefix_memberchk(P, [skos:broader,skos:narrower]), !,
  rdf_assert(S, P, O, graph:data),
  deref_instance(O).
% Assert
deref_triple(S, P, O0) :-
  (   O0 = literal(type(D,Lex0))
  ->  atom_strip(Lex0, Lex),
      O = literal(type(D,Lex))
  ;   O = O0
  ),
  rdf_assert(S, P, O, graph:data).
