:- module(mint, [run/0]).

/** <module> Mint Authorities

@author Wouter Beek
@see https://datasets.socialhistory.org/dataverse/lowcountries_GIS
@version 2017-2018
*/

:- use_module(library(apply)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(settings)).
:- use_module(library(thread)).
:- use_module(library(yall)).
:- use_module(library(zlib)).

:- use_module(library(dcg)).
:- use_module(library(dict)).
:- use_module(library(graph/gv)).
:- use_module(library(sw/rdf_export)).
:- use_module(library(sw/rdf_mem)).
:- use_module(library(sw/rdf_mem_geo)).
:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_term)).
:- use_module(library(sw/shacl)).
:- use_module(library(tapir)).

:- rdf_meta
   authority(+, r, +),
   house(+, r, r, +).

:- maplist(rdf_assert_prefix, [
     graph-'https://iisg.amsterdam/graph/mint/',
     resource-'https://iisg.amsterdam/resource/',
     vocab-'https://iisg.amsterdam/vocab/'
   ]).

:- set_setting(rdf_term:bnode_prefix_authority, 'iisg.amsterdam').
:- set_setting(rdf_term:bnode_prefix_scheme, https).

run :-
  Doc = 'https://datasets.socialhistory.org/dataverse/lowcountries_GIS',
  
  % convert authorities
  file_features('authorities.geojson.gz', Features1),
  maplist(authority(Doc, graph:authorities), Features1),

  % Convert houses: this _must_ be done after converting the
  % authorities.
  file_features('houses.geojson.gz', Features2),
  maplist(house(Doc, graph:authorities, graph:houses), Features2),

  % .geojson → .nq.gz
  setup_call_cleanup(
    gzopen('data.nq.gz', write, Out),
    forall(
      rdf_triple(S, P, O, G),
      rdf_write_quad(Out, S, P, O, G)
    ),
    close(Out)
  ),

  % .trig.gz → .svg
  rdf_equal(graph:vocab, G),
  setup_call_cleanup(
    rdf_load_file('vocab.trig.gz', [graph(DefG)]),
    gv_export(dot, svg, 'vocab.svg', {G}/[Out]>>shacl_export(Out, G)),
    maplist(rdf_retract_graph, [DefG,G])
  ),

  % upload to Triply
  rdf_bnode_iri(BNodePrefix),
  Properties = _{
    accessLevel: public,
    avatar: 'avatar.jpg',
    binary_files: ['mint.mp4','vocab.svg'],
    description: "Polygons of the major coin issueing authorities that existed in the Low Countries between the 6th and the 21st centuries.  This dataset also includes points for the mint houses responsible for the production of coins.",
    exampleResources: [authority-'Mechelen',house-'Maaseik'],
    files: ['data.nq.gz','meta.trig.gz','vocab.trig.gz'],
    prefixes: [
      bnode-BNodePrefix,
      authority-'https://iisg.amsterdam/resource/authority/',
      dataset-'https://iisg.amsterdam/dataset/',
      graph,
      house-'https://iisg.amsterdam/resource/house/',
      sh,
      vocab
    ]
  },
  dataset_upload(mint, Properties),

  % cleanup of temporary files
  concurrent_maplist(delete_file, ['data.nq.gz']),
  halt.

file_features(File, Features) :-
  setup_call_cleanup(
    gzopen(File, read, In),
    json_read_dict(In, Dict, [value_string_as(atom)]),
    close(In)
  ),
  _{features: Features} :< Dict.

authority(Doc, G1, Feature) :-
  _{properties: Properties} :< Feature,
  % vocab:Authority
  rdf_create_iri(resource, [authority,Properties.'AUTHORITY'], Authority),
  rdf_assert_triple(Authority, rdf:type, vocab:'Authority', G1),
  % vocab:Authority rdfs:label rdf:langString
  rdf_assert_triple(Authority, rdfs:label, Properties.'AUTHORITY', G1),
  % geo:Geometry
  % vocab:Authority geo:hasGeometry geo:Geometry
  _{geometry: Geometry1} :< Feature,
  (   Geometry1 == null
  ->  rdf_bnode_iri(Doc, _, Geometry2),
      rdf_assert_triple(Authority, geo:hasGeometry, Geometry2, G1)
  ;   _{coordinates: Coords, type: Type} :< Geometry1,
      Shape =.. [Type,Coords],
      % geo:Geometry geo:asWKT geo:wktLiteral
      rdf_assert_wkt(Authority, Shape, G1, Geometry2)
  ),
  % geo:Geometry vocab:begin xsd:gYear
  date(Properties.'DATEfrom', Begin),
  rdf_assert_triple(Geometry2, vocab:begin, Begin, G1),
  % geo:Geometry vocab:end xsd:gYear
  date(Properties.'DATEto', End),
  rdf_assert_triple(Geometry2, vocab:end, End, G1).

house(Doc, G1, G2, Feature) :-
  _{properties: Properties} :< Feature,
  % vocab:House
  rdf_create_iri(resource, [house,Properties.'MINT'], House),
  rdf_assert_triple(House, rdf:type, vocab:'House', G2),
  % vocab:House rdfs:label rdf:langString
  rdf_assert_triple(House, rdfs:label, Properties.'MINT', G2),
  % vocab:House vocab:hasAuthority vocab:Authority
  rdf_create_iri(resource, [authority,Properties.'AUTHORITY'], Authority),
  rdf_assert_triple(House, vocab:hasAuthority, Authority, G2),
  % Some authorities only appear in the source file for houses.
  (   rdf_triple(Authority, rdf:type, vocab:'Authority', G1)
  ->  true
  ;   rdf_assert_triple(Authority, rdf:type, vocab:'Authority', G1),
      rdf_assert_triple(Authority, rdfs:label, Properties.'AUTHORITY', G1)
  ),
  % vocab:House vocab:alloy vocab:Alloy
  (   Properties.'ALLOY' == null
  ->  true
  ;   rdf_create_iri(vocab, [Properties.'ALLOY'], Alloy),
      rdf_assert_triple(House, vocab:alloy, Alloy, G2)
  ),
  % vocab:House vocab:source xsd:string
  split_string(Properties.'SOURCE', ";", " ", Sources),
  forall(
    member(Source, Sources),
    rdf_assert_triple(House, vocab:source, Source, G2)
  ),
  % geo:Geometry
  % vocab:House geo:hasGeometry geo:Geometry
  _{geometry: Geometry1} :< Feature,
  (   Geometry1 == null
  ->  rdf_bnode_iri(Doc, _, Geometry2),
      rdf_assert_triple(House, geo:hasGeometry, Geometry2, G2)
  ;   _{coordinates:Coords, type:Type} :< Geometry1,
      Shape =.. [Type,Coords],
      % geo:Geometry geo:asWKT geo:wktLiteral
      rdf_assert_wkt(House, Shape, G2, Geometry2)
  ),
  % geo:Geometry vocab:range xsd:gYear
  date(Properties.'DATEfrom', Begin),
  rdf_assert_triple(Geometry2, vocab:begin, Begin, G2),
  % geo:Geometry vocab:end xsd:gYear
  date(Properties.'DATEto', End),
  rdf_assert_triple(Geometry2, vocab:end, End, G2).

date(date(Y,M,D)) -->
  integer(Y),
  "/",
  integer(M),
  "/",
  integer(D).

date(Atom, O) :-
  once(atom_phrase(date(Date), Atom)),
  Date = date(Y,M,D),
  (   ((M =:= 1, D =:= 1) ; (M =:= 12, D =:=31))
  ->  atom_number(Lex, Y),
      rdf_typed_literal(xsd:gYear, Lex, O)
  ;   O = Date
  ).
