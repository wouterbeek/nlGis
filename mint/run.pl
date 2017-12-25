:- module(mint, [run/0]).

/** <module> Mint Authorities

@author Wouter Beek
@see https://datasets.socialhistory.org/dataset.xhtml?persistentId=hdl:10622/HPIC74
@version 2017/04-2017/12
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(graph/gv)).
:- use_module(library(http/json)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_export)).
:- use_module(library(semweb/rdf_geo)).
:- use_module(library(semweb/shacl)).
:- use_module(library(tapir)).
:- use_module(library(thread)).
:- use_module(library(yall)).
:- use_module(library(zlib)).

:- rdf_meta
   authority(r, +),
   house(r, r, +).

:- maplist(rdf_create_prefix, [
     bnode-'https://iisg.amsterdam/.well-known/genid/',
     resource-'https://iisg.amsterdam/resource/',
     graph-'https://iisg.amsterdam/graph/mint/',
     vocab-'https://iisg.amsterdam/vocab/'
   ]).

run :-
  % convert authorities
  file_features('authorities.geojson.gz', Features1),
  maplist(authority(graph:authorities), Features1),

  % Convert houses: this _must_ be done after converting the
  % authorities.
  file_features('houses.geojson.gz', Features2),
  maplist(house(graph:authorities, graph:houses), Features2),

  % .geojson → .nq.gz
  setup_call_cleanup(
    gzopen('data.nq.gz', write, Out),
    rdf_save2(Out),
    close(Out)
  ),

  % .trig.gz → .svg
  rdf_equal(graph:vocab, G),
  setup_call_cleanup(
    rdf_load2('vocab.trig.gz', [graph(DefG)]),
    gv_export(dot, svg, 'vocab.svg', {G}/[Out]>>shacl_export(Out, G)),
    maplist(rdf_unload_graph, [DefG,G])
  ),

  % upload to Triply
  Properties = _{
    accessLevel: public,
    avatar: 'avatar.jpg',
    binary_files: ['mint.mp4','vocab.svg'],
    description: "Polygons of the major coin issueing authorities that existed in the Low Countries between the 6th and the 21st centuries.  This dataset also includes points for the mint houses responsible for the production of coins.",
    exampleResources: [authority-'Mechelen',house-'Maaseik'],
    files: ['data.nq.gz','meta.trig.gz','vocab.trig.gz'],
    prefixes: [
      bnode,
      authority-'https://iisg.amsterdam/resource/authority/',
      dataset-'https://iisg.amsterdam/dataset/',
      fabio,
      graph,
      house-'https://iisg.amsterdam/resource/house/',
      orcid,
      rel,
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

authority(G1, Feature) :-
  _{properties: Properties} :< Feature,
  % vocab:Authority
  rdf_create_iri(resource, [authority,Properties.'AUTHORITY'], Authority),
  rdf_assert(Authority, rdf:type, vocab:'Authority', G1),
  % vocab:Authority rdfs:label rdf:langString
  rdf_assert(Authority, rdfs:label, Properties.'AUTHORITY'^^xsd:string, G1),
  % geo:Geometry
  % vocab:Authority geo:hasGeometry geo:Geometry
  _{geometry: Geometry1} :< Feature,
  (   Geometry1 == null
  ->  rdf_create_well_known_iri(Geometry2),
      rdf_assert(Authority, geo:hasGeometry, Geometry2, G1)
  ;   _{coordinates: Coords, type: Type} :< Geometry1,
      Shape =.. [Type,Coords],
      % geo:Geometry geo:asWKT geo:wktLiteral
      rdf_assert_wkt(Authority, Shape, G1, Geometry2)
  ),
  % geo:Geometry vocab:begin xsd:gYear
  date(Properties.'DATEfrom', Begin),
  rdf_assert(Geometry2, vocab:begin, Begin, G1),
  % geo:Geometry vocab:end xsd:gYear
  date(Properties.'DATEto', End),
  rdf_assert(Geometry2, vocab:end, End, G1).

house(G1, G2, Feature) :-
  _{properties: Properties} :< Feature,
  % vocab:House
  rdf_create_iri(resource, [house,Properties.'MINT'], House),
  rdf_assert(House, rdf:type, vocab:'House', G2),
  % vocab:House rdfs:label rdf:langString
  rdf_assert(House, rdfs:label, Properties.'MINT'^^xsd:string, G2),
  % vocab:House vocab:hasAuthority vocab:Authority
  rdf_create_iri(resource, [authority,Properties.'AUTHORITY'], Authority),
  rdf_assert(House, vocab:hasAuthority, Authority, G2),
  % Some authorities only appear in the source file for houses.
  (   rdf(Authority, rdf:type, vocab:'Authority', G1)
  ->  true
  ;   rdf_assert(Authority, rdf:type, vocab:'Authority', G1),
      rdf_assert(Authority, rdfs:label, Properties.'AUTHORITY'^^xsd:string, G1)
  ),
  % vocab:House vocab:alloy vocab:Alloy
  (   Properties.'ALLOY' == null
  ->  true
  ;   rdf_create_iri(vocab, [Properties.'ALLOY'], Alloy),
      rdf_assert(House, vocab:alloy, Alloy, G2)
  ),
  % vocab:House vocab:source xsd:string
  split_string(Properties.'SOURCE', ";", " ", Sources),
  forall(
    member(Source, Sources),
    rdf_assert(House, vocab:source, Source^^xsd:string, G2)
  ),
  % geo:Geometry
  % vocab:House geo:hasGeometry geo:Geometry
  _{geometry: Geometry1} :< Feature,
  (   Geometry1 == null
  ->  rdf_create_well_known_iri(Geometry2),
      rdf_assert(House, geo:hasGeometry, Geometry2, G2)
  ;   _{coordinates:Coords, type:Type} :< Geometry1,
      Shape =.. [Type,Coords],
      % geo:Geometry geo:asWKT geo:wktLiteral
      rdf_assert_wkt(House, Shape, G2, Geometry2)
  ),
  % geo:Geometry vocab:range xsd:gYear
  date(Properties.'DATEfrom', Begin),
  rdf_assert(Geometry2, vocab:begin, Begin, G2),
  % geo:Geometry vocab:end xsd:gYear
  date(Properties.'DATEto', End),
  rdf_assert(Geometry2, vocab:end, End, G2).

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
  ->  rdf_equal(Y^^xsd:gYear, O)
  ;   rdf_equal(Date^^xsd:date, O)
  ).
