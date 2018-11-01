:- encoding(utf8).
/* Mint Authorities ETL

@author Wouter Beek
@see https://datasets.socialhistory.org/dataverse/lowcountries_GIS
@version 2017-2018
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(settings)).
:- use_module(library(thread)).
:- use_module(library(yall)).

:- use_module(library(dcg)).
:- use_module(library(file_ext)).
:- use_module(library(gis/geojson)).
:- use_module(library(graph/gv)).
:- use_module(library(http/http_client2)).
:- use_module(library(json_ext)).
:- use_module(library(semweb/rdf_export)).
:- use_module(library(semweb/rdf_mem)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_term)).
:- use_module(library(semweb/shacl_export)).
:- use_module(library(tapir/tapir_api)).

:- curl.

:- debug(known_issue).

:- rdf_meta
   authority(r, +),
   house(r, r, +).

:- maplist(rdf_register_prefix, [
     iisg-'https://iisg.amsterdam/vocab/',
     graph-'https://iisg.amsterdam/graph/mint/',
     resource-'https://iisg.amsterdam/resource/'
   ]).

:- set_setting(rdf_term:bnode_prefix_authority, 'iisg.amsterdam').
:- set_setting(rdf_term:bnode_prefix_scheme, https).

etl :-
  % convert authorities
  SourceFile1 = 'authorities.geojson.gz',
  geojson_file_features(SourceFile1, Features1),
  rdf_equal(G1, graph:authorities),
  maplist(assert_authority(G1), Features1),

  % Convert houses: this _must_ be done after converting the
  % authorities.
  SourceFile2 = 'houses.geojson.gz',
  geojson_file_features(SourceFile2, Features2),
  rdf_equal(G2, graph:house),
  maplist(assert_house(G1, G2), Features2),

  % .geojson → .nq.gz
  DataFile = 'data.nq.gz',
  write_to_file(DataFile, rdf_write_quads),

  % .trig → .svg
  VocabFile = 'vocab.trig',
  SvgFile = 'vocab.svg',
  export_schema(VocabFile, SvgFile),

  % Upload to Triply.
  upload(SourceFile1, SourceFile2, DataFile, VocabFile, SvgFile).



%! assert_authority(+G:rdf_graph, +Feature:dict) is det.

assert_authority(G1, Feature) :-
  _{properties: Properties} :< Feature,
  % iisg:Authority
  rdf_create_iri(resource, [authority,Properties.'AUTHORITY'], Authority),
  rdf_assert_triple(Authority, rdf:type, iisg:'Authority', G1),
  % iisg:Authority rdfs:label rdf:langString
  rdf_assert_triple(Authority, rdfs:label, string(Properties.'AUTHORITY'), G1),
  % iisg:Authority geo:hasGeometry/geo:asWKT SHAPE
  _{geometry: Dict} :< Feature,
  (   Dict == null
  ->  rdf_bnode_iri(Geometry),
      debug(known_issue, "Authority without geometry: ‘~a’.", [Properties.'AUTHORITY'])
  ;   geojson_shape(Dict, Shape),
      rdf_assert_shape(Authority, Shape, G1, Geometry)
  ),
  % geo:Geometry iisg:begin xsd:gYear
  date(Properties.'DATEfrom', Begin),
  rdf_assert_triple(Geometry, iisg:begin, Begin, G1),
  % geo:Geometry iisg:end xsd:gYear
  date(Properties.'DATEto', End),
  rdf_assert_triple(Geometry, iisg:end, End, G1).



%! assert_house(+AuthorityG:rdf_graph, +HouseG:rdf_graph, +Feature:dict) is det.

assert_house(G1, G2, Feature) :-
  _{properties: Properties} :< Feature,
  % iisg:House
  rdf_create_iri(resource, [house,Properties.'MINT'], House),
  rdf_assert_triple(House, rdf:type, iisg:'House', G2),
  % iisg:House rdfs:label rdf:langString
  rdf_assert_triple(House, rdfs:label, string(Properties.'MINT'), G2),
  % iisg:House iisg:hasAuthority iisg:Authority
  rdf_create_iri(resource, [authority,Properties.'AUTHORITY'], Authority),
  rdf_assert_triple(House, iisg:hasAuthority, Authority, G2),
  % Some authorities only appear in the source file for houses.
  (   rdf_triple(Authority, rdf:type, iisg:'Authority', G1)
  ->  true
  ;   rdf_assert_triple(Authority, rdf:type, iisg:'Authority', G1),
      rdf_assert_triple(Authority, rdfs:label, string(Properties.'AUTHORITY'), G1)
  ),
  % iisg:House iisg:alloy iisg:Alloy
  (   Properties.'ALLOY' == null
  ->  debug(known_issue, "House without alloy: ‘~a’.", [Properties.'MINT'])
  ;   rdf_create_iri(iisg, [Properties.'ALLOY'], Alloy),
      rdf_assert_triple(House, iisg:alloy, Alloy, G2)
  ),
  % iisg:House iisg:source xsd:string
  split_string(Properties.'SOURCE', ";", " ", Sources),
  forall(
    member(Source, Sources),
    rdf_assert_triple(House, iisg:source, Source, G2)
  ),
  % iisg:Authority geo:hasGeometry/geo:asWKT SHAPE
  _{geometry: Dict} :< Feature,
  (   Dict == null
  ->  rdf_bnode_iri(Geometry),
      debug(known_issue, "House without geometry: ‘~a’.", [Properties.'AUTHORITY'])
  ;   geojson_shape(Dict, Shape),
      rdf_assert_shape(House, Shape, G2, Geometry)
  ),
  % geo:Geometry iisg:range xsd:gYear
  date(Properties.'DATEfrom', Begin),
  rdf_assert_triple(Geometry, iisg:begin, Begin, G2),
  % geo:Geometry iisg:end xsd:gYear
  date(Properties.'DATEto', End),
  rdf_assert_triple(Geometry, iisg:end, End, G2).



%! export_schema(+VocabFile:atom, +SvgFile:atom) is det.

export_schema(VocabFile, SvgFile) :-
  rdf_equal(graph:vocab, G),
  setup_call_cleanup(
    rdf_load_file(VocabFile, [graph(DefG)]),
    shacl_export_file(mem(G), SvgFile),
    maplist(rdf_retract_graph, [DefG,G])
  ).



%! upload(SourceFile1:atom, SourceFile2:atom, DataFile:atom, VocabFile:atom, SvgFile:atom) is det.

upload(SourceFile1, SourceFile2, DataFile, VocabFile, SvgFile) :-
  rdf_bnode_prefix(BNodePrefix),
  Properties = _{
    accessLevel: public,
    assets: ['mint.mp4',SourceFile1,SourceFile2,SvgFile],
    avatar: 'avatar.jpg',
    description: "Polygons of the major coin issueing authorities that existed in the Low Countries between the 6th and the 21st centuries.  This dataset also includes points for the mint houses responsible for the production of coins.",
    exampleResources: [authority-'Mechelen',house-'Maaseik'],
    files: ['meta.trig',DataFile,VocabFile],
    prefixes: [
      bnode-BNodePrefix,
      authority-'https://iisg.amsterdam/resource/authority/',
      dataset-'https://iisg.amsterdam/dataset/',
      graph,
      house-'https://iisg.amsterdam/resource/house/',
      iisg,
      sh
    ]
  },
  dataset_upload(druid, nlgis, mint, Properties),
  concurrent_maplist(delete_file, [DataFile,VocabFile,SvgFile]).





% GENERICS %

%! date(+Atom:atom, -Date:compound) is det.

date(Atom, Value) :-
  once(atom_phrase(date(Date), Atom)),
  Date = date(Y,M,D),
  (((M =:= 1, D =:= 1) ; (M =:= 12, D =:=31)) -> Value = year(Y) ; Value = Date).

date(date(Y,M,D)) -->
  integer(Y),
  "/",
  integer(M),
  "/",
  integer(D).



%! geojson_file_features(+File:atom, -Features:dict) is det.

geojson_file_features(File, Features) :-
  json_load(File, Dict),
  _{features: Features} :< Dict.
