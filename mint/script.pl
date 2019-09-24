:- encoding(utf8).
:- module(script, [etl/0]).

/** <module> Mint Authorities ETL

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
:- use_module(library(semweb/rdf_api)).
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
  rdf_equal(B1, mem(graph:authorities)),
  maplist(assert_authority(B1), Features1),

  % Convert houses: this _must_ be done after converting the
  % authorities.
  SourceFile2 = 'houses.geojson.gz',
  geojson_file_features(SourceFile2, Features2),
  rdf_equal(B2, mem(graph:house)),
  maplist(assert_house(B1, B2), Features2),

  % .geojson → .nq.gz
  DataFile = 'data.nq.gz',
  rdf_save_file(DataFile),

  % .trig → .svg
  VocabFile = 'vocab.trig',
  SvgFile = 'vocab.svg',
  export_schema(VocabFile, SvgFile),

  % Upload to Druid.
  upload(SourceFile1, SourceFile2, DataFile, VocabFile, SvgFile).



%! assert_authority(+Backend, +Feature:dict) is det.

assert_authority(B, Feature) :-
  _{properties: Properties} :< Feature,
  % iisg:Authority
  rdf_create_iri(resource, [authority,Properties.'AUTHORITY'], Authority),
  assert_instance(B, Authority, iisg:'Authority'),
  % iisg:Authority rdfs:label rdf:langString
  assert_triple(B, Authority, rdfs:label, string(Properties.'AUTHORITY')),
  % iisg:Authority geo:hasGeometry/geo:asWKT SHAPE
  _{geometry: Dict} :< Feature,
  (   Dict == null
  ->  well_known_iri(Geometry),
      debug(known_issue, "Authority without geometry: ‘~a’.", [Properties.'AUTHORITY'])
  ;   geojson_shape(Dict, Shape),
      assert_shape(B, Authority, Shape, Geometry)
  ),
  % geo:Geometry iisg:begin xsd:gYear
  date(Properties.'DATEfrom', Begin),
  assert_triple(B, Geometry, iisg:begin, Begin),
  % geo:Geometry iisg:end xsd:gYear
  date(Properties.'DATEto', End),
  assert_triple(B, Geometry, iisg:end, End).



%! assert_house(+AuthorityBackend, +HouseBackend, +Feature:dict) is det.

assert_house(B1, B2, Feature) :-
  _{properties: Properties} :< Feature,
  % iisg:House
  rdf_create_iri(resource, [house,Properties.'MINT'], House),
  assert_triple(B2, House, rdf:type, iisg:'House'),
  % iisg:House rdfs:label rdf:langString
  assert_triple(B2, House, rdfs:label, string(Properties.'MINT')),
  % iisg:House iisg:hasAuthority iisg:Authority
  rdf_create_iri(resource, [authority,Properties.'AUTHORITY'], Authority),
  assert_triple(B2, House, iisg:hasAuthority, Authority),
  % Some authorities only appear in the source file for houses.
  (   instance(B1, Authority, iisg:'Authority')
  ->  true
  ;   assert_instance(B1, Authority, iisg:'Authority'),
      assert_triple(B1, Authority, rdfs:label, string(Properties.'AUTHORITY'))
  ),
  % iisg:House iisg:alloy iisg:Alloy
  (   Properties.'ALLOY' == null
  ->  debug(known_issue, "House without alloy: ‘~a’.", [Properties.'MINT'])
  ;   rdf_create_iri(iisg, [Properties.'ALLOY'], Alloy),
      assert_triple(B2, House, iisg:alloy, Alloy)
  ),
  % iisg:House iisg:source xsd:string
  split_string(Properties.'SOURCE', ";", " ", Sources),
  forall(
    member(Source, Sources),
    assert_triple(B2, House, iisg:source, Source)
  ),
  % iisg:Authority geo:hasGeometry/geo:asWKT SHAPE
  _{geometry: Dict} :< Feature,
  (   Dict == null
  ->  well_known_iri(Geometry),
      debug(known_issue, "House without geometry: ‘~a’.", [Properties.'AUTHORITY'])
  ;   geojson_shape(Dict, Shape),
      assert_shape(B2, House, Shape, Geometry)
  ),
  % geo:Geometry iisg:range xsd:gYear
  date(Properties.'DATEfrom', Begin),
  assert_triple(B2, Geometry, iisg:begin, Begin),
  % geo:Geometry iisg:end xsd:gYear
  date(Properties.'DATEto', End),
  assert_triple(B2, Geometry, iisg:end, End).



%! export_schema(+VocabFile:atom, +SvgFile:atom) is det.

export_schema(VocabFile, SvgFile) :-
  rdf_equal(graph:vocab, VocabG),
  setup_call_cleanup(
    rdf_load_file(VocabFile, [graph(DefG)]),
    shacl_export_file(mem(VocabG), SvgFile),
    maplist(rdf_retract_graph, [DefG,VocabG])
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
