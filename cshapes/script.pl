:- encoding(utf8).
/* CShapes ETL script

@author Wouter Beek
@see http://downloads.weidmann.ws/cshapes/Shapefiles/
@version 2017-2018
*/

:- use_module(library(apply)).
:- use_module(library(archive)).
:- use_module(library(process)).
:- use_module(library(settings)).
:- use_module(library(sgml)).
:- use_module(library(uri)).
:- use_module(library(xpath)).
:- use_module(library(yall)).
:- use_module(library(zlib)).

:- use_module(library(dcg)).
:- use_module(library(file_ext)).
:- use_module(library(gis/gml)).
:- use_module(library(graph/gv)).
:- use_module(library(http/http_client2)).
:- use_module(library(semweb/rdf_export)).
:- use_module(library(semweb/rdf_mem)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_term)).
:- use_module(library(semweb/shacl_export)).
:- use_module(library(tapir/tapir_api)).
:- use_module(library(xml_ext)).

:- curl.

:- maplist(rdf_register_prefix, [
     geo,
     graph-'https://iisg.amsterdam/graph/cshapes/',
     iisg-'https://iisg.amsterdam/vocab/',
     resource-'https://iisg.amsterdam/resource/'
   ]).

:- set_setting(rdf_term:bnode_prefix_authority, 'iisg.amsterdam').
:- set_setting(rdf_term:bnode_prefix_scheme, https).

etl :-
  % .zip → .dbf,.prj,.shp,.shx,.txt
  source_data_uri(Uri),
  download_shp(Uri, ShpFile),

  % .shp → .gml
  GmlFile = 'source.gml',
  DataFile = 'data.nq.gz',
  from_shp_to_gml(ShpFile, GmlFile),

  % .gml → .nq.gz
  gml_to_rdf(GmlFile, DataFile),

  % .trig → .svg
  VocabFile = 'vocab.trig',
  SvgFile = 'vocab.svg',
  export_schema(VocabFile, SvgFile),

  % Upload to Druid.
  upload([DataFile,VocabFile], [SvgFile]).





%! cshapes_record(+Dom:list(compound)) is det.

cshapes_record([element(_,_,Dom)]) :-
  flag(number_of_records, N, N+1),
  (   cshapes_record_(Dom)
  ->  true
  ;   print_message(warning, format("Failed to process record № ~D.", [N]))
  ).

cshapes_record_(Dom) :-
  rdf_equal(graph:data, G),
  % vocab:Country
  xpath_chk(Dom, //'ogr:CNTRY_NAME'(normalize_space), CountryName),
  atom_phrase(space_to_hyphen, CountryName, CountryLocal),
  rdf_create_iri(resource, [country,CountryLocal], Country),
  rdf_assert_triple(Country, rdf:type, iisg:'Country', G),
  % iisg:Country rdfs:label rdf:langString
  rdf_assert_triple(Country, rdfs:label, CountryName-'en-gb', G),
  % iisg:CountrySlice
  (   xpath_chk(Dom, //'ogr:COWSYEAR'(normalize_space), Y1),
      Y1 \== '-1'
  ->  xpath_chk(Dom, //'ogr:COWSMONTH'(normalize_space), M1),
      xpath_chk(Dom, //'ogr:COWSDAY'(normalize_space), D1),
      maplist(atom_number, [Y1,M1,D1], [Y2,M2,D2]),
      CowStart = date(Y2,M2,D2)
  ;   true
  ),
  (   xpath_chk(Dom, //'ogr:COWEYEAR'(normalize_space), Y3),
      Y3 \== '-1'
  ->  xpath_chk(Dom, //'ogr:COWEMONTH'(normalize_space), M3),
      xpath_chk(Dom, //'ogr:COWEDAY'(normalize_space), D3),
      maplist(atom_number, [Y3,M3,D3], [Y4,M4,D4]),
      CowEnd = date(Y4,M4,D4)
  ;   true
  ),
  (   xpath_chk(Dom, //'ogr:GWSYEAR'(normalize_space), Y5),
      Y5 \== '-1'
  ->  xpath_chk(Dom, //'ogr:GWSMONTH'(normalize_space), M5),
      xpath_chk(Dom, //'ogr:GWSDAY'(normalize_space), D5),
      maplist(atom_number, [Y5,M5,D5], [Y6,M6,D6]),
      GwStart = date(Y6,M6,D6)
  ;   true
  ),
  (   xpath_chk(Dom, //'ogr:GWEYEAR'(normalize_space), Y7),
      Y7 \== '-1'
  ->  xpath_chk(Dom, //'ogr:GWEMONTH'(normalize_space), M7),
      xpath_chk(Dom, //'ogr:GWEDAY'(normalize_space), D7),
      maplist(atom_number, [Y7,M7,D7], [Y8,M8,D8]),
      GwEnd = date(Y8,M8,D8)
  ;   true
  ),
  (ground(CowStart) -> Y = Y2 ; ground(GwStart) -> Y = Y6),
  rdf_create_iri(resource, [country,CountryLocal,Y], CountrySlice),
  rdf_assert_triple(CountrySlice, rdf:type, iisg:'CountrySlice', G),
  rdf_assert_triple(Country, iisg:hasTemporalSlice, CountrySlice, G),
  % vocag:CountrySlice rdfs:label rdf:langString
  atomics_to_string([CountryName,Y], " ", Label),
  rdf_assert_triple(CountrySlice, rdfs:label, Label-'en-gb', G),
  % iisg:Capital
  xpath_chk(Dom, //'ogr:CAPNAME'(normalize_space), CapitalName),
  atom_phrase(space_to_hyphen, CapitalName, CapitalLocal),
  rdf_create_iri(resource, [capital,CapitalLocal], Capital),
  rdf_assert_triple(Capital, rdf:type, iisg:'Capital', G),
  % iisg:Capital geo:hasGeometry/geo:asWKT geo:wktLiteral
  xpath_chk(Dom, //'ogr:CAPLONG'(number), CapitalLong),
  xpath_chk(Dom, //'ogr:CAPLAT'(number), CapitalLat),
  rdf_assert_shape(Capital, shape(_,_,_,'Point'([CapitalLong,CapitalLat])), G),
  % iisg:Capital rdfs:label rdf:langString
  rdf_assert_triple(Capital, rdfs:label, CapitalName-'en-gb', G),
  % iisg:CountrySlice iisg:cowCode xsd:string
  (   xpath_chk(Dom, //'ogr:COWCODE'(normalize_space), CowCode),
      CowCode \== '-1'
  ->  rdf_assert_triple(CountrySlice, iisg:cowCode, string(CowCode), G)
  ;   true
  ),
  % iisg:CountrySlice iisg:cowStart xsd:date
  (   ground(CowStart)
  ->  rdf_assert_triple(CountrySlice, iisg:cowStart, CowStart, G)
  ;   true
  ),
  % iisg:CountrySlice iisg:cowEnd xsd:date
  (   ground(CowEnd)
  ->  rdf_assert_triple(CountrySlice, iisg:cowEnd, CowEnd, G)
  ;   true
  ),
  % iisg:CountrySlice iisg:gwStart xsd:date
  (   ground(GwStart)
  ->  rdf_assert_triple(CountrySlice, iisg:gwStart, GwStart, G)
  ;   true
  ),
  % iisg:CountrySlice iisg:gwEnd xsd:date
  (   ground(GwEnd)
  ->  rdf_assert_triple(CountrySlice, iisg:gwEnd, GwEnd, G)
  ;   true
  ),
  % iisg:CountrySlice geo:hasGeometry/geo:asWKT geo:wktLiteral
  xpath_chk(Dom, //'ogr:geometryProperty'(content), [GeoDom]),
  gml_shape(GeoDom, Shape),
  rdf_assert_shape(CountrySlice, Shape, G),
  % iisg:CountrySlice iisg:area xsd:double
  xpath_chk(Dom, //'ogr:AREA'(normalize_space), AreaAtom),
  atom_number(AreaAtom, Area),
  rdf_assert_triple(CountrySlice, iisg:area, Area, G),
  % iisg:CountrySlice iisg:capital iisg:Capital
  rdf_assert_triple(CountrySlice, iisg:capital, Capital, G),
  % iisg:CountrySlice iisg:gwCode xsd:string
  (   xpath_chk(Dom, //'ogr:GWCODE'(normalize_space), GwCode),
      GwCode \== '-1'
  ->  rdf_assert_triple(CountrySlice, iisg:gwCode, string(GwCode), G)
  ;   true
  ),
  % iisg:CountrySlice iisg:isoAlpha2 xsd:string
  (   xpath_chk(Dom, //'ogr:ISO1AL2'(normalize_space), IsoAlpha2)
  ->  rdf_assert_triple(CountrySlice, iisg:isoAlpha2, string(IsoAlpha2), G)
  ;   true
  ),
  % iisg:CountrySlice iisg:isoAlpha3 xsd:string
  (   xpath_chk(Dom, //'ogr:ISO1AL3'(normalize_space), IsoAlpha3)
  ->  rdf_assert_triple(CountrySlice, iisg:isoAlpha3, string(IsoAlpha3), G)
  ;   true
  ),
  % iisg:CountrySlice iisg:isoName rdf:langString
  (   xpath_chk(Dom, //'ogr:ISONAME'(normalize_space), IsoName)
  ->  rdf_assert_triple(CountrySlice, iisg:isoName, IsoName-'en-gb', G)
  ;   true
  ),
  % iisg:CountrySlice iisg:isoNumber xsd:positiveInteger
  (   xpath_chk(Dom, //'ogr:ISO1NUM'(normalize_space), IsoNumberAtom),
      atom_number(IsoNumberAtom, IsoNumber),
      IsoNumber > 0
  ->  rdf_assert_triple(CountrySlice, iisg:isoNumber, positive_integer(IsoNumber), G)
  ;   true
  ).

space_to_hyphen, "-" --> " ", !, space_to_hyphen.
space_to_hyphen, [C] --> [C], !, space_to_hyphen.
space_to_hyphen --> "".



%! cshapes_version(-Version:compound)// .

cshapes_version(version(Major,Minor,Patch)) -->
  "cshapes_",
  integer(Major),
  ".",
  integer(Minor),
  ("-" -> integer(Patch) ; {Patch = 0}),
  ".zip".



%! download_shp(+Uri:atom, -FromFile:atom) is det.

download_shp(Uri, FromFile) :-
  http_call(Uri, download_shp_stream(FromFile)).

download_shp_stream(FromFile, In) :-
  archive_extract(In, ., []),
  expand_file_name("*.shp", [FromFile]),
  delete_directory_and_contents('__MACOSX').



%! export_schema(+FromFile:atom, +ToFile:atom) is det.

export_schema(FromFile, ToFile) :-
  rdf_equal(VocabG, graph:vocab),
  setup_call_cleanup(
    rdf_load_file(FromFile, [graph(DefG)]),
    shacl_export_file(mem(VocabG), ToFile),
    maplist(rdf_retract_graph, [DefG,VocabG])
  ).



%! from_shp_to_gml(+FromFile:atom, +ToFile:atom) is det.

from_shp_to_gml(FromFile, ToFile) :-
  process_create(
    path(ogr2ogr),
    ['-f','GML',file(ToFile),file(FromFile)],
    [process(Pid),stderr(pipe(ProcErr)),stdout(pipe(ProcOut))]
  ),
  thread_create(copy_stream_data(ProcErr, user_error), _, [detached(true)]),
  thread_create(copy_stream_data(ProcOut, user_output), _, [detached(true)]),
  process_wait(Pid, exit(Status)),
  (Status =:= 0 -> true ; print_message(warning, process_status(Status))),
  delete_files_by_extensions([dbf,gfs,prj,shp,shx,xsd]).



%! gml_to_rdf(+FromFile:atom, +ToFile:atom) is det.

gml_to_rdf(FromFile, ToFile) :-
  read_from_file(FromFile, [In]>>call_on_xml(In, ['ogr:cshapes'], cshapes_record)),
  rdf_save_file(ToFile),
  delete_files_by_extension(gml).



%! source_data_uri(-Uri:atom) is det.
%
% Obtains the URI from which the latest version of the source data can
% be downloaded.

source_data_uri(Uri) :-
  BaseUri = 'http://downloads.weidmann.ws/cshapes/Shapefiles/',
  load_html(BaseUri, Dom, []),
  findall(
    Version-RelUri,
    (
      xpath(Dom, //a(@href), RelUri),
      uri_components(RelUri, UriComps),
      uri_data(path, UriComps, Path),
      atom_codes(Path, Codes),
      phrase(cshapes_version(Version), Codes)
    ),
    Pairs
  ),
  sort(1, @>, Pairs, Sorted),
  Sorted = [Version-RelUri|_],
  uri_resolve(RelUri, BaseUri, Uri).



%! upload(+DataFiles:list(atom), +AssetFiles:list(atom)) is det.

upload(DataFiles, AssetFiles) :-
  rename_file('cshapes_shapefile_documentation.txt', 'data.txt'),
  rdf_bnode_prefix(BNodePrefix),
  Properties = _{
    accessLevel: public,
    assets: ['data.txt'|AssetFiles],
    avatar: 'avatar.png',
    description: "The countries of the world, from 1920 untill today.  This dataset includes state boundaries and capitals and is coded according to the Correlates of War and the Gleditsch and Ward (1999) state lists.",
    exampleResources: [
      country-'Netherlands/1946',
      country-'Germany-Democratic-Republic/1954',
      country-'Germany-Federal-Republic/1955',
      country-'Germany/1990',
      country-'India/1947',
      country-'India/1949',
      country-'Chad/1960',
      country-'Chad/1973'
    ],
    files: ['meta.trig'|DataFiles],
    prefixes: [
      bnode-BNodePrefix,
      capital-'https://iisg.amsterdam/resource/capital/',
      country-'https://iisg.amsterdam/resource/country/',
      dataset-'https://iisg.amsterdam/dataset/',
      graph,
      iisg,
      resource,
      sh
    ]
  },
  dataset_upload(druid, nlgis, cshapes, Properties),
  maplist(delete_file, DataFiles),
  maplist(delete_file, AssetFiles),
  delete_files_by_extension(txt).
