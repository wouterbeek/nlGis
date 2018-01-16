:- module(cshapes, [run/0]).

/** <module> CShapes conversion script

@author Wouter Beek
@version 2017/04-2017/12
*/

:- use_module(library(apply)).
:- use_module(library(archive)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(file_ext)).
:- use_module(library(geo/gml)).
:- use_module(library(graph/gv)).
:- use_module(library(http/http_client2)).
:- use_module(library(process)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_export)).
:- use_module(library(semweb/rdf_geo)).
:- use_module(library(semweb/shacl)).
:- use_module(library(sgml)).
:- use_module(library(tapir)).
:- use_module(library(uri)).
:- use_module(library(xml/xml_ext)).
:- use_module(library(xpath)).
:- use_module(library(yall)).
:- use_module(library(zlib)).

:- maplist(rdf_create_prefix, [
     bnode-'https://iisg.amsterdam/.well-known/genid/',
     graph-'https://iisg.amsterdam/graph/cshapes/',
     resource-'https://iisg.amsterdam/resource/',
     vocab-'https://iisg.amsterdam/vocab/'
   ]).

run :-
  % Obtain the download URI.
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
  uri_resolve(RelUri, BaseUri, Uri),
  http_open2(Uri, In),
  call_cleanup(
    run(In),
    close(In)
  ).

run(In1) :-
  % .zip → .dbf,.prj,.shp,.shx,.txt
  archive_extract(In1, ., []),

  % .shp → .gml
  process_create(
    path(ogr2ogr),
    ['-f','GML',file('data.gml'),file('cshapes.shp')],
    [process(Pid),stderr(pipe(ProcErr)),stdout(pipe(ProcOut))]
  ),
  thread_create(copy_stream_data(ProcErr, user_error), _, [detached(true)]),
  thread_create(copy_stream_data(ProcOut, user_output), _, [detached(true)]),
  process_wait(Pid, exit(Status)),
  (Status =:= 0 -> true ; print_message(warning, process_status(Status))),

  % .gml → .nq.gz
  setup_call_cleanup(
    open('data.gml', read, In2),
    call_on_xml(In2, ['ogr:cshapes'], cshapes_record),
    close(In2)
  ),
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
  rename_file('cshapes_shapefile_documentation.txt', 'data.txt'),

  Properties = _{
    accessLevel: public,
    avatar: 'avatar.png',
    binary_files: ['data.txt','vocab.svg'],
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
    files: ['data.nq.gz','meta.trig.gz','vocab.trig.gz'],
    prefixes: [
      bnode,
      capital-'https://iisg.amsterdam/resource/capital/',
      country-'https://iisg.amsterdam/resource/country/',
      dataset-'https://iisg.amsterdam/dataset/',
      graph,
      resource,
      sh,
      vocab
    ]
  },
  dataset_upload(cshapes, Properties),

  % Cleanup temporary files.
  delete_directory_and_contents('__MACOSX'),
  concurrent_maplist(
    delete_files_by_extension,
    [dbf,gml,'nq.gz',prj,shp,shx,txt,xsd]
  ),
  halt.

cshapes_version(version(Major,Minor,Patch)) -->
  "cshapes_",
  integer(Major),
  ".",
  integer(Minor),
  ("-" -> integer(Patch) ; {Patch = 0}),
  ".zip".

cshapes_record([element(_,_,Dom)]) :-
  flag(number_of_records, NumRecords, NumRecords+1),
  (   cshapes_record_(Dom)
  ->  true
  ;   print_message(warning, chsapes_record(NumRecords))
  ).

cshapes_record_(Dom) :-
  rdf_equal(graph:data, G),
  % vocab:Country
  xpath_chk(Dom, //'ogr:CNTRY_NAME'(normalize_space), CountryName),
  atom_phrase(space_to_hyphen, CountryName, CountryLocal),
  rdf_create_iri(resource, [country,CountryLocal], Country),
  rdf_assert(Country, rdf:type, vocab:'Country', G),
  % vocab:Country rdfs:label rdf:langString
  rdf_assert(Country, rdfs:label, CountryName@'en-gb', G),
  % vocab:CountrySlice
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
  rdf_assert(CountrySlice, rdf:type, vocab:'CountrySlice', G),
  rdf_assert(Country, vocab:hasTemporalSlice, CountrySlice, G),
  % vocag:CountrySlice rdfs:label rdf:langString
  atomic_list_concat([CountryName,Y], ' ', Label),
  rdf_assert(CountrySlice, rdfs:label, Label@'en-gb', G),
  % vocab:Capital
  xpath_chk(Dom, //'ogr:CAPNAME'(normalize_space), CapitalName),
  atom_phrase(space_to_hyphen, CapitalName, CapitalLocal),
  rdf_create_iri(resource, [capital,CapitalLocal], Capital),
  rdf_assert(Capital, rdf:type, vocab:'Capital', G),
  % vocab:Capital geo:hasGeometry/geo:asWKT geo:wktLiteral
  xpath_chk(Dom, //'ogr:CAPLONG'(number), CapitalLong),
  xpath_chk(Dom, //'ogr:CAPLAT'(number), CapitalLat),
  rdf_assert_wkt(Capital, 'Point'([CapitalLong,CapitalLat]), G),
  % vocab:Capital rdfs:label rdf:langString
  rdf_assert(Capital, rdfs:label, CapitalName@'en-gb', G),
  % vocab:CountrySlice vocab:cowCode xsd:string
  (   xpath_chk(Dom, //'ogr:COWCODE'(normalize_space), CowCode),
      CowCode \== '-1'
  ->  rdf_assert(CountrySlice, vocab:cowCode, CowCode^^xsd:string, G)
  ;   true
  ),
  % vocab:CountrySlice vocab:cowStart xsd:date
  (   ground(CowStart)
  ->  rdf_assert(CountrySlice, vocab:cowStart, CowStart, G)
  ;   true
  ),
  % vocab:CountrySlice vocab:cowEnd xsd:date
  (   ground(CowEnd)
  ->  rdf_assert(CountrySlice, vocab:cowEnd, CowEnd, G)
  ;   true
  ),
  % vocab:CountrySlice vocab:gwStart xsd:date
  (   ground(GwStart)
  ->  rdf_assert(CountrySlice, vocab:gwStart, GwStart, G)
  ;   true
  ),
  % vocab:CountrySlice vocab:gwEnd xsd:date
  (   ground(GwEnd)
  ->  rdf_assert(CountrySlice, vocab:gwEnd, GwEnd, G)
  ;   true
  ),
  % vocab:CountrySlice geo:hasGeometry/geo:asWKT geo:wktLiteral
  xpath_chk(Dom, //'ogr:geometryProperty'(content), [GeoDom]),
  gml_shape(GeoDom, Shape),
  rdf_assert_wkt(CountrySlice, Shape, G),
  % vocab:CountrySlice vocab:area xsd:double
  xpath_chk(Dom, //'ogr:AREA'(normalize_space), Area0),
  atom_number(Area0, Area),
  rdf_assert(CountrySlice, vocab:area, Area, G),
  % vocab:CountrySlice vocab:capital vocab:Capital
  rdf_assert(CountrySlice, vocab:capital, Capital, G),
  % vocab:CountrySlice vocab:gwCode xsd:string
  (   xpath_chk(Dom, //'ogr:GWCODE'(normalize_space), GwCode),
      GwCode \== "-1"
  ->  rdf_assert(CountrySlice, vocab:gwCode, GwCode^^xsd:string, G)
  ;   true
  ),
  % vocab:CountrySlice vocab:isoAlpha2 xsd:string
  (   xpath_chk(Dom, //'ogr:ISO1AL2'(normalize_space), IsoAlpha2)
  ->  rdf_assert(CountrySlice, vocab:isoAlpha2, IsoAlpha2^^xsd:string, G)
  ;   true
  ),
  % vocab:CountrySlice vocab:isoAlpha3 xsd:string
  (   xpath_chk(Dom, //'ogr:ISO1AL3'(normalize_space), IsoAlpha3)
  ->  rdf_assert(CountrySlice, vocab:isoAlpha3, IsoAlpha3^^xsd:string, G)
  ;   true
  ),
  % vocab:CountrySlice vocab:isoName rdf:langString
  (   xpath_chk(Dom, //'ogr:ISONAME'(normalize_space), IsoName)
  ->  rdf_assert(CountrySlice, vocab:isoName, IsoName@'en-gb', G)
  ;   true
  ),
  % vocab:CountrySlice vocab:isoNumber xsd:positiveInteger
  (   xpath_chk(Dom, //'ogr:ISO1NUM'(normalize_space), IsoNumber0),
      atom_number(IsoNumber0, IsoNumber),
      IsoNumber > 0
  ->  rdf_assert(CountrySlice, vocab:isoNumber, IsoNumber^^xsd:positiveInteger, G)
  ;   true
  ).

space_to_hyphen, "-" --> " ", !, space_to_hyphen.
space_to_hyphen, [C] --> [C], !, space_to_hyphen.
space_to_hyphen --> "".
