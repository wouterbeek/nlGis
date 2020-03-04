:- encoding(utf8).
:- module(script, [run/0]).

/** <module> CShapes ETL

@author Wouter Beek
@see http://downloads.weidmann.ws/cshapes/Shapefiles/
@version 2017-2020
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

:- use_module(library(call_ext)).
:- use_module(library(date_time)).
:- use_module(library(dcg)).
:- use_module(library(file_ext)).
:- use_module(library(geo/gml)).
:- use_module(library(graph/gv)).
:- use_module(library(http/http_client2)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(rdf/rdf_export)).
:- use_module(library(rdf/rdf_mem)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(shacl/shacl_export)).
:- use_module(library(triply/triply_api)).
:- use_module(library(xml_ext)).

:- curl.

:- debug(cshapes).

:- maplist(rdf_register_prefix, [
     cow-'https://iisg.amsterdam/vocab/cow/',
     'cow-country'-'https://iisg.amsterdam/resource/cow/country/',
     'cow-observation'-'https://iisg.amsterdam/resource/cow/observation/',
     cshapes-'https://iisg.amsterdam/vocab/cshapes/',
     capital-'https://iisg.amsterdam/resource/cshapes/geometry/',
     dimension-'https://iisg.amsterdam/resource/cshapes/dimension/',
     geometry-'https://iisg.amsterdam/resource/cshapes/geometry/',
     graph-'https://iisg.amsterdam/resource/cshapes/graph/',
     gw-'https://iisg.amsterdam/vocab/gw/',
     'gw-country'-'https://iisg.amsterdam/resource/gw/country/',
     'gw-observation'-'https://iisg.amsterdam/resource/gw/observation/',
     instant-'https://iisg.amsterdam/resource/cshapes/instant/',
     interval-'https://iisg.amsterdam/resource/cshapes/interval/',
     iso-'https://iisg.amsterdam/vocab/iso/',
     'iso-country'-'https://iisg.amsterdam/resource/iso/country/',
     measure-'https://iisg.amsterdam/resource/cshapes/measure/',
     prov,
     qb,
     time
   ]).

:- set_setting(rdf_term:bnode_prefix_authority, 'iisg.amsterdam').
:- set_setting(rdf_term:bnode_prefix_scheme, https).

run :-
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
  export_schema(VocabFile, SvgFile).





%! cshapes_record(+Dom:list(compound)) is det.

cshapes_record([element(_,_,Dom)]) :-
  flag(number_of_records, N, N+1),
  rdf_prefix_iri(graph, data, Name),
  create_dataset([graph(memory(Name))], [], Dataset),
  (   assert_cshape(Dom, Dataset)
  ->  true
  ;   print_message(warning, format("Failed to process record № ~D.", [N]))
  ),
  debug(cshapes, "Processed record ~D.", [N]).

assert_cshape(Dom, Dataset) :-
  % Geometry: identified by the GML feature ID.
  xpath_chk(Dom, //'ogr:FEATUREID'(normalize_space), GeometryCode),
  rdf_create_iri(geometry, [GeometryCode], Geometry),
  % a cshapes:Geometry
  assert_instance(Geometry, cshapes:'Geometry', Dataset),
  % geo:asWKT geo:wktLiteral
  xpath_chk(Dom, //'ogr:geometryProperty'(content), [GeoDom]),
  gml_shape(GeoDom, Shape),
  assert_triple(Geometry, geo:asWKT, Shape, Dataset),
  % cshapes:area xsd:double
  xpath_chk(Dom, //'ogr:AREA'(number), Area),
  assert_triple(Geometry, cshapes:area, Area, Dataset),
  % Country: identified by the CoW, G&W, or ISO ID.
  xpath_chk(Dom, //'ogr:CNTRY_NAME'(normalize_space), Name),
  assert_cow(Dom, Name, GeometryCode, Geometry, Country1, Dataset),
  assert_gw(Dom, Name, GeometryCode, Geometry, Country2, Dataset),
  assert_iso(Dom, Name, Geometry, Country3, Dataset),
  % Link countries to their capital.
  assert_capital(Dom, Country1, Country2, Country3, Dataset).

space_to_hyphen, "-" --> " ", !, space_to_hyphen.
space_to_hyphen, [C] --> [C], !, space_to_hyphen.
space_to_hyphen --> "".

assert_capital(Dom, Country1, Country2, Country3, Dataset) :-
  % Capital: identified by its name.
  xpath_chk(Dom, //'ogr:CAPNAME'(normalize_space), Name),
  atom_phrase(space_to_hyphen, Name, Local),
  rdf_create_iri(capital, [Local], Capital),
  assert_instance(Capital, cshapes:'Capital', Dataset),
  % geo:hasGeometry/geo:asWKT geo:wktLiteral
  xpath_chk(Dom, //'ogr:CAPLONG'(number), CapitalLong),
  xpath_chk(Dom, //'ogr:CAPLAT'(number), CapitalLat),
  assert_geometry(shape(_,_,_,'Point'([CapitalLong,CapitalLat])), Capital, Dataset),
  % rdfs:label rdf:langString
  assert_label(Capital, Name-[en], Dataset),
  % cshapes:capital cshapes:Capital
  call_if_ground(
    Country1,
    assert_triple(Country1, cshapes:capital, Capital, Dataset)
  ),
  call_if_ground(
    Country2,
    assert_triple(Country2, cshapes:capital, Capital, Dataset)
  ),
  call_if_ground(
    Country3,
    assert_triple(Country3, cshapes:capital, Capital, Dataset)
  ).

assert_cow(Dom, Name, GeometryCode, Geometry, Country, Dataset) :-
  % cow:Country
  xpath_chk(Dom, //'ogr:COWCODE'(normalize_space), CountryCode),
  CountryCode \== '-1', !,
  rdf_create_iri('cow-country', [CountryCode], Country),
  assert_instance(Country, cow:'Country', Dataset),
  % rdfs:label rdf:langString
  assert_label(Country, Name-[en], Dataset),
  % geo:hasGeometry cshapes:Geometry
  assert_triple(Country, geo:hasGeometry, Geometry, Dataset),
  % cow:number xsd:positiveInteger
  atom_number(CountryCode, Number),
  assert_triple(Country, cow:number, positive_integer(Number), Dataset),
  % observation
  (   xpath_chk(Dom, //'ogr:COWSYEAR'(normalize_space), Y1),
      Y1 \== '-1'
  ->  xpath_chk(Dom, //'ogr:COWSMONTH'(normalize_space), M1),
      xpath_chk(Dom, //'ogr:COWSDAY'(normalize_space), D1),
      maplist(atom_number, [Y1,M1,D1], [Y2,M2,D2]),
      BeginDate = date(Y2,M2,D2)
  ;   true
  ),
  (   xpath_chk(Dom, //'ogr:COWEYEAR'(normalize_space), Y3),
      Y3 \== '-1'
  ->  xpath_chk(Dom, //'ogr:COWEMONTH'(normalize_space), M3),
      xpath_chk(Dom, //'ogr:COWEDAY'(normalize_space), D3),
      maplist(atom_number, [Y3,M3,D3], [Y4,M4,D4]),
      EndDate = date(Y4,M4,D4)
  ;   true
  ),
  assert_instant(instant, BeginDate, BeginLex, Begin, Dataset),
  assert_instant(instant, EndDate, EndLex, End, Dataset),
  assert_interval(interval, BeginLex, Begin, EndLex, End, Interval, Dataset),
  assert_observation(
    'cow-observation',
    GeometryCode,
    CountryCode,
    "Correlates of War"-[en],
    Country,
    Interval,
    Geometry,
    Dataset).
assert_cow(_, _, _, _, _, _).

assert_gw(Dom, Name, GeometryCode, Geometry, Country, Dataset) :-
  % IRI
  xpath_chk(Dom, //'ogr:GWCODE'(normalize_space), CountryCode),
  CountryCode \== '-1', !,
  rdf_create_iri('gw-country', [CountryCode], Country),
  % a gw:Country
  assert_instance(Country, gw:'Country', Dataset),
  % rdfs:label rdf:langString
  assert_label(Country, Name-[en], Dataset),
  % geo:hasGeometry cshapes:Geometry
  assert_triple(Country, geo:hasGeometry, Geometry, Dataset),
  % gw:number xsd:positiveInteger
  atom_number(CountryCode, Number),
  assert_triple(Country, gw:number, positive_integer(Number), Dataset),
  % observation
  (   xpath_chk(Dom, //'ogr:GWSYEAR'(normalize_space), Y5),
      Y5 \== '-1'
  ->  xpath_chk(Dom, //'ogr:GWSMONTH'(normalize_space), M5),
      xpath_chk(Dom, //'ogr:GWSDAY'(normalize_space), D5),
      maplist(atom_number, [Y5,M5,D5], [Y6,M6,D6]),
      BeginDate = date(Y6,M6,D6)
  ;   true
  ),
  (   xpath_chk(Dom, //'ogr:GWEYEAR'(normalize_space), Y7),
      Y7 \== '-1'
  ->  xpath_chk(Dom, //'ogr:GWEMONTH'(normalize_space), M7),
      xpath_chk(Dom, //'ogr:GWEDAY'(normalize_space), D7),
      maplist(atom_number, [Y7,M7,D7], [Y8,M8,D8]),
      EndDate = date(Y8,M8,D8)
  ;   true
  ),
  assert_instant(instant, BeginDate, BeginLex, Begin, Dataset),
  assert_instant(instant, EndDate, EndLex, End, Dataset),
  assert_interval(interval, BeginLex, Begin, EndLex, End, Interval, Dataset),
  assert_observation(
    'gw-observation',
    GeometryCode,
    CountryCode,
    "Gleditsch & Ward"-[en],
    Country,
    Interval,
    Geometry,
    Dataset
  ).
assert_gw(_, _, _, _, _, _).

assert_instant(Alias, Date, Lex, Instant, Dataset) :-
  rdf_literal_dwim(Date, O),
  rdf_literal_lexical_form(O, Lex),
  rdf_prefix_iri(Alias, [Lex], Instant),
  % rdf:type time:Instant
  assert_instance(Instant, time:'Instant', Dataset),
  % rdfs:label rdf:langString
  date_time_label(Date, Label),
  assert_label(Instant, Label, Dataset),
  % time:isXSDDate xsd:date
  assert_triple(Instant, time:inXSDDate, O, Dataset).

assert_interval(Alias, BeginLex, Begin, EndLex, End, Interval, Dataset) :-
  format(atom(Local), "~a…~a", [BeginLex,EndLex]),
  rdf_prefix_iri(Alias, [Local], Interval),
  % rdf:type time:ProperInterval
  assert_instance(Interval, time:'ProperInterval', Dataset),
  % rdfs:label xsd:string
  format(string(Label), "[~s, ~s]", [BeginLex,EndLex]),
  assert_label(Interval, Label, Dataset),
  % time:begin time:Instant
  call_if_ground(Begin, assert_triple(Interval, time:begin, Begin, Dataset)),
  % time:end time:Instant
  call_if_ground(End, assert_triple(Interval, time:end, End, Dataset)).

assert_iso(Dom, Name, Geometry, Country, Dataset) :-
  xpath_chk(Dom, //'ogr:ISO1NUM'(normalize_space), CountryCode),
  CountryCode \== '0', !,
  rdf_create_iri('iso-country', [CountryCode], Country),
  assert_instance(Country, iso:'Country', Dataset),
  assert_label(Country, Name-[en], Dataset),
  assert_triple(Country, geo:hasGeometry, Geometry, Dataset),
  % iso:Alpha2 xsd:string
  xpath_chk(Dom, //'ogr:ISO1AL2'(normalize_space), IsoAlpha2),
  assert_triple(Country, iso:alpha2, string(IsoAlpha2), Dataset),
  % iso:Alpha3 xsd:string
  xpath_chk(Dom, //'ogr:ISO1AL3'(normalize_space), IsoAlpha3),
  assert_triple(Country, iso:alpha3, string(IsoAlpha3), Dataset),
  % iso:Name rdf:langString
  xpath_chk(Dom, //'ogr:ISONAME'(normalize_space), IsoName),
  assert_triple(Country, iso:name, IsoName-[en], Dataset),
  % iso:Number xsd:positiveInteger
  atom_number(CountryCode, Number),
  assert_triple(Country, iso:number, positive_integer(Number), Dataset).
assert_iso(_, _, _, _, _).

assert_observation(Alias, GeometryCode, CountryCode, Attribution, Country, Interval, Geometry, Dataset) :-
  atomic_list_concat([GeometryCode,CountryCode], -, Local),
  rdf_prefix_iri(Alias, Local, Observation),
  % a qb:Observation
  assert_instance(Observation, qb:'Observation', Dataset),
  % dimension:attribution rdfs:Resource
  assert_triple(Observation, dimension:attribution, Attribution, Dataset),
  assert_triple(Observation, prov:wasAttrbiutedTo, Attribution, Dataset),
  % dimension:country cshapes:Country
  assert_triple(Observation, dimension:country, Country, Dataset),
  % dimension:temporalExtent time:ProperInterval
  assert_triple(Observation, dimension:temporalExtent, Interval, Dataset),
  % measure:spatialExtent cshapes:Geometry
  assert_triple(Observation, measure:spatialExtent, Geometry, Dataset).



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
  rdf_prefix_iri(graph, vocab, VocabGraphName),
  create_dataset([graph(memory(VocabGraphName))], [], Dataset),
  setup_call_cleanup(
    rdf_load_file(FromFile, [graph(DefG)]),
    shacl_export_file(Dataset, ToFile),
    maplist(rdf_retract_graph, [DefG,VocabGraphName])
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
  rdf_save_file(ToFile).
  %delete_files_by_extension(gml).



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
