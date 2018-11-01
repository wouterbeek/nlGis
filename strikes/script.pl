:- encoding(utf8).
/* Strikes ETL

@author Wouter Beek
@see https://datasets.socialhistory.org/dataset.xhtml?persistentId=hdl:10622/APNT4U
@version 2017-2018
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(settings)).
:- use_module(library(thread)).
:- use_module(library(yall)).

:- use_module(library(csv_ext)).
:- use_module(library(dcg)).
:- use_module(library(file_ext)).
:- use_module(library(graph/gv)).
:- use_module(library(http/http_client2)).
:- use_module(library(os_ext)).
:- use_module(library(semweb/rdf_export)).
:- use_module(library(semweb/rdf_mem)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_term)).
:- use_module(library(semweb/shacl_export)).
:- use_module(library(stream_ext)).
:- use_module(library(string_ext)).
:- use_module(library(tapir/tapir_api)).

:- curl.

:- debug(known_issue).
:- debug(unknown_issue).

:- maplist(rdf_register_prefix, [
     dcterm,
     graph-'https://iisg.amsterdam/graph/strikes/',
     iisg-'https://iisg.amsterdam/vocab/',
     resource-'https://iisg.amsterdam/resource/'
   ]).

:- rdf_meta
   action_iri(+, r),
   character_iri(+, r),
   result_iri(+, r).

:- set_setting(rdf_term:bnode_prefix_authority, 'iisg.amsterdam').
:- set_setting(rdf_term:bnode_prefix_scheme, https).

etl :-
  % .xlsx → .csv
  SourceFile = 'Netherlands_2017.xlsx',
  CsvFile = 'source.csv',
  convert_file(SourceFile, csv, CsvFile),

  % WINDOWS-1252 → UTF-8
  recode_file(CsvFile, 'windows-1252'),

  % .csv → .nq.gz
  DataFile = 'data.nq.gz',
  transform(CsvFile, DataFile),

  % .trig.gz → .svg
  VocabFile = 'vocab.trig',
  ImgFile  ='vocab.svg',
  export_scheme(VocabFile, ImgFile).

  % Upload to Druid.
  %upload(DataFile, VocabFile, ImgFile).



%! cnvert_row(+Row:compound, +G:rdf_graph) is det.

convert_row(Row, G) :-
  compound_name_arguments(Row, row, [Id|T]),
  convert_row(Id, T, G).

convert_row(Id, _, _) :-
  memberchk(Id, [15039,15052,16157,16182,16627]), !,
  debug(known_issue, "Skip buggy ID ‘~d’.", [Id]).
convert_row(Id, L, G) :-
  % iisg:Strike
  atom_number(Local, Id),
  rdf_create_iri(resource, [strike,Local], Strike),
  rdf_assert_triple(Strike, rdf:type, iisg:'Strike', G),
  % dcterm:description
  nth1(5, L, Description),
  (   Description == ''
  ->  true
  ;   rdf_assert_triple(Strike, dcterm:description, Description-'nl-nl', G)
  ),
  % iisg:action
  nth1(10, L, Action0),
  (   Action0 == ''
  ->  true
  ;   action_iri(Action0, Action)
  ->  rdf_assert_triple(Strike, iisg:action, Action, G)
  ;   debug(unknown_issue, "Unrecognized action ‘~a’ for ID ‘~d’.", [Action0,Id])
  ),
  % iisg:character
  nth1(6, L, Character0),
  (   Character0 == ''
  ->  true
  ;   character_iri(Character0, Character)
  ->  rdf_assert_triple(Strike, iisg:character, Character, G)
  ;   debug(unknown_issue, "Unrecognized character ‘~a’ for ID ‘~d’.", [Character0,Id])
  ),
  % iisg:Strike iisg:company iisg:Company
  % iisg:Company rdfs:label rdf:langString
  nth1(12, L, CompanyString),
  forall(
    (
      split_string(CompanyString, ";", " ", CompanyNames),
      member(CompanyName, CompanyNames),
      \+ memberchk(CompanyName, ["","algemeen","diverse / several","onbekend"])
    ),
    (
      atomic_list_concat(CompanyNameParts, ' ', CompanyName),
      atomic_list_concat(CompanyNameParts, -, CompanyLocal),
      rdf_create_iri(resource, [company,CompanyLocal], Company),
      rdf_assert_triple(Company, rdf:type, iisg:'Company', G),
      rdf_assert_triple(Company, rdfs:label, CompanyName-'nl-nl', G),
      rdf_assert_triple(Strike, iisg:company, Company, G)
    )
  ),
  % iisg:date
  nth1(3, L, Day),
  nth1(2, L, Month),
  nth1(1, L, Year),
  (   Year == ''
  ->  true
  ;   Day == ''
  ->  (   Month == ''
      ->  rdf_assert_triple(Strike, iisg:date, year(Year), G)
      ;   rdf_assert_triple(Strike, iisg:date, year_month(Year,Month), G)
      )
  ;   rdf_assert_triple(Strike, iisg:date, date(Year,Month,Day), G)
  ),
  % iisg:duration
  nth1(4, L, Duration),
  (   Duration == ''
  ->  true
  ;   rdf_assert_triple(Strike, iisg:duration, nonneg(Duration), G)
  ),
  % iisg:id xsd:string
  number_string(Id, Id0),
  rdf_assert_triple(Strike, iisg:id, Id0, G),
  % iisg:occupation
  nth1(14, L, Occupation),
  (   Occupation == ''
  ->  true
  ;   rdf_assert_triple(Strike, iisg:occupation, Occupation-'en-gb', G)
  ),
  % iisg:place
  nth1(13, L, PlaceString),
  split_string(PlaceString, ";", " ", PlaceComps),
  maplist(assert_place(Strike,G), PlaceComps),
  % iisg:result
  nth1(8, L, Result0),
  (   result_iri(Result0, Result)
  ->  rdf_assert_triple(Strike, iisg:result, Result, G)
  ;   true
  ),
  % iisg:sector
  nth1(9, L, Sector),
  (   Sector == ''
  ->  true
  ;   rdf_assert_triple(Strike, iisg:sector, Sector-'en-gb', G)
  ),
  % iisg:specification-of-action
  nth1(11, L, SpecificationOfAction),
  (   SpecificationOfAction == ''
  ->  true
  ;   rdf_assert_triple(Strike, iisg:'specification-of-action', SpecificationOfAction-'en-gb', G)
  ),
  % iisg:number-of-campaigners
  % iisg:number-of-companies
  % iisg:number-of-days-indirect-strikers
  % iisg:number-of-days-locked-out-workers
  % iisg:number-of-indirect-strikers
  % iisg:number-of-laid-off-workers
  % iisg:number-of-locked-out-workers
  % iisg:number-of-actions
  % iisg:number-of-days
  % iisg:number-of-workers
  nth1(15, L, Totals),
  split_string(Totals, ";", " ", TotalComps),
  maplist(assert_total(Strike,G), TotalComps),
  % iisg:typeOfStrike
  nth1(7, L, Type),
  (   Type == ''
  ->  true
  ;   rdf_assert_triple(Strike, iisg:'type-of-strike', Type-'en-gb', G)
  ).

assert_place(_, _, "") :- !.
assert_place(Strike, G, String) :-
  string_phrase(province_place(Province,Place), String), !,
  rdf_assert_triple(Strike, iisg:province, Province-'nl-nl', G),
  rdf_assert_triple(Strike, iisg:place, Place-'nl-nl', G).
assert_place(Strike, G, Place) :-
  rdf_assert_triple(Strike, iisg:place, Place-'nl-nl', G).

province_place(Province, Place) -->
  ...(Codes1),
  " ( ", !,
  ...(Codes2),
  " )", !,
  {maplist(string_codes, [Place,Province], [Codes1,Codes2])}.

action_iri('Lockout', iisg:lockout).
action_iri('Nothing happened', iisg:nothing).
action_iri('Other action due to labour disputes', iisg:other).
action_iri('Strike', iisg:strike).

character_iri('Union', iisg:union).
character_iri('Wildcat', iisg:wildcat).

result_iri('lost', iisg:lost).
result_iri('settled', iisg:settled).
result_iri('undecided', iisg:undecided).
result_iri('victory', iisg:victory).

assert_total(_, _, "") :- !.
assert_total(Strike, G, String) :-
  string_phrase(total(P, N), String),
  rdf_assert_triple(Strike, P, nonneg(N), G).

total(P, N) -->
  (   "Campaigners"
  ->  {rdf_equal(iisg:'number-of-campaigners', P)}
  ;   "Companies involved"
  ->  {rdf_equal(iisg:'number-of-companies', P)}
  ;   "Days not worked by indirect strikers"
  ->  {rdf_equal(iisg:'number-of-days-indirect-strikers', P)}
  ;   "Days not worked by locked out workers"
  ->  {rdf_equal(iisg:'number-of-days-locked-out-workers', P)}
  ;   "Indirect strikers"
  ->  {rdf_equal(iisg:'number-of-indirect-strikers', P)}
  ;   "Laid off workers"
  ->  {rdf_equal(iisg:'number-of-laid-off-workers', P)}
  ;   "Locked out workers"
  ->  {rdf_equal(iisg:'number-of-locked-out-workers', P)}
  ;   "Number of actions"
  ->  {rdf_equal(iisg:'number-of-actions', P)}
  ;   "Strike days"
  ->  {rdf_equal(iisg:'number-of-days', P)}
  ;   "Workers involved"
  ->  {rdf_equal(iisg:'number-of-workers', P)}
  ),
  ": ",
  integer(N).



%! export_scheme(+VocabFile:atom, +ImgFile:atom) is det.

export_scheme(VocabFile, ImgFile) :-
  rdf_equal(graph:vocab, VocabG),
  setup_call_cleanup(
    rdf_load_file(VocabFile, [graph(DefG)]),
    shacl_export_file(mem(VocabG), ImgFile),
    maplist(rdf_retract_graph, [DefG,VocabG])
  ).



%! transform(+CsvFile:atom, +DataFile:atom) is det.

transform(CsvFile, DataFile) :-
  rdf_equal(DataG, graph:data),
  read_from_file(CsvFile, transform_stream(DataG)),
  write_to_file(DataFile, {DataG}/[Out]>>rdf_write_quads(Out, DataG)),
  delete_file(CsvFile).

transform_stream(DataG, In) :-
  forall(
    csv_read_stream_row(In, Row),
    convert_row(Row, DataG)
  ).



%! upload(+DataFile:atom, +VocabFile:atom, +ImgFile:atom) is det.

upload(DataFile, VocabFile, ImgFile) :-
  rdf_bnode_prefix(BNodePrefix),
  Properties = _{
    accessLevel: public,
    assets: [ImgFile],
    avatar: 'avatar.jpg',
    description: "Netherlands Strikes, lockouts and other forms of labour conflict (number, number of companies, workers, days lost), 1372-2010.",
    files: [DataFile,'meta.trig',VocabFile],
    prefixes: [
      bnode-BNodePrefix,
      company-'https://iisg.amsterdam/resource/company/',
      dataset-'https://iisg.amsterdam/dataset/',
      graph,
      sh,
      strike-'https://iisg.amsterdam/resource/strike/',
      iisg
    ]
  },
  dataset_upload(druid, dataLegend, strikes, Properties),
  concurrent_maplist(delete_file, [DataFile,ImgFile]).
