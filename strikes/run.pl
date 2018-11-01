:- module(strikes, [run/0]).

/** <module> Strikes

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(settings)).
:- use_module(library(thread)).
:- use_module(library(yall)).
:- use_module(library(zlib)).

:- use_module(library(csv_ext)).
:- use_module(library(dcg)).
:- use_module(library(file_ext)).
:- use_module(library(graph/gv)).
:- use_module(library(os_ext)).
:- use_module(library(semweb/rdf_export)).
:- use_module(library(semweb/rdf_mem)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_term)).
:- use_module(library(semweb/shacl_export)).
:- use_module(library(stream_ext)).
:- use_module(library(string_ext)).
:- use_module(library(tapir/tapir_api)).

:- rdf_meta
   action_iri(+, r),
   character_iri(+, r),
   convert_row(+, r),
   result_iri(+, r).

:- maplist(rdf_register_prefix, [
     dcterm,
     graph-'https://iisg.amsterdam/graph/strikes/',
     resource-'https://iisg.amsterdam/resource/',
     vocab-'https://iisg.amsterdam/vocab/'
   ]).

:- set_setting(rdf_term:bnode_prefix_authority, 'iisg.amsterdam').
:- set_setting(rdf_term:bnode_prefix_scheme, https).

run :-
  % .xlsx → .csv
  convert_file('data.xlsx', csv, File),

  % WINDOWS-1252 → UTF-8
  recode_file(File, 'windows-1252'),

  % .csv → .nq.gz
  rdf_equal(G, graph:data),
  read_from_file(
    File,
    [In]>>forall(
            csv_read_stream_row(In, Row),
            convert_row(Row, G)
          )
  ),
  delete_file(File),
  write_to_file('data.nq.gz', rdf_write_quads),

  % .trig.gz → .svg
  rdf_equal(graph:vocab, VocabG),
  setup_call_cleanup(
    rdf_load_file('vocab.trig', [graph(DefG)]),
    gv_export_file(mem(G), 'vocab.svg'),
    maplist(rdf_retract_graph, [DefG,VocabG])
  ),

  % upload to Druid.
  rdf_bnode_prefix(BNodePrefix),
  Properties = _{
    accessLevel: public,
    assets: ['vocab.svg'],
    avatar: 'avatar.jpg',
    description: "Netherlands Strikes, lockouts and other forms of labour conflict (number, number of companies, workers, days lost), 1372-2010.",
    files: ['data.nq.gz','meta.trig','vocab.trig'],
    prefixes: [
      bnode-BNodePrefix,
      company-'https://iisg.amsterdam/resource/company/',
      dataset-'https://iisg.amsterdam/dataset/',
      graph,
      sh,
      strike-'https://iisg.amsterdam/resource/strike/',
      vocab
    ]
  },
  dataset_upload(druid, nlgis, strikes, Properties),

  % cleanup temporary files
  concurrent_maplist(delete_file, ['data.nq.gz','vocab.svg']),
  halt.

convert_row(
  row(
    Id, Year, Month, Day, DurationAtom, Description, Character1, TypeOfStrike,
    Result1, Sector, Action1, SpecificationOfAction, CompanyString, Place,
    Occupation, Totals
  ),
  G
) :-
  % vocab:Strike
  atom_number(Local, Id),
  rdf_create_iri(resource, [strike,Local], Strike),
  rdf_assert_triple(Strike, rdf:type, vocab:'Strike', G),
  % dc:description
  (   Description == ''
  ->  true
  ;   rdf_assert_triple(Strike, dcterm:description, Description-'nl-nl', G)
  ),
  % vocab:action
  (action_iri(Action1, Action2)),
  rdf_assert_triple(Strike, vocab:action, Action2, G),
  % vocab:character
  (   character_iri(Character1, Character2)
  ->  rdf_assert_triple(Strike, vocab:character, Character2, G)
  ;   true
  ),
  % vocab:Strike vocab:company vocab:Company
  % vocab:Company rdfs:label rdf:langString
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
      rdf_assert_triple(Company, rdf:type, vocab:'Company', G),
      rdf_assert_triple(Company, rdfs:label, CompanyName-'nl-nl', G),
      rdf_assert_triple(Strike, vocab:company, Company, G)
    )
  ),
  % vocab:date
  (   Day == ''
  ->  (   Month == ''
      ->  rdf_assert_triple(Strike, vocab:date, year(Year), G)
      ;   rdf_assert_triple(Strike, vocab:date, year_month(Year,Month), G)
      )
  ;   rdf_assert_triple(Strike, vocab:date, date(Year,Month,Day), G)
  ),
  % vocab:duration
  (   DurationAtom == ''
  ->  true
  ;   atom_number(Duration, DurationAtom),
      rdf_assert_triple(Strike, vocab:duration, nonneg(Duration), G)
  ),
  % vocab:id xsd:string
  rdf_assert_triple(Strike, vocab:id, str(Id), G),
  % vocab:occupation
  (   Occupation == ''
  ->  true
  ;   rdf_assert_triple(Strike, vocab:occupation, Occupation-'en-gb', G)
  ),
  % vocab:place
  split_string(Place, ";", " ", PlaceComps),
  maplist(
    {Strike,G}/[String]>>(
      string_phrase(place(Province-Place), String),
      % TBD: link to province
      rdf_assert_triple(Strike, vocab:province, Province-'nl-nl', G),
      % TBD: link to GeoNames
      rdf_assert_triple(Strike, vocab:place, Place-'nl-nl', G)
    ),
    PlaceComps
  ),
  % vocab:result
  (   result_iri(Result1, Result2)
  ->  rdf_assert_triple(Strike, vocab:result, Result2, G)
  ;   true
  ),
  % vocab:sector
  (   Sector == ''
  ->  true
  ;   rdf_assert_triple(Strike, vocab:sector, Sector-'en-gb', G)
  ),
  % vocab:specification-of-action
  (   SpecificationOfAction == ''
  ->  true
  ;   rdf_assert_triple(Strike, vocab:'specification-of-action', SpecificationOfAction-'en-gb', G)
  ),
  split_string(Totals, ";", " ", TotalComps),
  maplist(
    {Strike,G}/[String]>>(
      string_phrase(total(P, N), String),
      rdf_assert_triple(Strike, P, nonneg(N), G)
    ),
    TotalComps
  ),
  % vocab:typeOfStrike
  (   TypeOfStrike == ''
  ->  true
  ;   rdf_assert_triple(Strike, vocab:'type-of-strike', TypeOfStrike-'en-gb', G)
  ).

place(Province-Place) -->
  ...(Codes1),
  " ( ", !,
  ...(Codes2),
  " )", !,
  {maplist(string_codes, [Place,Province], [Codes1,Codes2])}.

action_iri('Lockout', vocab:lockout).
action_iri('Nothing happened', vocab:nothing).
action_iri('Other action due to labour disputes', vocab:other).
action_iri('Strike', vocab:strike).

character_iri('Union', vocab:union).
character_iri('Wildcat', vocab:wildcat).

result_iri('lost', vocab:lost).
result_iri('settled', vocab:settled).
result_iri('undecided', vocab:undecided).
result_iri('victory', vocab:victory).

total(P, N) -->
  (   "Campaigners"
  ->  {rdf_equal(vocab:'number-of-campaigners', P)}
  ;   "Companies involved"
  ->  {rdf_equal(vocab:'number-of-companies', P)}
  ;   "Days not worked by indirect strikers"
  ->  {rdf_equal(vocab:'number-of-days-indirect-strikers', P)}
  ;   "Days not worked by locked out workers"
  ->  {rdf_equal(vocab:'number-of-days-locked-out-workers', P)}
  ;   "Indirect strikers"
  ->  {rdf_equal(vocab:'number-of-indirect-strikers', P)}
  ;   "Laid off workers"
  ->  {rdf_equal(vocab:'number-of-laid-off-workers', P)}
  ;   "Locked out workers"
  ->  {rdf_equal(vocab:'number-of-locked-out-workers', P)}
  ;   "Number of actions"
  ->  {rdf_equal(vocab:'number-of-actions', P)}
  ;   "Strike days"
  ->  {rdf_equal(vocab:'number-of-days', P)}
  ;   "Workers involved"
  ->  {rdf_equal(vocab:'number-of-workers', P)}
  ),
  ": ",
  integer(N).
