:- module(strikes, [run/0]).

/** <module> Strikes

@author Wouter Beek
@version 2017/04-2017/12
*/

:- use_module(library(apply)).
:- use_module(library(csv_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(file_ext)).
:- use_module(library(graph/gv)).
:- use_module(library(lists)).
:- use_module(library(os_ext)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_export)).
:- use_module(library(semweb/rdf_geo)).
:- use_module(library(semweb/shacl)).
:- use_module(library(string_ext)).
:- use_module(library(tapir)).
:- use_module(library(thread)).
:- use_module(library(yall)).
:- use_module(library(zlib)).

:- rdf_meta
   action_iri(+, r),
   character_iri(+, r),
   convert_row(+, r),
   result_iri(+, r).

:- maplist(rdf_create_prefix, [
     bnode-'https://iisg.amsterdam/.well-known/genid/',
     graph-'https://iisg.amsterdam/graph/strikes/',
     resource-'https://iisg.amsterdam/resource/',
     vocab-'https://iisg.amsterdam/vocab/'
   ]).

run :-
  % .xlsx → .csv
  convert_file('data.xlsx', csv, File),

  % WINDOWS-1252 → UTF-8
  recode_file(File),

  % .csv → .nq.gz
  setup_call_cleanup(
    open(File, read, In),
    forall(
      csv_read_stream_row(In, Row),
      convert_row(Row, graph:data)
    ),
    close(In)
  ),
  delete_file(File),
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
    binary_files: ['vocab.svg'],
    description: "Netherlands Strikes, lockouts and other forms of labour conflict (number, number of companies, workers, days lost), 1372-2010.",
    files: ['data.nq.gz','meta.trig','vocab.trig'],
    prefixes: [
      bnode,
      company-'https://iisg.amsterdam/resource/company/',
      dataset-'https://iisg.amsterdam/dataset/',
      graph,
      sh,
      strike-'https://iisg.amsterdam/resource/strike/',
      vocab
    ]
  },
  dataset_upload(strikes, Properties),

  % cleanup temporary files
  concurrent_maplist(delete_file, ['data.nq.gz','vocab.svg']),
  halt.

convert_row(
  row(
    Id, Year, Month, Day, Duration, Description, Character1, TypeOfStrike,
    Result1, Sector, Action1, SpecificationOfAction, CompanyString1, Place,
    Occupation, Totals
  ),
  G
) :-
  % vocab:Strike
  atom_number(Local, Id),
  rdf_create_iri(resource, [strike,Local], Strike),
  rdf_assert(Strike, rdf:type, vocab:'Strike', G),
  % dc:description
  (   Description == ''
  ->  true
  ;   rdf_assert(Strike, dc:description, Description@'nl-nl', G)
  ),
  % vocab:action
  (action_iri(Action1, Action2)),
  rdf_assert(Strike, vocab:action, Action2, G),
  % vocab:character
  (   character_iri(Character1, Character2)
  ->  rdf_assert(Strike, vocab:character, Character2, G)
  ;   true
  ),
  % vocab:Strike vocab:company vocab:Company
  % vocab:Company rdfs:label rdf:langString
  forall(
    (
      atom_string(CompanyString1, CompanyString2),
      split_string(CompanyString2, ";", " ", CompanyNames),
      member(CompanyName, CompanyNames),
      \+ memberchk(CompanyName, ["","algemeen","diverse / several","onbekend"])
    ),
    (
      atomic_list_concat(CompanyNameParts, ' ', CompanyName),
      atomic_list_concat(CompanyNameParts, -, CompanyLocal),
      rdf_create_iri(resource, [company,CompanyLocal], Company),
      rdf_assert(Company, rdf:type, vocab:'Company', G),
      rdf_assert(Company, rdfs:label, CompanyName@'nl-nl', G),
      rdf_assert(Strike, vocab:company, Company, G)
    )
  ),
  % vocab:date
  (   Day == ''
  ->  (   Month == ''
      ->  rdf_assert(Strike, vocab:date, Year^^xsd:gYear, G)
      ;   rdf_assert(Strike, vocab:date, year_month(Year,Month)^^xsd:gYearMonth, G)
      )
  ;   rdf_assert(Strike, vocab:date, date(Year,Month,Day)^^xsd:date, G)
  ),
  % vocab:duration
  (   Duration == ''
  ->  true
  ;   rdf_assert(Strike, vocab:duration, Duration^^xsd:nonNegativeInteger, G)
  ),
  % vocab:id
  rdf_assert(Strike, vocab:id, Id^^xsd:string, G),
  % vocab:occupation
  (   Occupation == ''
  ->  true
  ;   rdf_assert(Strike, vocab:occupation, Occupation@'en-gb', G)
  ),
  % vocab:place
  atom_phrase(places(Strike, G), Place),
  % vocab:result
  (   result_iri(Result1, Result2)
  ->  rdf_assert(Strike, vocab:result, Result2, G)
  ;   true
  ),
  % vocab:sector
  (   Sector == ''
  ->  true
  ;   rdf_assert(Strike, vocab:sector, Sector@'en-gb', G)
  ),
  % vocab:specification-of-action
  (   SpecificationOfAction == ''
  ->  true
  ;   rdf_assert(Strike, vocab:'specification-of-action', SpecificationOfAction@'en-gb', G)
  ),
  atom_phrase(*&(total(Strike, G), "; "), Totals),
  % vocab:typeOfStrike
  (   TypeOfStrike == ''
  ->  true
  ;   rdf_assert(Strike, vocab:'type-of-strike', TypeOfStrike@'en-gb', G)
  ).

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

total(Strike, G) -->
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
  integer(N),
  {rdf_assert(Strike, P, N^^xsd:nonNegativeInteger, G)}.

places(Strike, G) -->
  *&(place, "; ", Pairs),
  {
    forall(
      member(Province-Place, Pairs),
      (
        % TBD: link to province
        rdf_assert(Strike, vocab:province, Province@'nl-nl', G),
        % TBD: link to GeoNames
        rdf_assert(Strike, vocab:place, Place@'nl-nl', G)
      )
    )
  }.

place(Province-Place) -->
  ...(Codes1),
  " ( ", !,
  ...(Codes2),
  " )", !,
  {maplist(string_codes, [Place,Province], [Codes1,Codes2])}.
