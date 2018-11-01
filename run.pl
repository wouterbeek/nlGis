:- module(nlgis, [run/0]).

/** <module> nlGIS

@author Wouter Beek
@version 2017/10-2017/12
*/

:- use_module(library(tapir/tapir_api)).

run :-
  Properties = _{
    accessLevel: public,
    avatar: 'avatar.svg',
    imports: [dataLegend-[cshapes,gemeentegeschiedenis,mint]],
    prefixes: [
      amco-'http://www.gemeentegeschiedenis.nl/amco/',
      authority-'https://iisg.amsterdam/resource/authority/',
      bnode-'https://iisg.amsterdam/.well-known/genid/',
      capital-'https://iisg.amsterdam/resource/capital/',
      cbs-'http://www.gemeentegeschiedenis.nl/cbscode/',
      country-'https://iisg.amsterdam/resource/country/',
      dataset-'https://iisg.amsterdam/dataset/',
      departement-'http://www.gemeentegeschiedenis.nl/departement/',
      fabio,
      'gg-graph'-'http://www.gemeentegeschiedenis.nl/graph/',
      'gg-vocab'-'http://www.gemeentegeschiedenis.nl/gg-schema#',
      graph-'https://iisg.amsterdam/graph/',
      house-'https://iisg.amsterdam/resource/house/',
      municipality-'http://www.gemeentegeschiedenis.nl/gemeentenaam/',
      orcid,
      province-'http://www.gemeentegeschiedenis.nl/provincie/',
      rel,
      sh,
      vocab-'https://iisg.amsterdam/vocab/'
    ]
  },
  dataset_upload(nlgis, Properties),
  halt.
