#!/usr/bin/env bash

CCONVERT="iconv -f iso-8859-1 -t utf-8"
CPARSE="stack exec IMDBhs-exe"
CCOUNT_YEARS="cut -f2 | sort | uniq -c | awk -v OFS="\," '{print \$2, \$1, sum+=\$1}'"

movies=$1

echo $CCOUNT_YEARS

eval $CCONVERT $movies | eval $CPARSE | eval $CCOUNT_YEARS
