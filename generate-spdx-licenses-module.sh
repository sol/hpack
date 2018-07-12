#!/bin/bash

set -o nounset
set -o errexit

exec > "src/Hpack/SpdxLicenses.hs"

echo '-- DO NOT MODIFY MANUALLY'
echo '--'
echo "-- This file has been generated with $0."
echo
echo '{-# LANGUAGE OverloadedStrings #-}'
echo 'module Hpack.SpdxLicenses where'
echo 'import Data.Text (Text)'
echo 'import Distribution.SPDX.LicenseId'

first=true

echo 'licenses :: [(LicenseId, Text)]'
echo 'licenses = ['
for license in spdx-licenses/*.txt; do
  file=$(basename "$license")
  license_id=${file%.*}
  license_id_constructor=${license_id//-/_}
  $first && echo -n "    " || echo -n "  , "
  first=false
  echo -n "($license_id_constructor, "
  echo '"\'
  while IFS='' read -r line || [[ -n "$line" ]]; do
    echo "\\${line//\"/\\\"}\\n\\"
  done < $license
  echo '\")'
done
echo '  ]'
