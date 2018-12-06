for langCode in "en-US"; do
# for langCode in "de-AT" "en-CA" "nl-BE" "da-DK" "en-AU" "it-IT" "de-DE" "en-US" "fr-CA" "sv-SE" "fi-FI" "de-CH" "nl-NL"; do
  psql "${DATABASE_URL}" -c "\COPY (SELECT * FROM ads WHERE lang = '$langCode' AND (paid_for_by is not null or political_probability > 0.7) AND suppressed=false) to '$langCode.csv' with csv header"
  aws s3 cp --acl public-read $langCode.csv s3://pp-projects-static/fbpac/ 
  aws s3 cp --acl public-read $langCode.csv s3://data-publica/fbpac-ads-$langCode.csv
done
