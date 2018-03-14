for langCode in "de-AT" "en-CA" "nl-BE" "da-DK" "en-AU" "it-IT" "de-DE" "en-US" "fr-CA" "sv-SE" "fi-FI" "de-CH" "nl-NL"; do
  psql "${DATABASE_URL}" -c "\COPY (SELECT * FROM ads WHERE lang = '$langCode' AND political_probability > 0.7) to '$langCode.csv' with csv header"
  aws s3 cp $langCode.csv s3://pp-projects-static/fbpac/ # --public-read
done
