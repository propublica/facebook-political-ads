/usr/local/bin/pipenv run ./classify build
for langCode in "es-ES" "de-AT" "en-CA" "nl-BE" "da-DK" "en-AU" "it-IT" "de-DE" "en-US" "fr-CA" "sv-SE" "fi-FI" "de-CH" "nl-NL"; do
	aws s3 cp --acl public-read data/$langCode/classifier.dill s3://pp-data/fbpac-models/$langCode/classifier.dill
done

