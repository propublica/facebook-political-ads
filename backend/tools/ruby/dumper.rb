require 'bundler'
Bundler.require

conn = PG.connect(:dbname => 'facebook_ads_germany')

res = conn.exec("select html from ads")

puts res.map { |r| Oga.parse_html(r["html"]).children.text }.to_json
