require 'rubygems'
require 'bundler'

Bundler.require

require 'koala'
require 'json'

g = Koala::Facebook::API.new(ENV["ACCESS_TOKEN"])

a = $stdin.each_line.map do |it|
  g.get_connections(it.chomp, "posts").map { |i| i["message"] }
end.flatten.reject(&:nil?)

puts a.to_json
