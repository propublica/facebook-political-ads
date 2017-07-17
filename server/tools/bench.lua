counter = 1

request = function()
  counter = counter + 1
  wrk.method = "POST"
  path = "/facebook-ads/ads"
  wrk.headers["Content-Type"] = "application/x-www-form-urlencoded"
  wrk.body = '{"id":"test' .. counter .. '", "html":"<div></div>", "political":true}'
  return wrk.format(nil, path)
end
