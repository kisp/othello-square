Given("no users are connected to the game server") do
  res = Net::HTTP.get_response(URI(SERVER_HTTP_RESET_URL))
  expect(res).to be_a Net::HTTPOK
end
