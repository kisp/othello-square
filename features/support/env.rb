require 'capybara/cucumber'
# require 'selenium-webdriver'

Capybara.register_driver :selenium_chrome_headless_docker_friendly do |app|
  Capybara::Selenium::Driver.load_selenium
  browser_options = ::Selenium::WebDriver::Chrome::Options.new
  browser_options.args << '--headless'
  browser_options.args << '--disable-gpu'
  # Sandbox cannot be used inside unprivileged Docker container
  browser_options.args << '--no-sandbox'
  Capybara::Selenium::Driver.new(app, browser: :chrome, options: browser_options)
end

Capybara.javascript_driver = :selenium_chrome_headless_docker_friendly

# # caps = Selenium::WebDriver::Remote::Capabilities.chrome
# # caps[:platform] = 'Windows 10'
# # caps[:version] = '92'
# # caps[:build] = my_test_build
# # caps[:name] = my_test_name
# # driver = Selenium::WebDriver.for :remote, url: cloud_url, desired_capabilities: caps



# Capybara.register_driver(:headless_chrome) do |app|
#   capabilities = Selenium::WebDriver::Options.chrome(
#     # Add 'no-sandbox' arg if you have an "unknown error: Chrome failed to start: exited abnormally"
#     # @see https://github.com/SeleniumHQ/selenium/issues/4961
#     chromeOptions: { args: %w[headless disable-gpu no-sandbox] }
#   )

#   Capybara::Selenium::Driver.new(app, browser: :chrome, desired_capabilities: capabilities)
# end

# Capybara.javascript_driver = :headless_chrome

# # Capybara.javascript_driver = :selenium_chrome
# # Capybara.javascript_driver = :selenium_chrome_headless
