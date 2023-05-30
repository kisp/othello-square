require "capybara/cucumber"

# from
# Running Chrome Headless, Selenium and Capybara inside GitLab CI and Docker
# https://blog.phusion.nl/2018/05/24/using-chrome-headless-selenium-and-capybara-inside-gitlab-runner-and-docker/

Capybara.register_driver :selenium_chrome_headless_docker_friendly do |app|
  Capybara::Selenium::Driver.load_selenium
  browser_options = ::Selenium::WebDriver::Chrome::Options.new
  browser_options.args << "--headless"
  browser_options.args << "--disable-gpu"

  # Sandbox cannot be used inside unprivileged Docker container
  browser_options.args << "--no-sandbox"
  Capybara::Selenium::Driver.new(
    app,
    browser: :chrome,
    options: browser_options,
  )
end

if ENV["GITLAB_CI"]
  Capybara.javascript_driver = :selenium_chrome_headless_docker_friendly
elsif ENV["HEADLESS"]&.downcase == "true"
  Capybara.javascript_driver = :selenium_chrome_headless
else
  Capybara.javascript_driver = :selenium_chrome
end

puts "Capybara.javascript_driver set to #{Capybara.javascript_driver.inspect}"
