@frontend @javascript
Feature: A user closing the browser window should not prevent other users from logging in

  Background:
    Given no users are connected to the game server

    Given that "Alice" visits the homepage
    And "Alice" is asked to login with her nickname
    When "Alice" logs in with her nickname
    Then "Alice" sees the welcome message "Welcome, Alice!"

    Given that "Bob" visits the homepage
    And "Bob" is asked to login with his nickname

  Scenario: Alice logs in and closes the connection then Bob comes and can log in
    When "Alice" closes the browser window

    Then "Bob" logs in with his nickname
    And "Bob" sees the welcome message "Welcome, Bob!"
