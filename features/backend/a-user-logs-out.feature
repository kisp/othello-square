@backend @em
Feature: A user logs out

  Background:
    Given no users are connected to the game server
    Given "Jane" is connected and logged in to the game server
    And "Bob" is connected and logged in to the game server
    When "Bob" asks the server to get a list of users
    Then "Bob" can see that "Jane" is there
    When "Jane" asks the server to get a list of users
    Then "Jane" can see that "Bob" is there

  Scenario: Jane logs out and Bob sees that Jane is not online
    When "Jane" logs out
    Then "Bob" can see that "Jane" is not there
