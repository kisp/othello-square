@backend @em
Feature: A user invites another user for a game

  Background:
    Given no users are connected to the game server
    Given "Jane" is connected and logged in to the game server
    And "Bob" is connected and logged in to the game server
    When "Bob" asks the server to get a list of users
    Then "Bob" can see that "Jane" is there
    When "Jane" asks the server to get a list of users
    Then "Jane" can see that "Bob" is there

  Scenario: Jane receives a game invitation from Bob
    When "Bob" invites "Jane" to play a game
    Then "Jane" receives a game invitation from "Bob"
    When "Jane" accepts the game invitation from "Bob"
    Then "Bob" gets a game starts with "Jane" message
    And "Jane" gets a game starts with "Bob" message

    Then it is "Bob" 's turn
    And "Jane" is waiting for "Bob" 's turn
