@em
Feature: A user invites another user for a game

  Background:
    Given no users are connected to the game server
    Given "jane" is connected and logged in to the game server
    And "bob" is connected and logged in to the game server
    When "bob" asks the server to get a list of users
    Then "bob" can see that "jane" is there
    When "jane" asks the server to get a list of users
    Then "jane" can see that "bob" is there

  Scenario: jane receives a game invitation from bob
    When "bob" invites "jane" to play a game
    Then "jane" receives a game invitation from "bob"
    When "jane" accepts the game invitation from "bob"
    Then "bob" gets a game starts with "jane" message
    And "jane" gets a game starts with "bob" message
