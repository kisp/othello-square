@backend @em
Feature: Two users can see each other

  Background:
    Given no users are connected to the game server
    Given "jane" is connected and logged in to the game server
    And "bob" is connected and logged in to the game server

  Scenario: bob sees jane
    When "bob" asks the server to get a list of users
    Then "bob" can see that "jane" is there

  Scenario: jane sees bob
    When "jane" asks the server to get a list of users
    Then "jane" can see that "bob" is there
