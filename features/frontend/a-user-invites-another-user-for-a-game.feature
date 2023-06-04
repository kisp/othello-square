@frontend @javascript
Feature: A user invites another user for a game

  Background:
    Given no users are connected to the game server
    Given that "Alice" visits the homepage
    And that "Bob" visits the homepage

    And "Alice" is asked to login with her nickname
    And "Bob" is asked to login with his nickname

    When "Alice" logs in with her nickname
    Then "Alice" sees the welcome message "Welcome, Alice!"

    When "Bob" logs in with his nickname
    Then "Bob" sees the welcome message "Welcome, Bob!"

    Then "Alice" can see that "Bob" is there
    Then "Bob" can see that "Alice" is there

  Scenario: Alice receives a game invitation from Bob
    When "Bob" invites "Alice" to play a game
    Then "Alice" receives a game invitation from "Bob"

    When "Alice" accepts the game invitation from "Bob"
    Then "Bob" gets a game starts with "Alice" message
    And "Alice" gets a game starts with "Bob" message

    Then it is "Bob" 's turn
    And "Alice" is waiting for "Bob" 's turn
