@frontend @javascript
Feature: A user is shown whose current turn it is

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

    When "Bob" invites "Alice" to play a game
    Then "Alice" receives a game invitation from "Bob"

    When "Alice" accepts the game invitation from "Bob"
    Then "Bob" gets a game starts with "Alice" message
    And "Alice" gets a game starts with "Bob" message

    Then it is "Bob" 's turn
    And "Alice" is waiting for "Bob" 's turn

    Then "Bob" sees a pieces balance of "2/2"
    And "Alice" sees a pieces balance of "2/2"

  Scenario: Bob makes a first move and Alice replies
    When "Bob" makes a move to "3,4"
    Then "Bob" is waiting for "Alice" 's turn
    And it is "Alice" 's turn
    And "Alice" sees a "black" move to "3,4" from her opponent

    When "Alice" makes a move to "5,3"
    Then "Alice" is waiting for "Bob" 's turn
    And it is "Bob" 's turn
    And "Bob" sees a "white" move to "5,3" from his opponent

    When "Bob" makes a move to "6,4"
    Then "Bob" is waiting for "Alice" 's turn
    And it is "Alice" 's turn
    And "Alice" sees a "black" move to "6,4" from her opponent
