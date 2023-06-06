@frontend @javascript
Feature: A user accepts a game invitation and is then listed as currently playing

  Background:
    Given no users are connected to the game server

    Given that "Jane" visits the homepage
    And "Jane" is asked to login with her nickname
    When "Jane" logs in with her nickname
    Then "Jane" sees the welcome message "Welcome, Jane!"

    Given that "Other_user" visits the homepage
    And "Other_user" is asked to login with her nickname
    When "Other_user" logs in with her nickname
    Then "Other_user" sees the welcome message "Welcome, Other_user!"

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

  Scenario: Jane can see that Alice and Bob play with each other
    Given "Jane" can see that "Bob" is there
    And "Jane" can see that "Bob" is currently not playing
    When "Alice" accepts the game invitation from "Bob"
    Then "Jane" can see that "Bob" is currently playing with "Alice"
    And "Jane" can see that "Alice" is currently playing with "Bob"
    And "Jane" can see that "Other_user" is currently not playing
