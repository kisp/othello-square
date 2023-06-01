@frontend @javascript
Feature: Two users login to the site and see each other online

  Background:
    Given no users are connected to the game server

  Scenario: Login as Peter and Bob
    Given that I visit the homepage
    And that "Bob" visits the homepage

    And I am asked to login with my nickname
    And "Bob" is asked to login with his nickname

    When I login as "Peter"
    Then I see the welcome message "Welcome, Peter!"

    When "Bob" logs in with his nickname
    Then "Bob" sees the welcome message "Welcome, Bob!"

    Then I can see that "Bob" is there
    Then "Bob" can see that "Peter" is there

  Scenario: Login as Peter, Alice, and Jane
    Given that I visit the homepage
    And I am asked to login with my nickname
    When I login as "Peter"
    Then I see the welcome message "Welcome, Peter!"

    Given that "Alice" visits the homepage
    And "Alice" is asked to login with her nickname
    When "Alice" logs in with her nickname
    Then "Alice" sees the welcome message "Welcome, Alice!"

    Given that "Jane" visits the homepage
    And "Jane" is asked to login with her nickname
    When "Jane" logs in with her nickname
    Then "Jane" sees the welcome message "Welcome, Jane!"

    Then I can see that "Alice" is there
    Then I can see that "Jane" is there

    Then "Alice" can see that "Peter" is there
    Then "Alice" can see that "Jane" is there

    Then "Jane" can see that "Peter" is there
    Then "Jane" can see that "Alice" is there
