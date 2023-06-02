@frontend @javascript
Feature: A user visits the site and is prompted for her name
  As an othello player
  I want to login to the site with my nickname
  So that other players can see that I am online and invite me to play

  Background:
    Given no users are connected to the game server
    Given that I visit the homepage
    And I am asked to login with my nickname

  Scenario: Login as Peter
    When I login as "Peter"
    Then I see the welcome message "Welcome, Peter!"

  Scenario: Login as Jane
    When I login as "Jane"
    Then I see the welcome message "Welcome, Jane!"

  Scenario: Login as Peter by hitting enter
    When I login as "Peter" by hitting enter
    Then I see the welcome message "Welcome, Peter!"

  Scenario: Login with an invalid name (empty string)
    When I login as ""
    Then I am asked to login with my nickname

  Scenario: Login with an invalid name (only whitespace)
    When I login as "   "
    Then I am asked to login with my nickname
