@javascript
Feature: A user visits the site and is prompted for her name
  As an othello player
  I want to login to the site with my nickname
  So that other players can see that I am online and invite me to play

  Scenario: Login as Peter
    Given that I visit the homepage
    And I am asked to login with my nickname
    When I login as "Peter"
    Then I see the welcome message "Welcome, Peter!"

  Scenario: Login as Jane
    Given that I visit the homepage
    And I am asked to login with my nickname
    When I login as "Jane"
    Then I see the welcome message "Welcome, Jane!"

  Scenario: Login as Peter by hitting enter
    Given that I visit the homepage
    And I am asked to login with my nickname
    When I login as "Peter" by hitting enter
    Then I see the welcome message "Welcome, Peter!"

  Scenario: Login with an invalid name (empty string)
    Given that I visit the homepage
    And I am asked to login with my nickname
    When I login as ""
    Then I am asked to login with my nickname

  Scenario: Login with an invalid name (only whitespace)
    Given that I visit the homepage
    And I am asked to login with my nickname
    When I login as "   "
    Then I am asked to login with my nickname
