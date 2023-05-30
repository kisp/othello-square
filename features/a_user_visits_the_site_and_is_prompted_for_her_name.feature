Feature: A user visits the site and is prompted for her name
  As an othello player
  I want to login to the site with my nickname
  So that other players can see that I am online and invite me to play

  @javascript
  Scenario: Login as Peter
    Given that I visit the homepage
    And I am asked to login with my nickname
    When I login as "Peter"
    Then I see the welcome message "Welcome, Peter!"
