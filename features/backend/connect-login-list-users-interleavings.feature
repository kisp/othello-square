@backend @em
Feature: Connect / login / list users interleavings

  Scenario: (b1 b2 b3 jA jB jC)
    Given no users are connected to the game server
    When "bob" has opened a connection to the game server
    When "bob" logs in with her username
    When "bob" asks the server to get a list of users
    When "jane" has opened a connection to the game server
    When "jane" logs in with her username
    When "jane" asks the server to get a list of users
    Then "bob" can see that "jane" is there
    Then "jane" can see that "bob" is there

  Scenario: (b1 b2 jA b3 jB jC)
    Given no users are connected to the game server
    When "bob" has opened a connection to the game server
    When "bob" logs in with her username
    When "jane" has opened a connection to the game server
    When "bob" asks the server to get a list of users
    When "jane" logs in with her username
    When "jane" asks the server to get a list of users
    Then "bob" can see that "jane" is there
    Then "jane" can see that "bob" is there

  Scenario: (b1 b2 jA jB b3 jC)
    Given no users are connected to the game server
    When "bob" has opened a connection to the game server
    When "bob" logs in with her username
    When "jane" has opened a connection to the game server
    When "jane" logs in with her username
    When "bob" asks the server to get a list of users
    When "jane" asks the server to get a list of users
    Then "bob" can see that "jane" is there
    Then "jane" can see that "bob" is there

  Scenario: (b1 b2 jA jB jC b3)
    Given no users are connected to the game server
    When "bob" has opened a connection to the game server
    When "bob" logs in with her username
    When "jane" has opened a connection to the game server
    When "jane" logs in with her username
    When "jane" asks the server to get a list of users
    When "bob" asks the server to get a list of users
    Then "bob" can see that "jane" is there
    Then "jane" can see that "bob" is there

  Scenario: (b1 jA b2 b3 jB jC)
    Given no users are connected to the game server
    When "bob" has opened a connection to the game server
    When "jane" has opened a connection to the game server
    When "bob" logs in with her username
    When "bob" asks the server to get a list of users
    When "jane" logs in with her username
    When "jane" asks the server to get a list of users
    Then "bob" can see that "jane" is there
    Then "jane" can see that "bob" is there

  Scenario: (b1 jA b2 jB b3 jC)
    Given no users are connected to the game server
    When "bob" has opened a connection to the game server
    When "jane" has opened a connection to the game server
    When "bob" logs in with her username
    When "jane" logs in with her username
    When "bob" asks the server to get a list of users
    When "jane" asks the server to get a list of users
    Then "bob" can see that "jane" is there
    Then "jane" can see that "bob" is there

  Scenario: (b1 jA b2 jB jC b3)
    Given no users are connected to the game server
    When "bob" has opened a connection to the game server
    When "jane" has opened a connection to the game server
    When "bob" logs in with her username
    When "jane" logs in with her username
    When "jane" asks the server to get a list of users
    When "bob" asks the server to get a list of users
    Then "bob" can see that "jane" is there
    Then "jane" can see that "bob" is there

  Scenario: (b1 jA jB b2 b3 jC)
    Given no users are connected to the game server
    When "bob" has opened a connection to the game server
    When "jane" has opened a connection to the game server
    When "jane" logs in with her username
    When "bob" logs in with her username
    When "bob" asks the server to get a list of users
    When "jane" asks the server to get a list of users
    Then "bob" can see that "jane" is there
    Then "jane" can see that "bob" is there

  Scenario: (b1 jA jB b2 jC b3)
    Given no users are connected to the game server
    When "bob" has opened a connection to the game server
    When "jane" has opened a connection to the game server
    When "jane" logs in with her username
    When "bob" logs in with her username
    When "jane" asks the server to get a list of users
    When "bob" asks the server to get a list of users
    Then "bob" can see that "jane" is there
    Then "jane" can see that "bob" is there

  Scenario: (b1 jA jB jC b2 b3)
    Given no users are connected to the game server
    When "bob" has opened a connection to the game server
    When "jane" has opened a connection to the game server
    When "jane" logs in with her username
    When "jane" asks the server to get a list of users
    When "bob" logs in with her username
    When "bob" asks the server to get a list of users
    Then "bob" can see that "jane" is there
    Then "jane" can see that "bob" is there

  Scenario: (jA b1 b2 b3 jB jC)
    Given no users are connected to the game server
    When "jane" has opened a connection to the game server
    When "bob" has opened a connection to the game server
    When "bob" logs in with her username
    When "bob" asks the server to get a list of users
    When "jane" logs in with her username
    When "jane" asks the server to get a list of users
    Then "bob" can see that "jane" is there
    Then "jane" can see that "bob" is there

  Scenario: (jA b1 b2 jB b3 jC)
    Given no users are connected to the game server
    When "jane" has opened a connection to the game server
    When "bob" has opened a connection to the game server
    When "bob" logs in with her username
    When "jane" logs in with her username
    When "bob" asks the server to get a list of users
    When "jane" asks the server to get a list of users
    Then "bob" can see that "jane" is there
    Then "jane" can see that "bob" is there

  Scenario: (jA b1 b2 jB jC b3)
    Given no users are connected to the game server
    When "jane" has opened a connection to the game server
    When "bob" has opened a connection to the game server
    When "bob" logs in with her username
    When "jane" logs in with her username
    When "jane" asks the server to get a list of users
    When "bob" asks the server to get a list of users
    Then "bob" can see that "jane" is there
    Then "jane" can see that "bob" is there

  Scenario: (jA b1 jB b2 b3 jC)
    Given no users are connected to the game server
    When "jane" has opened a connection to the game server
    When "bob" has opened a connection to the game server
    When "jane" logs in with her username
    When "bob" logs in with her username
    When "bob" asks the server to get a list of users
    When "jane" asks the server to get a list of users
    Then "bob" can see that "jane" is there
    Then "jane" can see that "bob" is there

  Scenario: (jA b1 jB b2 jC b3)
    Given no users are connected to the game server
    When "jane" has opened a connection to the game server
    When "bob" has opened a connection to the game server
    When "jane" logs in with her username
    When "bob" logs in with her username
    When "jane" asks the server to get a list of users
    When "bob" asks the server to get a list of users
    Then "bob" can see that "jane" is there
    Then "jane" can see that "bob" is there

  Scenario: (jA b1 jB jC b2 b3)
    Given no users are connected to the game server
    When "jane" has opened a connection to the game server
    When "bob" has opened a connection to the game server
    When "jane" logs in with her username
    When "jane" asks the server to get a list of users
    When "bob" logs in with her username
    When "bob" asks the server to get a list of users
    Then "bob" can see that "jane" is there
    Then "jane" can see that "bob" is there

  Scenario: (jA jB b1 b2 b3 jC)
    Given no users are connected to the game server
    When "jane" has opened a connection to the game server
    When "jane" logs in with her username
    When "bob" has opened a connection to the game server
    When "bob" logs in with her username
    When "bob" asks the server to get a list of users
    When "jane" asks the server to get a list of users
    Then "bob" can see that "jane" is there
    Then "jane" can see that "bob" is there

  Scenario: (jA jB b1 b2 jC b3)
    Given no users are connected to the game server
    When "jane" has opened a connection to the game server
    When "jane" logs in with her username
    When "bob" has opened a connection to the game server
    When "bob" logs in with her username
    When "jane" asks the server to get a list of users
    When "bob" asks the server to get a list of users
    Then "bob" can see that "jane" is there
    Then "jane" can see that "bob" is there

  Scenario: (jA jB b1 jC b2 b3)
    Given no users are connected to the game server
    When "jane" has opened a connection to the game server
    When "jane" logs in with her username
    When "bob" has opened a connection to the game server
    When "jane" asks the server to get a list of users
    When "bob" logs in with her username
    When "bob" asks the server to get a list of users
    Then "bob" can see that "jane" is there
    Then "jane" can see that "bob" is there

  Scenario: (jA jB jC b1 b2 b3)
    Given no users are connected to the game server
    When "jane" has opened a connection to the game server
    When "jane" logs in with her username
    When "jane" asks the server to get a list of users
    When "bob" has opened a connection to the game server
    When "bob" logs in with her username
    When "bob" asks the server to get a list of users
    Then "bob" can see that "jane" is there
    Then "jane" can see that "bob" is there
