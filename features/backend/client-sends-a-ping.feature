@backend @em
Feature: Client sends a ping

  As a user connecting with my client browser to the deployed app
  I want the server to be able to play ping/pong with messages
  So that my client can send ping messages every 30s and the proxy of
  the deployed app will not reach its timeout of 60s to close idle TCP
  connections

  Background:
    Given no users are connected to the game server
    And "Jane" has opened a connection to the game server

  Scenario: A ping message gets a pong reply
    When "Jane" sends a ping message
    Then "Jane" receives a pong message
