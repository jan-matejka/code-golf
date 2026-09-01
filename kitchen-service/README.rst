###############
Kitchen Service
###############

A model kitchen service.

v0.1 - Basic Idea
=================

- Customers talk to Servers to order menu items.
- Servers pass the order as a ticket to the kitchen.
- Cooks prepare the ticket and notify the server.
- Server delivers the items to customers.
- 1 cook, 1 server, 3 customers, 3 menu items may be good numbers to start
  with.

v0.2
====

- Ability to run with N of everything (cooks, servers, customers, menu items).

v0.3
====

- Some kind of visualization would be nice.

  - So grafana and prometheus integration I guess.
  - Maybe structlog and loki as well?

v0.4
====

- Add kitchen constraint.

  - Kitchen has fixed amount of tools that has to be acquired by cooks in order
    for them to be able to prepare ordered items (resolve tickets).
  - Kitchen has an infinite amount of ingredients.
  - TBD: We have to define some tools and what menu items need those tools.
    Something very simple to start with.
  - (Cook + ingredient + tools* + time) produces a menu-item
