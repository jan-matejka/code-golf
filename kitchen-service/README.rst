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

TBD
===

1. What determines whether a table is served by a single server or any server?

   It comes down to how the restaurant structures its floor plan and
   service model -- there isn't one universal rule, but a few common
   systems:

   1. Sectioning (most common -- one server owns a set of tables). The
      floor is divided into stations/sections (e.g., tables 10-15), and
      each server is assigned a section for their shift. Every table in
      that section is served exclusively by that server -- this is the
      standard model in most sit-down restaurants, since it ties
      accountability (and tips) to a single person and keeps the ticket
      tied to one name for tracking.

   2. Team/zone service (a subset of servers). In fine dining or
      high-volume restaurants, a team of 2-4 servers (sometimes a captain
      + back waiter + runner) covers a larger zone together. Any of them
      might greet, take orders, or deliver food to any table in that zone
      -- this smooths coverage during rushes and breaks, since no single
      person is a bottleneck.

   3. Any server (food runners / expediting). Regardless of sectioning,
      food running is often decoupled from service ownership -- kitchen
      tickets get expedited ("fired") at the pass, and any available
      runner or server may carry the food out, since the physical act of
      delivery doesn't require the guest relationship. The ticket still
      shows which server "owns" the table for billing/tips, even if
      someone else drops the plate.

   4. Rotation/seating assignment. The host/hostess typically decides who
      gets the next table by a rotation system ("next up" seating),
      balancing sections so no one server gets overloaded -- this
      determines which server owns a table before the meal even starts,
      rather than servers choosing tables themselves.

   5. Exceptions -- large parties/VIPs. Big groups or VIP tables sometimes
      get a dedicated server or team assigned outside the normal rotation,
      since a single section-server can't handle the volume alone.

   Bottom line: ownership of a table (whose ticket it is, who gets the tip)
   is almost always tied to section assignment, decided at seating time --
   but the physical act of serving/running food can be done by anyone
   available, especially during a rush, without changing who the ticket
   belongs to.
