#############
Message Queue
#############

.. image:: ./assets/overview.png

Dependencies
############

- Podman >= 4.3.1
- Podman-compose >= 1.2.0
- GNU make

Build
#####

1. Build all the container images (this will take a while):

    ``$ podman-compose -p code-golf_message-queue build``

2. Start infrastructure:

    ``$ podman-compose up -d infra``

3. Run message proucers:

    ``$ make run-producers``

4. Open grafana at http://localhost:3000. Login as admin/admin. Navigate to the overview dashboard.

Dev Notes
#########

- Producers have -dev versions of the container for development with sources bind mounted.

- All the containers run on host network (to support --userns for -dev containers) so ports need to
  be available.

- Some containers assume the user's id=1000 (namely grafana at the moment) for uid maps.

Example
#######

Runtimes overview from my workstation:

.. image:: ./assets/runtimes-overview.png
