#############
Message Queue
#############

|mq-python CI| |mq-rust CI| |mq-golang CI| |mq-haskell CI| |mq-cpp CI|

.. |mq-python CI| image:: https://github.com/jan-matejka/code-golf/actions/workflows/mq-python.yaml/badge.svg
   :target: https://github.com/jan-matejka/code-golf/tree/master/message-queue/python
   :alt: mq-python CI

.. |mq-cpp CI| image:: https://github.com/jan-matejka/code-golf/actions/workflows/mq-cpp.yaml/badge.svg
   :target: https://github.com/jan-matejka/code-golf/tree/master/message-queue/cpp
   :alt: mq-cpp CI

.. |mq-haskell CI| image:: https://github.com/jan-matejka/code-golf/actions/workflows/mq-haskell.yaml/badge.svg
   :target: https://github.com/jan-matejka/code-golf/tree/master/message-queue/haskell
   :alt: mq-haskell CI

.. |mq-golang CI| image:: https://github.com/jan-matejka/code-golf/actions/workflows/mq-golang.yaml/badge.svg
   :target: https://github.com/jan-matejka/code-golf/tree/master/golang
   :alt: mq-golang CI

.. |mq-rust CI| image:: https://github.com/jan-matejka/code-golf/actions/workflows/mq-rust.yaml/badge.svg
   :target: https://github.com/jan-matejka/code-golf/tree/master/rust
   :alt: mq-rust CI

.. image:: ./assets/overview.png

Dependencies
############

- Podman >= 4.3.1
- Podman-compose >= 1.2.0
- GNU make

Build
#####

1. Build all the container images (this will take a while):

    ``$ make build``

2. Start infrastructure:

    ``$ make infra-up``

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
