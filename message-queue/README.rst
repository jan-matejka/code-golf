#############
Message Queue
#############

.. image:: ./assets/overview.png

Build
#####

1. ``$ make build`` to build the infra containers with podman

2. ``$ make pod`` to run the required infra in a pod with port forwarding from the host.

3.  ``cd <lang> && make && make check`` to build message producers.

    c++, go, and rust can be built localy on debian stable.

    python and haskell are containerized to be built with ``make image`` and ran inside ``make
    container``.

    Refer to ``make help`` for more.

4. Run the message producers ``./main``.

   Sometimes you need to locate the binary first depending on build system (e.g. haskell, rust).

   Note not all implementation support the metric visualization yet (see https://github.com/jan-matejka/code-golf/issues/6).

5. Open grafana at http://localhost:3000.

   Login as admin/admin.

   Navigate to the overview dashboard.
