#######################
(not quite a) Code Golf
#######################

My personal code golf repository that's not quite a code golf.

My goal here is to implement programs in the shortest possible source code while adhering to the
current best practices.

The purpose of these goals are several:

1. Learn languages and runtimes.

2. Learn related tooling (build, testing, linting, ci, containerization, etc).

3. Implement a `MCREs <https://stackoverflow.com/help/minimal-reproducible-example>`_ for reference
   and possibly benchmarking.

4. Learn 3rd party services such as databases, logging systems, monitoring systems, etc.


Projects
########

- `message-queue/ <./message-queue>`_

    Benchmarks of various languages, IO systems, and message queue systems (aspirational).

    |mq-python CI| |mq-rust CI| |mq-golang CI| |mq-haskell CI| |mq-cpp CI|

.. |mq-python CI| image:: https://github.com/jan-matejka/code-golf/actions/workflows/mq-python.yaml/badge.svg
   :target: https://github.com/jan-matejka/code-golf/actions/workflows/mq-python.yaml
   :alt: mq-python CI

.. |mq-cpp CI| image:: https://github.com/jan-matejka/code-golf/actions/workflows/mq-cpp.yaml/badge.svg
   :target: https://github.com/jan-matejka/code-golf/actions/workflows/mq-cpp.yaml
   :alt: mq-cpp CI

.. |mq-haskell CI| image:: https://github.com/jan-matejka/code-golf/actions/workflows/mq-haskell.yaml/badge.svg
   :target: https://github.com/jan-matejka/code-golf/actions/workflows/mq-haskell.yaml
   :alt: mq-haskell CI

.. |mq-golang CI| image:: https://github.com/jan-matejka/code-golf/actions/workflows/mq-golang.yaml/badge.svg
   :target: https://github.com/jan-matejka/code-golf/actions/workflows/mq-golang.yaml
   :alt: mq-golang CI

.. |mq-rust CI| image:: https://github.com/jan-matejka/code-golf/actions/workflows/mq-rust.yaml/badge.svg
   :target: https://github.com/jan-matejka/code-golf/actions/workflows/mq-rust.yaml
   :alt: mq-rust CI

- `5.connection-counter <./5.connection-counter>`_

    MCRE for various methods of managing state in haskell language.

- `3.cmp <./3.cmp>`_

    Basic implementation of ``/usr/bin/cmp`` in Netwide Assembler.

- The rest.

    Generally uninteresting.
