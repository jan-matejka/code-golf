Code Sample :: Encrypt plaintext in C via GnuPG Made Easy library
#################################################################

configure
#########

1. The code sample needs to run in environment with prepared .gnupg
   homedir with a gpg key. The fingerprint needs to be inserted into the
   `main.c source file`_.

2. INSERT_FAILURE can be defined in `main.c source file`_ to screw up
   the resulting decrypted plaintext to test for failure behaviour.

.. _main.c source file: ./main.c

build
#####

.. code-block:: text

    make
run
###

.. code-block:: text

    ./main
