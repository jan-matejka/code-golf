.PHONY: build
build: clean

	gcc main.c $(shell gpgme-config --libs) -Wall -o main

.PHONY: clean
clean:

	$(RM) main result result.*
