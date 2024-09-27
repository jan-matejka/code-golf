.PHONY: build
build:

	cabal build

.PHONY: cbuild
cbuild:

	podman rm mq-producer-haskell-build
	podman run --name mq-producer-haskell-build --userns keep-id -v $(realpath ../..):/home/user/code-golf -u user -it mq-producer-haskell stack build

.PHONY: clean
clean:

	cabal clean

.PHONY: build-container
build-container:

	podman build -f Containerfile -t localhost/mq-producer-haskell

.PHONY: run-container
run-container:

	podman rm mq-producer-haskell
	podman run --name mq-producer-haskell --userns keep-id -v $(realpath ../..):/home/user/code-golf  -u user -it mq-producer-haskell bash

.PHONY: exec-root
exec-root:

	podman exec -u root -it stack bash
