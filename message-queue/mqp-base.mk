# Base GNU makefile for building individual implementations and
# their container images and container.
.DEFAULT_TARGET = build
CONTAINERFILE := Containerfile
COMMON_NAME := mq-producer
NAME := $(shell basename $(CURDIR))
FULL_NAME := $(COMMON_NAME)-$(NAME)

.PHONY: image
image: ## Build image

	# Need to build with parent as context due to some shared code like this makefile
	podman build -f $(CONTAINERFILE) -t localhost/$(FULL_NAME) ..

.PHONY: container
container: ## Run the image in container

	podman rm $(FULL_NAME) || true
	# Run with --network=host for now.
	# Option --userns and --pod are mutually exlusive.
	# TBD Maybe its possible to run the entire pod with --userns.
	podman run \
		--name $(FULL_NAME) \
		--userns keep-id \
		-v $(realpath .):/home/user/mq \
		-u user \
		--network=host \
		-it \
		$(FULL_NAME) \
		zsh

.PHONY: exec-root
exec-root: ## Exec into running container as root

	podman exec -u root -it $(FULL_NAME) bash

.PHONY: help
help: ## Print help

	@@grep -h '^\([a-zA-Z]\+\):' $(MAKEFILE_LIST) | \
		sort | \
		awk -F ':.*?## ' 'NF==2 {printf "  %-26s%s\n", $$1, $$2}'
