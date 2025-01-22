include ../../help.mk
# Base GNU makefile for building message producers, and their container
# images and providing container helpers.
.DEFAULT_GOAL = build
CONTAINERFILE := Containerfile
COMMON_NAME := mq-producer
NAME := $(shell basename $(CURDIR))
FULL_NAME := $(COMMON_NAME)-$(NAME)

compose = podman-compose -f ../compose.yaml -p code-golf_message-queue $1

.PHONY: image
image: dev-image ## Build image

	# Build the images separately because podman-compose-build runs the builds in
	# parallel, even tho one image depends on the other. Which is weird.
	# Also the output is unreadable since it jumbles the outputs of both builds
	# without any indication of which image the outputs is from.
	# Now also because we want check-container to depend on the dev-image only
	$(call compose,build producer-$(NAME))

.PHONY: dev-image
dev-image: ## Build dev image

	$(call compose,build producer-$(NAME)-dev)

.PHONY: container
container: ## Run the image in container

	$(call compose,run --rm producer-$(NAME)-dev)

.PHONY: container-check
# Run image build manually. Do not add as container-check dependency. Even full
# cache takes relatively long and is mostly unnecessary.
container-check: ## Run make check inside container

	$(call compose,run --rm $(CONTAINER_CHECK_COMPOSE_ARGS) \
		producer-$(NAME)-dev -c 'make check')

.PHONY: exec-root
exec-root: ## Exec into running container as root

	podman exec -u root -it $(FULL_NAME) bash
