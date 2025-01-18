compose_compose=podman-compose -p $(COMPOSE_PROJECT) $1

.PHONY: compose-$(COMPOSE_SERVICE)-up
compose-$(COMPOSE_SERVICE)-up: ## Start $* containers

	$(call compose_compose,up -d $(COMPOSE_SERVICE))

.PHONY: compose-$(COMPOSE_SERVICE)-down
compose-$(COMPOSE_SERVICE)-down: ## Stop $* containers

	$(call compose_compose,down $(COMPOSE_SERVICE))

.PHONY: $(COMPOSE_SERVICE)-up
$(COMPOSE_SERVICE)-up: compose-before_up compose-$(COMPOSE_SERVICE)-up ## Start $* containers

.PHONY: $(COMPOSE_SERVICE)-down
$(COMPOSE_SERVICE)-down: compose-$(COMPOSE_SERVICE)-down compose-after_down ## Stop $* containers

.PHONY: compose-before_up
compose-before_up:

.PHONY: compose-after_down
compose-after_down:

.PHONY: compose_build
compose_build:

	$(call compose_compose,build)
