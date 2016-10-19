install: ## Install build dependencies and packages
	npm install -g elm
	npm install -g elm-test
	elm-package install -y

build: ## Build
	elm-make

test: ## Run unit tests
	elm-test

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-10s\033[0m %s\n", $$1, $$2}'

.DEFAULT_GOAL := help
