.DEFAULT_GOAL := help

################################################################################
# Server

serve: ## Launch the server.
	mkdir -p tmp
	cabal run --ghc-options="-O0" -- csdc-server serve --config=config.json

.PHONY: serve

TEST_ARGS=

serve-test: ## Launch the test script in the server app.
	mkdir -p tmp
	cabal run --ghc-options="-O0" -- csdc-server test --config=config.json $(TEST_ARGS)

.PHONY: serve-test

################################################################################
# GUI

gui-build: ## Build the GUI into www/app.js.
	mkdir -p www
	cd csdc-gui && elm make src/Main.elm --output ../www/app.js

.PHONY: gui-build

gui-build-optimized: ## Build the GUI into www/app.min.js
	mkdir -p www
	cd csdc-gui && elm make --optimize src/Main.elm --output ../www/app.js
	uglifyjs www/app.js \
	  --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
		| uglifyjs --mangle --output=www/app.min.js

.PHONY: gui-build-optimized

################################################################################
# Haskell development

ghcid: ## Launch ghcid for dars-server.
	ghcid --command 'cabal repl dars-server --ghc-options="-O0"'

.PHONY: ghcid

ghcid-server: ## Launch ghcid for the server executable.
	ghcid --command 'cabal repl dars-server:exe:dars-server --ghc-options="-O0"'

.PHONY: ghcid-server

format-haskell: ## Format the Haskell code
	ormolu --mode inplace $$(git ls-files '*.hs')
	cabal-fmt --inplace $$(git ls-files '*.cabal')

.PHONY: format-haskell

################################################################################
# Database

postgres: ## Launch psql with the correct arguments.
	database/run-postgresql.sh

psql: ## Launch psql with the correct arguments.
	PGHOST=localhost PGPORT=5432 PGDATABASE=dars PGUSER=dars PGPASSWORD=dars psql

.PHONY: psql

################################################################################
# Help

help: ## Print the list of commands.
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: help
