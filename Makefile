#!/usr/bin/env make

.DEFAULT_GOAL := default

SRC	:= $(wildcard *.hs */*.hs)
TARGET	:= UserApp

.PHONY: default
default: format check build test exec

.PHONY: all
all:	format check build test doc exec

.PHONY: format
format:	$(SRC)
	@echo format ...
	@cabal-fmt --inplace $(TARGET).cabal
	@stylish-haskell --verbose --inplace $(SRC)

.PHONY: check
check:	tags lint

.PHONY: tags
tags:	$(SRC)
	@echo tags ...
	@hasktags --ctags --extendedctag $(SRC)

.PHONY: lint
lint:	$(SRC)
	@echo lint ...
	@cabal check --verbose
	@hlint --cross --color --show $(SRC)

.PHONY: build
build:
	@echo build ...
	@stack build --pedantic --no-test

.PHONY: test
test:
	@echo test ...
	@stack test --fast

.PHONY: doc
doc:
	@echo doc ...
	@stack haddock

.PHONY: exec
exec:
	@echo
	@echo run with good parameters ...
	stack exec -- main testuser testpassword
	@echo
	@echo run with bad parameters ...
	stack exec -- main 'test%user' 'test@password'

.PHONY: setup
setup:
	stack update
	stack path
	stack query
	stack ls dependencies

.PHONY: ghci
ghci:
	@stack ghci --ghci-options -Wno-type-defaults

.PHONY: clean
clean:
	@stack clean
	@cabal clean

.PHONY: cleanall
cleanall: clean
	@stack purge
	@rm -f tags
