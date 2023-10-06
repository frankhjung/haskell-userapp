#!/usr/bin/env make

.PHONY:	all build check clean cleanall default exec ghci lint setup style tags test

SRC	:= $(wildcard *.hs */*.hs)

default:	format check build test exec

all:	format check build test exec

format:	$(SRC)
	@echo format ...
	@stylish-haskell --config=.stylish-haskell.yaml --inplace $(SRC)
	@cabal-fmt --inplace UserApp.cabal

check:	tags lint

tags:	$(SRC)
	@echo tags ...
	@hasktags --ctags --extendedctag $(SRC)

lint:	$(SRC)
	@echo lint ...
	@cabal check --verbose
	@hlint --cross --color --show $(SRC)

build:
	@echo build ...
	@stack build --pedantic

test:
	@echo test ...
	@stack test

exec:
	@echo
	@echo run with good parameters ...
	stack exec -- main testuser testpassword
	@echo
	@echo run with bad parameters ...
	stack exec -- main 'test%user' 'test@password'

setup:
	stack update
	stack path
	stack query
	stack ls dependencies

ghci:
	@stack ghci --ghci-options -Wno-type-defaults

clean:
	@stack clean
	@cabal clean
	@rm -f tags
	@rm -f $(wildcard *.hi **/*.hi)
	@rm -f $(wildcard *.o **/*.o)
	@rm -f $(wildcard *.prof **/*.prof)
	@rm -f $(wildcard *.tix **/*.tix)

cleanall: clean
	@stack purge
