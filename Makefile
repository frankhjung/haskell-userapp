#!/usr/bin/env make

.PHONY:	all build check clean cleanall default exec ghci lint setup style tags test

TARGET	:= userapp
SRC	:= $(wildcard *.hs */*.hs)

GOOD_ARGS	?= 'testuser testpassword'
BAD_ARGS	?= 'test%user test@password'

default:	check build test exec

all:	check build test exec

check:	tags style lint

tags:	$(SRC)
	@echo tags ...
	@hasktags --ctags --extendedctag $(SRC)

style:	$(SRC)
	@echo style ...
	@stylish-haskell --config=.stylish-haskell.yaml --inplace $(SRC)

lint:	$(SRC)
	@echo lint ...
	@cabal check --verbose
	@hlint --cross --color --show $(SRC)

build:
	@echo build ...
	@stack build --pedantic

test:
	@echo test ...
	@stack test $(TARGET)

exec:
	@echo
	@echo With good parameters ...
	echo $(GOOD_ARGS) | stack exec -- $(TARGET) -s
	@echo
	@echo With bad parameters ...
	echo $(BAD_ARGS) | stack exec -- $(TARGET) -s

setup:
	@stack path
	@stack query
	@stack ls dependencies

ghci:
	@stack ghci --ghci-options -Wno-type-defaults

clean:
	@stack clean
	-$(RM) $(addsuffix .hi, $(basename $(SRC)))
	-$(RM) $(addsuffix .o, $(basename $(SRC)))
	-$(RM) $(addsuffix .prof, $(basename $(SRC)))
	-$(RM) $(addsuffix .tix, $(basename $(SRC)))

cleanall: clean
	@stack purge
