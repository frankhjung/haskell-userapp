#!/usr/bin/env make

.PHONY:	all bench build check clean cleanall doc exec ghci install lint setup style tags test

TARGET	:= userapp
SRCS	:= $(wildcard *.hs */*.hs)

GOOD_ARGS	?= 'testuser testpassword'
BAD_ARGS	?= 'test%user test@password'

.PHONY: default
default:	check build test exec

all:	check build test doc exec

check:	tags style lint

tags:	$(SRCS)
	@echo tags ...
	@hasktags --ctags --extendedctag $(SRCS)

style:	$(SRCS)
	@echo style ...
	@stylish-haskell --config=.stylish-haskell.yaml --inplace $(SRCS)

lint:	$(SRCS)
	@echo lint ...
	@hlint --color $(SRCS)

build:
	@echo build ...
	@stack build --no-test

test:
	@echo test ...
	@stack test

exec:
	@echo
	@echo With good parameters ...
	echo $(GOOD_ARGS) | stack exec -- $(TARGET) -s
	@echo
	@echo With bad parameters ...
	echo $(BAD_ARGS) | stack exec -- $(TARGET) -s

doc:
	@stack haddock --no-rerun-tests --no-reconfigure --haddock-deps

bench:
	@stack bench --benchmark-arguments '-o .stack-work/benchmark.html'

install:
	@stack install --local-bin-path $(HOME)/bin

setup:
	@stack update
	@stack setup
	@stack build
	@stack query
	@stack ls dependencies
	#stack exec ghc-pkg -- list

ghci:
	@stack ghci --ghci-options -Wno-type-defaults

clean:
	@stack clean
	@$(RM) -rf *.tix

cleanall: clean
	@stack clean --full
	#$(RM) -rf .stack-work/
	#$(RM) -rf $(patsubst %.hs, %.hi, $(SRCS))
	#$(RM) -rf $(patsubst %.hs, %.o, $(SRCS))
