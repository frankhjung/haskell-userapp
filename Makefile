#!/usr/bin/env make

.PHONY:	all bench build check clean cleanall doc exec ghci help install lint setup style tags test

TARGET	:= userapp-exe
SUBS	:= $(wildcard */)
SRCS	:= $(wildcard $(addsuffix *.hs, $(SUBS)))

GOOD_ARGS	?= 'testuser testpassword'
BAD_ARGS	?= 'test%user test@password'

.PHONY: default
default:	check build test exec

all:	check build test doc exec

check:	tags style lint

tags:
	@hasktags --ctags --extendedctag $(SRCS)

style:
	@stylish-haskell --config=.stylish-haskell.yaml --inplace $(SRCS)

lint:
	@hlint --color $(SRCS)

help:
	@echo "make [all|bench|build|check|clean|cleanall|doc|exec|ghci|install|lint|setup|style|tags|test]"

build:
	@stack build --pedantic --no-test --ghc-options='-O2'

test:
	@stack test

exec:
	@echo
	@echo With good parameters ...
	echo $(GOOD_ARGS) | stack exec -- $(TARGET) -s
	@echo
	@echo With bad parameters ...
	echo $(BAD_ARGS) | stack exec -- $(TARGET) -s

doc:
	@stack test --coverage
	@stack haddock

bench:
	@stack bench --benchmark-arguments '-o .stack-work/benchmark.html'

install:
	@stack install --local-bin-path $(HOME)/bin

setup:
	-stack setup
	-stack build --dependencies-only --test --no-run-tests
	-stack query
	-stack ls dependencies

ghci:
	@stack ghci --ghci-options -Wno-type-defaults

clean:
	@stack clean
	@$(RM) -rf $(TARGET).tix stack.yaml.lock

cleanall: clean
	@$(RM) -rf .stack-work/ $(TARGET)
