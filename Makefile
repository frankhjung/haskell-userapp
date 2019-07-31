#!/usr/bin/env make

# execute fib with parameters:
#
#   make ARGS="-i 12 -s 12" exec

.PHONY:	all bench build check clean cleanall doc exec ghci install lint setup style tags test

TARGET	:= userapp-exe
SUBS	:= $(wildcard */)
SRCS	:= $(wildcard $(addsuffix *.hs, $(SUBS)))
RTSOPTS	?= +RTS -N1

ARGS	?= 'testuser testpassword'

.PHONY: default
default:	check build test

all:	check build test doc exec

check:	tags style lint

tags:
	@hasktags --ctags --extendedctag $(SRCS)

style:
	@stylish-haskell --config=.stylish-haskell.yaml --inplace $(SRCS)

lint:
	@hlint --color $(SRCS)

build:
	@stack build --pedantic --no-test --ghc-options='-O2'

test:
	@stack test --coverage --test-arguments '$(RTSOPTS)'

exec:
	@echo $(ARGS) | stack exec -- $(TARGET) $(RTSOPTS) -s

bench:
	@stack bench --benchmark-arguments '-o .stack-work/benchmark.html $(RTSOPTS)'

doc:
	@stack haddock

install:
	@stack install --local-bin-path $(HOME)/bin

setup:
	-stack setup
	-stack build --dependencies-only --test --no-run-tests
	-stack query
	-stack ls dependencies

ghci:
	@stack ghci --ghci-options -Wno-type-defaults

jupyter:
	@stack exec jupyter -- notebook

clean:
	@stack clean
	@$(RM) -rf $(TARGET).tix stack.yaml.lock

cleanall: clean
	@$(RM) -rf .stack-work/ $(TARGET)
