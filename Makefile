SHELL := /bin/bash

.PHONY: all build run build-elm build-backend watch clean

all: build

build: build-backend build-elm

build-backend:
	cabal build exe:haskell-course

build-elm:
	./scripts/build-elm.sh

run: build-backend
	cabal run exe:haskell-course

watch:
	./scripts/watch-elm.sh

clean:
	rm -rf dist dist-newstyle frontend/elm-stuff frontend/.elm-deps-ok
	rm -f .elm-bin/elm
