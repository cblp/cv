all: cv

build:
	stack build

cv: build
	stack exec cv
