.PHONY: build test testbin

build:
	idris2 --build sirdi.ipkg

testbin:
	make -C tests testbin

test:
	make -C tests test
