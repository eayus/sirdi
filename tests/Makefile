SIRDI ?= "$$(pwd)/../build/exec/sirdi"

help:
	echo $(PWD)

INTERACTIVE ?= --interactive

.PHONY: testbin test

test:
	./build/exec/runtests $(SIRDI) $(INTERACTIVE) --failure-file failures --only $(only)

retest:
	@touch failures
	./build/exec/runtests $(SIRDI) $(INTERACTIVE) --failure-file failures --only-file failures --only $(only)

testbin:
	idris2 --build tests.ipkg

clean:
	rm -r build

