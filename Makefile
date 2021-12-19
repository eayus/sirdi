.PHONY: build prebuild test testbin

build: prebuild
	idris2 --build sirdi.ipkg

prebuild:
	idris2 -x build build.idr -p contrib

testbin:
	make -C tests testbin

test:
	make -C tests test

clean:
	rm -r build/
	rm -r depends
	make -C tests clean
