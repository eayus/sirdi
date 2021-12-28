INSTALL_DIR = ~/.idris2/bin

.PHONY: build prebuild test testbin clean install

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

install: build
	install build/exec/sirdi $(INSTALL_DIR)
	install build/exec/sirdi_app/* $(INSTALL_DIR)/sirdi_app
