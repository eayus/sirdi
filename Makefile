INSTALL_DIR = ~/.idris2/bin

.PHONY: build prebuild test testbin clean install

build: ./depends/hashable-0
	idris2 --build sirdi.ipkg

prebuild:
	idris2 -x build build.idr -p contrib

testbin:
	make -C tests testbin

test:
	make -C tests test

clean:
	rm -rf ./build
	rm -rf ./depends
	make -C tests clean

install: ./build/exec/sirdi ./build/exec/sirdi_app
	install build/exec/sirdi $(INSTALL_DIR)
	mkdir -p $(INSTALL_DIR)/sirdi_app
	install $(wildcard build/exec/sirdi_app/*) $(INSTALL_DIR)/sirdi_app/

./depends/hashable-0:
	make prebuild

./build/exec/sirdi:
	make build

./build/exec/sirdi_app:
	make build

