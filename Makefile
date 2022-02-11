.PHONY: build-deps
build-deps:
	idris2 --exec buildDepends build.idr -p contrib

.PHONY: build-lib
build-lib: build-deps
	idris2 --exec buildSirdiLib build.idr -p contrib

.PHONY: build-cli
build-cli: build-lib
	idris2 --exec buildSirdiCli build.idr -p contrib
