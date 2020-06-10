DEST_DIR = $(shell pwd)/dist

install:
	stack install --split-objs --ghc-options="-fPIC -fllvm"

build:
	stack build

build-static:
	./nix-build.sh $(DEST_DIR)

serve:
	stack build && stack exec -- apie-server
