install:
	stack install --split-objs --ghc-options="-fPIC -fllvm"

build:
	stack build

build-static:
	./nix-build.sh

serve:
	stack build && stack exec -- apie-server

serve-cgi:
	python -m http.server --cgi 8000
