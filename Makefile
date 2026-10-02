
.PHONY= update build optim

all: clean update build optim

mhs: build-mhs

update:
	wasm32-wasi-cabal update

build:
	wasm32-wasi-cabal build 
	rm -rf public
	cp -r static public
	$(eval my_wasm=$(shell wasm32-wasi-cabal list-bin app | tail -n 1))
	$(shell wasm32-wasi-ghc --print-libdir)/post-link.mjs --input $(my_wasm) --output public/ghc_wasm_jsffi.js
	cp -v $(my_wasm) public/

optim:
	wasm-opt -all -O2 public/app.wasm -o public/app.wasm
	wasm-tools strip -o public/app.wasm public/app.wasm

serve:
	http-server public

clean:
	rm -rf dist-newstyle dist-mcabal public

# The three package is not in the MicroHs package database of the mhs shell,
# so install the revision pinned in cabal.project into it (.mcabal) first.
THREE_REPO = https://github.com/three-hs/three.hs
THREE_REV  = 8af69c0d3e6b4f14efb9dd1e65c85c7df9be095d

dist-mcabal/three.hs-$(THREE_REV):
	rm -rf dist-mcabal/three.hs-*
	git clone -q $(THREE_REPO) $@
	git -C $@ checkout -q $(THREE_REV)
	cd $@ && mcabal install

build-mhs: dist-mcabal/three.hs-$(THREE_REV)
	mcabal --options=-tbrowser build
	rm -rf public
	cp -rv static public
	cp -v ./dist-mcabal/bin/mhs/app public/index.js

