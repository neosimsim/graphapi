.POSIX:

all:
	cabal new-build

install:
	cabal new-install --overwrite-policy=always exe:graphsearch

check:
	cabal -fdevelop new-build exe:graphsearch
	cabal -fdevelop new-test
	cabal check
	hfmt -l

clean:
	cabal new-clean

