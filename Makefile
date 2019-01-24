.POSIX:

all:
	$(MAKE) -C graphsearch all
	$(MAKE) -C graphserver all

install:
	$(MAKE) -C graphsearch install
	$(MAKE) -C graphserver install

check:
	$(MAKE) -C graphsearch check
	$(MAKE) -C graphserver check

clean:
	$(MAKE) -C graphsearch clean
	$(MAKE) -C graphserver clean

regression-tests: install
	$(MAKE) -k -C regress regress

