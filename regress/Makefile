SCRIPTS=\
	result-written \
	create-dirs-on-start \

TARGETS=$(SCRIPTS) \
	help \

regress: $(TARGETS)

$(SCRIPTS):
	sh -e $@

help:
	graphsearch -h | diff help.out -

.PHONY: $(TARGETS)

