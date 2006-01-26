# -----------------------------------------------------------------------------
#
# KASCADE top level makefile
# 
# Original Author: Glenn Sembroski
# $Author$
# $Date$
# $Revision$
# $Tag$
#
# -----------------------------------------------------------------------------

STAGES = kascade


TARGETS = $(STAGES)

all: $(STAGES)


.PHONY: $(TARGETS)

$(TARGETS):
	$(MAKE) -C $@

clean: $(addsuffix -clean,$(TARGETS))

.PHONY: $(addsuffix -clean,$(TARGETS))

$(addsuffix -clean,$(TARGETS)):
	$(MAKE) -C $(@:-clean=) clean
