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

#Order of building of STAGES is important
STAGES =VHDF5 \
	common \
	kascade \
	kaslite \
	kassrtmrg \
	kastrigger \
	kasaomega

TARGETS = $(STAGES)

all: $(STAGES)


.PHONY: $(TARGETS)

$(TARGETS):
	$(MAKE) -C $@

clean: $(addsuffix -clean,$(TARGETS))

.PHONY: $(addsuffix -clean,$(TARGETS))

$(addsuffix -clean,$(TARGETS)):
	$(MAKE) -C $(@:-clean=) clean
