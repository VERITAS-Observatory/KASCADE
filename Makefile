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
	kasaomega \
	utilities

TARGETS = $(STAGES)

INSTALLTARGETS =kascade \
		kaslite \
		kassrtmrg \
		kastrigger \
		kasaomega \
		utilities

all: $(STAGES)


.PHONY: $(TARGETS)

$(TARGETS):
	$(MAKE) -C $@

install: $(addsuffix -install,$(INSTALLTARGETS))

.PHONY: $(addsuffix -install,$(INSTALLTARGETS))

$(addsuffix -install,$(INSTALLTARGETS)):
	$(MAKE) -C $(@:-install=) install


clean: $(addsuffix -clean,$(TARGETS))

.PHONY: $(addsuffix -clean,$(TARGETS))

$(addsuffix -clean,$(TARGETS)):
	$(MAKE) -C $(@:-clean=) clean
