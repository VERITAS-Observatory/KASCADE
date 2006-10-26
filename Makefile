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
STAGES =common \
	kascade \
	kaslite \
	kassrtmrg \
	kastrigger \
	kasaomega \
	ksarraytrigger \
	utilities

#VHDF5 \

TARGETS = $(STAGES)

INSTALLTARGETS =kascade \
		kaslite \
		kassrtmrg \
		kastrigger \
		kasaomega \
		ksarraytrigger \
		utilities

all: $(STAGES) install


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
