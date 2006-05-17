# -----------------------------------------------------------------------------
#
# KASCADE kascade directory makefile
# 
# Original Author: Glenn Sembroski
# $Author$
# $Date$
# $Revision$
# $Tag$
#
# -----------------------------------------------------------------------------

TOPDIR = ..

include ../Makefile.common


LIBTARGET  = $(LIBDIR)/libutilities.a


CPPTARGET1   = randomCreateRanluxSeed.cpp

CPPTARGETS= $(CPPTARGET1) 
CPPTARGETOBJECTS   = $(addprefix $(TMPDIR)/, $(CPPTARGETS:.cpp=.o)) 
CPPEXES      = $(addprefix $(BINDIR)/, $(CPPTARGETS:.cpp=)) 

CPPSOURCES := $(notdir $(shell ls $(SRCDIR)/*.cpp | grep -v $(CPPTARGET1)  ))
CPPOBJECTS = $(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) 

all: $(LIBTARGET) $(CPPTARGETOBJECTS) $(CPPTARGETS)

$(LIBTARGET): $(CPPOBJECTS)
	$(AR) r $@ $^
	ranlib $@

$(CPPTARGET1): $(LIBTARGET) 
	$(F90) -o $(addprefix $(BINDIR)/,$(@:.cpp=)) \
	          $(addprefix $(TMPDIR)/,$(@:.cpp=.o)) $^  \
	           $(KASLDFLAGS) $(KASLIBS) $(CLDFLAGS) 

$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) $<

$(addprefix $(TMPDIR)/, $(CPPTARGETS:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) $<


.PHONY: install

install:
	$(INSTALLCMD) $(CPPEXES) $(TOPDIR)/bin



.PHONY: clean

clean:
	$(RM) $(LIBTARGET) $(addsuffix /*~, $(ALLDIR)) \
		$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) \
		$(CPPTARGETOBJECTS) \
		$(CPPEXES)

