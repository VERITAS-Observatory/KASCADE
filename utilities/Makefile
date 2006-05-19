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
CPPTARGET2   = ksSumVDFRootFiles.cpp
CPPTARGETS   = $(CPPTARGET1) $(CPPTARGET2)
CPPTARGETOBJECTS   = $(addprefix $(TMPDIR)/, $(CPPTARGETS:.cpp=.o)) 
CPPEXES      = $(addprefix $(BINDIR)/, $(CPPTARGETS:.cpp=)) 
CPPSOURCES   = $(notdir $(shell ls $(SRCDIR)/*.cpp | grep -v $(CPPTARGET1) | grep -v $(CPPTARGET2) ))
CPPOBJECTS = $(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) 

all: $(LIBTARGET) $(CPPTARGETOBJECTS) $(CPPEXES)

$(LIBTARGET): $(CPPOBJECTS)
	$(AR) r $@ $^
	ranlib $@

$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) $<

$(addprefix $(TMPDIR)/, $(CPPTARGET1:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) $<

$(addprefix $(TMPDIR)/, $(CPPTARGET2:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(VEGASCMNINCDIR) $(CFLAGS)  $< -D__STDC_LIMIT_MACROS \
        -D__STDC_CONSTANT_MACROS 

$(addprefix $(BINDIR)/, $(CPPTARGET1:.cpp=)): $(LIBTARGET) 
	$(F90) -o $(@) $(addprefix $(TMPDIR)/, $(CPPTARGET1:.cpp=.o)) $^  \
	          $(KASLDFLAGS) $(KASLIBS) $(CLDFLAGS)

$(addprefix $(BINDIR)/, $(CPPTARGET2:.cpp=)): $(LIBTARGET) 
	$(F90) -o $(@) $(addprefix $(TMPDIR)/, $(CPPTARGET2:.cpp=.o)) $^  \
	          $(VEGASLDFLAGS) $(CLDFLAGS) $(KASLDFLAGS) $(KASLIBS) \
	          $(ROOTLIBS) 

 
.PHONY: install

install:
	$(INSTALLCMD) $(CPPEXES) $(TOPDIR)/bin



.PHONY: clean

clean:
	$(RM)  $(addsuffix /*~, $(ALLDIR)) \
		$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) \
		$(CPPTARGETOBJECTS) \
		$(CPPEXES)

