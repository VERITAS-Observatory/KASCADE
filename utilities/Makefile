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
CPPTARGET2   = ksSumFiles.cpp
CPPTARGET3   = ksMergeFiles.cpp
CPPTARGET4   = ksCopyFile.cpp

CPPTARGETS   = $(CPPTARGET1) $(CPPTARGET2) \
               $(CPPTARGET3) \
	       $(CPPTARGET4) 
CPPTARGETOBJECTS   = $(addprefix $(TMPDIR)/, $(CPPTARGETS:.cpp=.o)) 
CPPEXES      = $(addprefix $(BINDIR)/, $(CPPTARGETS:.cpp=)) 
CPPSOURCES   = $(notdir $(shell ls $(SRCDIR)/*.cpp | grep -v $(CPPTARGET1) | \
		grep -v $(CPPTARGET2) | \
		grep -v $(CPPTARGET3) | \
		grep -v $(CPPTARGET4) ))
CPPOBJECTS = $(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) 

ifdef VEGAS
    ifdef ROOTSYS
all: $(LIBTARGET) $(CPPTARGETOBJECTS) $(CPPEXES)
    else 
all:  $(addprefix $(TMPDIR)/, $(CPPTARGET1:.cpp=.o)) \
	$(addprefix $(BINDIR)/, $(CPPTARGET1:.cpp=)) norootcomment
    endif
else
all:  $(addprefix $(TMPDIR)/, $(CPPTARGET1:.cpp=.o)) \
	$(addprefix $(BINDIR)/, $(CPPTARGET1:.cpp=)) noVEGAScomment
endif

$(LIBTARGET): $(CPPOBJECTS)
	$(AR) r $@ $^
	ranlib $@

$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) $<

$(addprefix $(TMPDIR)/, $(CPPTARGET1:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) $<

$(addprefix $(TMPDIR)/, $(CPPTARGET2:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(VEGASCMNINCDIR) $(CFLAGS)   $(VBFCFLAGS) \
	$<
# -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS 

$(addprefix $(TMPDIR)/, $(CPPTARGET3:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(VEGASCMNINCDIR) $(CFLAGS) $(VBFCFLAGS) \
	 $< 
#-D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS 

$(addprefix $(TMPDIR)/, $(CPPTARGET4:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(VEGASCMNINCDIR) $(CFLAGS) $(VBFCFLAGS) \
	 $<
# -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS 

$(addprefix $(BINDIR)/, $(CPPTARGET1:.cpp=)): $(LIBTARGET) 
	$(F90) -o $(@) $(addprefix $(TMPDIR)/, $(CPPTARGET1:.cpp=.o)) $^  \
	          $(KASLDFLAGS) $(KASLIBS) $(CLDFLAGS)

$(addprefix $(BINDIR)/, $(CPPTARGET2:.cpp=)): $(LIBTARGET) 
	$(F90) -o $(@) $(addprefix $(TMPDIR)/, $(CPPTARGET2:.cpp=.o)) $^  \
	          $(VEGASLDFLAGS) $(CLDFLAGS) $(KASLDFLAGS) $(KASLIBS) \
	          $(ROOTLIBS) $(VBFLDFLAGS) $(VBFLIBS) 

$(addprefix $(BINDIR)/, $(CPPTARGET3:.cpp=)): $(LIBTARGET) 
	$(F90) -o $(@) $(addprefix $(TMPDIR)/, $(CPPTARGET3:.cpp=.o)) $^  \
	          $(VEGASLDFLAGS) $(CLDFLAGS) $(KASLDFLAGS) $(KASLIBS) \
	          $(ROOTLIBS) $(VBFLDFLAGS) $(VBFLIBS) 


$(addprefix $(BINDIR)/, $(CPPTARGET4:.cpp=)): $(LIBTARGET) 
	$(F90) -o $(@) $(addprefix $(TMPDIR)/, $(CPPTARGET4:.cpp=.o)) $^  \
	          $(VEGASLDFLAGS) $(CLDFLAGS) $(KASLDFLAGS) $(KASLIBS) \
	          $(ROOTLIBS) $(VBFLDFLAGS) $(VBFLIBS) 

 
.PHONY: install

ifdef VEGAS
    ifdef ROOTSYS
install:
	$(INSTALLCMD) $(CPPEXES) $(TOPDIR)/bin
    else 
install:norootcomment 
	$(INSTALLCMD) $(addprefix $(BINDIR)/, $(CPPTARGET1:.cpp=)) \
	$(TOPDIR)/bin
    endif
else
install: noVEGAScomment
	$(INSTALLCMD) $(addprefix $(BINDIR)/, $(CPPTARGET1:.cpp=)) \
	$(TOPDIR)/bin
endif




.PHONY: clean

clean:
	$(RM)  $(addsuffix /*~, $(ALLDIR)) \
		$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) \
		$(CPPTARGETOBJECTS) \
		$(CPPEXES)



.PHONY: norootcomment
norootcomment:
	@echo "******************kasaomega/Makefile**************************"
	@echo "* ROOTSYS not defined (ROOT not installed?)"
	@echo "* Can't make or install " \
	$(addprefix $(BINDIR)/, $(CPPTARGET2:.cpp=))
	@echo "**************************************************************"

.PHONY: noVEGAScomment
noVEGAScomment:
	@echo "******************utilities/Makefile**************************"
	@echo "* VEGAS not defined (VEGAS not installed?)"
	@echo "* Can't make or install" \
	$(addprefix $(BINDIR)/, $(CPPTARGET2:.cpp=))
	@echo "**************************************************************"
