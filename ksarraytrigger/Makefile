# -----------------------------------------------------------------------------
#
# KASCADE kkarraytrigger  directory makefile
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

LIBTARGET  = $(LIBDIR)/libksarraytrigger.a

CPPTARGET   = ksArrayTrigger.cpp

CPPTARGETOBJECTS   = $(addprefix $(TMPDIR)/, $(CPPTARGET:.cpp=.o)) 
CPPEXES      = $(addprefix $(BINDIR)/, $(CPPTARGET:.cpp=)) 
CPPSOURCES   = $(notdir $(shell ls $(SRCDIR)/*.cpp | grep -v $(CPPTARGET) ))
CPPOBJECTS   = $(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) 

all: $(LIBTARGET) $(CPPTARGETOBJECTS) $(CPPEXES)

$(LIBTARGET): $(CPPOBJECTS)
	$(AR) r $@ $^
	ranlib $@

$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(VEGASCMNINCDIR) $(CFLAGS) $(VBFCFLAGS)  \
	-D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS 	$< 

$(addprefix $(TMPDIR)/, $(CPPTARGET:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(VEGASCMNINCDIR) $(CFLAGS) $(VBFCFLAGS) \
	-D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS  $<  

$(addprefix $(BINDIR)/, $(CPPTARGET:.cpp=)): $(LIBTARGET) 
	$(F90) -o $(@) $(addprefix $(TMPDIR)/, $(CPPTARGET:.cpp=.o)) $^  \
	          $(VEGASLDFLAGS) $(CLDFLAGS) $(KASLDFLAGS) $(KASLIBS) \
	          $(ROOTLIBS) $(VBFLDFLAGS) $(VBFLIBS) 
 
.PHONY: install

install:
	$(INSTALLCMD) $(CPPEXES) $(TOPDIR)/bin

.PHONY: clean

clean:
	$(RM)  $(addsuffix /*~, $(ALLDIR)) \
		$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) \
		$(CPPTARGETOBJECTS) \
		$(CPPEXES)
