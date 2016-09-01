# -----------------------------------------------------------------------------
#
# kaslite  directory makefile
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


LIBTARGET = $(LIBDIR)/libkassrtmrg.a
EXETARGET = ksPeSortMerge.cpp
#TESTTARGET = LargeTTreeTest.cpp

#Nothing below here should have to change (much, maybe)

#F77SOURCES := $(notdir $(shell ls $(SRCDIR)/*.for | grep -v $(EXETARGET)))
#F90SOURCES := $(notdir $(shell ls $(SRCDIR)/*.f90 | grep -v $(EXETARGET)))
CPPSOURCES := $(notdir $(shell ls $(SRCDIR)/*.cpp | grep -v $(EXETARGET) ))
#                                                 | grep -v $(TESTTARGET) ))


#INCLUDES := $(notdir $(shell ls $(INCDIR)/*.h ))
#echo INCLUDES: $(INCLUDES)

EXEOBJECT = $(addprefix $(TMPDIR)/, $(EXETARGET:.cpp=.o)) 
EXE = $(addprefix $(BINDIR)/, $(EXETARGET:.cpp=)) 

#TESTEXEOBJECT = $(addprefix $(TMPDIR)/, $(TESTTARGET:.cpp=.o)) 
#TESTEXE = $(addprefix $(BINDIR)/, $(TESTTARGET:.cpp=)) 

#ifdef ROOTSYS

all: $(LIBTARGET) $(EXEOBJECT) $(EXETARGET) 

#$(TESTEXEOBJECT) $(TESTTARGET) 
#else
#all: norootcomment
#endif



CPPOBJECTS = $(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) 

$(LIBTARGET): $(CPPOBJECTS)
	$(AR) r $@ $^
	ranlib $@

$(EXETARGET): $(LIBTARGET) 
	$(CXX) -o $(addprefix $(BINDIR)/,$(@:.cpp=)) \
	          $(addprefix $(TMPDIR)/,$(@:.cpp=.o)) $^  \
	          $(CCLIB) $(KASLDFLAGS) $(KASLIBSROOT) $(ROOTLIBS)

$(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.for
	$(F77) -o $@ -c $(F77FLAGS) $<

$(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.f90
	$(F90) -o $@ -c $(F90FLAGS) $<

$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) $<

#$(addprefix $(TMPDIR)/, $(EXETARGET:.for=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.for
#	$(F77) -o $@ -c $(F77FLAGS) $<

$(addprefix $(TMPDIR)/, $(EXETARGET:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) $<

#$(addprefix $(TMPDIR)/, $(TESTTARGET:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
#	$(CXX) -o $@ -c $(CFLAGS) $<


.PHONY: install

#ifdef ROOTSYS
install:
	$(INSTALLCMD) $(EXE)  $(TOPDIR)/bin 
#        $(TESTEXE)
#else
#install: norootcomment
#endif


.PHONY: clean

clean:
	$(RM) $(LIBTARGET) $(addsuffix /*~, $(ALLDIR)) \
		$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) \
		$(EXEOBJECT)		$(EXE)                 \
		$(TESTEXEOBJECT)        $(TESTEXE)

#		$(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)) \
#		$(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)) \


.PHONY: norootcomment
norootcomment:
	@echo "******************kassrtmrg/Makefile**************************"
	@echo "* ROOTSYS not defined (ROOT not installed?)"
	@echo "* Can't make or install " $(EXE)
	@echo "**************************************************************"
