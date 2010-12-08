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


LIBTARGET = $(LIBDIR)/libkascade.a
EXETARGET = ksKascade.cpp

EXETESTTARGET   = BendTest.cpp
LIBTESTTARGET = $(LIBDIR)/libkascadeTest.a
TESTOBJECTS   = $(TMPDIR)/bend.o


F77SOURCES := $(notdir $(shell ls $(SRCDIR)/*.for | grep -v $(EXETARGET)))
#F90SOURCES := $(notdir $(shell ls $(SRCDIR)/*.f90 | grep -v $(EXETARGET)))
CPPSOURCES := $(notdir $(shell ls $(SRCDIR)/*.cpp | grep -v $(EXETARGET)| grep -v $(EXETESTTARGET)))





#INCLUDES := $(notdir $(shell ls $(INCDIR)/*.h $(INCDIR)/*.hpp))
#echo INCLUDES: $(INCLUDES)

#EXEOBJECT = $(addprefix $(TMPDIR)/, $(EXETARGET:.for=.o)) 
#EXE = $(addprefix $(BINDIR)/, $(EXETARGET:.for=)) 
EXEOBJECT = $(addprefix $(TMPDIR)/, $(EXETARGET:.cpp=.o)) 
EXE = $(addprefix $(BINDIR)/, $(EXETARGET:.cpp=)) 

EXETESTOBJECT = $(addprefix $(TMPDIR)/, $(EXETESTTARGET:.cpp=.o)) 
EXETEST = $(addprefix $(BINDIR)/, $(EXETESTTARGET:.cpp=)) 

all: $(LIBTARGET) $(EXEOBJECT) $(EXE)  

test: $(LIBTESTTARGET)  $(TESTOBJECTS) $(EXETESTOBJECT) $(EXETEST)

F77OBJECTS = $(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)) 
#F90OBJECTS = $(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)) 
CPPOBJECTS = $(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) 

#$(LIBTARGET): $(F77OBJECTS) $(F90OBJECTS) $(CPPOBJECTS)
$(LIBTARGET): $(F77OBJECTS) $(CPPOBJECTS)
	$(AR) r $@ $^
	ranlib $@

$(LIBTESTTARGET): $(TESTOBJECTS)
	$(AR) r $@ $^
	ranlib $@




$(EXE): $(LIBTARGET)
	$(F90) -o $(EXE) $(EXEOBJECT) $^ $(KASLDFLAGS) $(KASLIBS) $(CLDFLAGS)

$(EXETEST): $(LIBTESTTARGET)
	$(F90) -o $(EXETEST) $(EXETESTOBJECT) $^ $(KASLDFLAGS) $(CLDFLAGS)


$(addprefix $(TMPDIR)/, $(EXETARGET:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) -D_NOROOT $<

$(addprefix $(TMPDIR)/, $(EXETESTTARGET:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) -D_NOROOT $<


$(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.for
	$(F77) -o $@ -c $(F77FLAGS) $<

#$(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.f90
#	$(F90) -o $@ -c $(F90FLAGS) $<

$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) -D_NOROOT $<



.PHONY: install

install:
	$(INSTALLCMD) $(EXE) $(TOPDIR)/bin 

.PHONY: clean

clean:
	$(RM) $(LIBTARGET) $(addsuffix /*~, $(ALLDIR)) \
		$(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)) \
		$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) \
		$(EXEOBJECT)$(EXETESTOBJECT) \
		out/.depend \
		$(EXE) $(EXETEST)

#		$(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)) \
