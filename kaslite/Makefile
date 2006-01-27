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


LIBTARGET = $(LIBDIR)/libkaslite.a
EXETARGET = kaslite.for

#Nothing below here should have to change (much, maybe)

F77SOURCES := $(notdir $(shell ls $(SRCDIR)/*.for | grep -v $(EXETARGET)))
F90SOURCES := $(notdir $(shell ls $(SRCDIR)/*.f90 | grep -v $(EXETARGET)))
CPPSOURCES := $(notdir $(shell ls $(SRCDIR)/*.cpp | grep -v $(EXETARGET)))


INCLUDES := $(notdir $(shell ls $(INCDIR)/*.h ))
#echo INCLUDES: $(INCLUDES)

EXEOBJECT = $(addprefix $(TMPDIR)/, $(EXETARGET:.for=.o)) 
EXE = $(addprefix $(BINDIR)/, $(EXETARGET:.for=)) 

all: $(LIBTARGET) $(EXEOBJECT) $(EXETARGET)

F77OBJECTS = $(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)) 
F90OBJECTS = $(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)) 
CPPOBJECTS = $(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) 

$(LIBTARGET): $(F77OBJECTS) $(F90OBJECTS) $(CPPOBJECTS)
	$(AR) r $@ $^
	ranlib $@

$(EXETARGET): $(LIBTARGET)
	$(F90) -o $(EXE) $(EXEOBJECT) $^ $(LDFLAGS)  $(KASLDFLAGS) $(KASLIBS)

$(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.for
	$(F77) -o $@ -c $(F77FLAGS) $<

$(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.f90
	$(F90) -o $@ -c $(F90FLAGS) $<

$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) $<

$(addprefix $(TMPDIR)/, $(EXETARGET:.for=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.for
	$(F77) -o $@ -c $(F77FLAGS) $<

.PHONY: clean

clean:
	$(RM) $(LIBTARGET) $(addsuffix /*~, $(ALLDIR)) \
		$(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)) \
		$(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)) \
		$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) \
		$(EXEOBJECT) \
		out/.depend \
		$(EXE)

