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
EXETARGET = ksLight.cpp

#Nothing below here should have to change (much, maybe)

F77SOURCES := $(notdir $(shell ls $(SRCDIR)/*.for | grep -v $(EXETARGET)))
CPPSOURCES := $(notdir $(shell ls $(SRCDIR)/*.cpp | grep -v $(EXETARGET)))


#INCLUDES := $(notdir $(shell ls $(INCDIR)/*.h ))
#echo INCLUDES: $(INCLUDES)

EXEOBJECT = $(addprefix $(TMPDIR)/, $(EXETARGET:.cpp=.o)) 
EXE = $(addprefix $(BINDIR)/, $(EXETARGET:.cpp=)) 

all: $(LIBTARGET) $(EXEOBJECT) $(EXE)

F77OBJECTS = $(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)) 
#F90OBJECTS = $(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)) 
CPPOBJECTS = $(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) 

#$(LIBTARGET): $(F77OBJECTS) $(F90OBJECTS) $(CPPOBJECTS)
$(LIBTARGET): $(F77OBJECTS) $(CPPOBJECTS)
	$(AR) r $@ $^
	ranlib $@

$(EXE): $(LIBTARGET)
	$(F90) -o $(EXE) $(EXEOBJECT) $^ $(KASLDFLAGS) $(KASLIBS) $(CLDFLAGS)

$(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.for
	$(F77) -o $@ -c $(F77FLAGS) $<

$(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.f90
	$(F90) -o $@ -c $(F90FLAGS) $<

$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) -D_NOROOT $<

#$(addprefix $(TMPDIR)/, $(EXETARGET:.for=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.for
#	$(F77) -o $@ -c $(F77FLAGS) $<

$(addprefix $(TMPDIR)/, $(EXETARGET:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS)  -D_NOROOT $<


.PHONY: install

install:
	$(INSTALLCMD) $(EXE) $(TOPDIR)/bin 

.PHONY: clean

clean:
	$(RM) $(LIBTARGET) $(addsuffix /*~, $(ALLDIR)) \
		$(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)) \
		$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) \
		$(EXEOBJECT) \
		$(EXE)

#		$(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)) \
