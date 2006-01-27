# -----------------------------------------------------------------------------
#
# KASCADE common directory makefile
# 
# Original Author: Glenn Sembroski
# $Author$
# $Date$
# $Revision$
# $Tag$
#
# -----------------------------------------------------------------------------

include ../Makefile.common

TOPDIR = ..

#Some of this f90 compiles get a little tricky. The order they are done is 
#important. Do those sepecial files first

FIRSTF90SOURCES    := w10m_subs.f90
FIRSTF90USESOURCES := structures.f90


LIBTARGET = $(LIBDIR)/libKASCADECommon.a

F77SOURCES    := $(notdir $(shell echo $(SRCDIR)/*.for))
F90SOURCES    := $(FIRSTF90SOURCES) \
	   $(notdir $(shell ls $(SRCDIR)/*.f90 | grep -v $(FIRSTF90SOURCES)))
CPPSOURCES    := $(notdir $(shell echo $(SRCDIR)/*.cpp))
F90USESOURCES := $(FIRSTF90USESOURCES) \
	   $(notdir $(shell ls $(INCDIR)/*.f90| grep -v $(FIRSTF90USESOURCES)))

INCLUDES      := $(notdir $(shell ls $(INCDIR)/*.h ))


all: $(LIBTARGET) 


F77OBJECTS = $(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)) 
F90OBJECTS = $(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)) 
CPPOBJECTS = $(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) 
F90USEOBJECTS = $(addprefix $(TMPDIR)/, $(F90USESOURCES:.f90=.o)) 

$(LIBTARGET): $(F90USEOBJECTS) $(F77OBJECTS) $(F90OBJECTS) $(CPPOBJECTS) 
	$(AR) r $@ $^
	ranlib $@

$(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.for
	$(F77) -o $@ -c $(F77FLAGS) $<

$(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.f90
	$(F90) -o $@ -c $(F90FLAGS) $<

$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) $<

$(addprefix $(TMPDIR)/, $(F90USESOURCES:.f90=.o)): $(TMPDIR)/%.o: \
            $(INCDIR)/%.f90
	$(F90) -o $@ -c $(F90FLAGS) $<

.PHONY: clean

clean:
	$(RM) $(LIBTARGET) $(addsuffix /*~, $(ALLDIR)) *.mod \
		$(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)) \
		$(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)) \
		$(addprefix $(TMPDIR)/, $(F90USESOURCES:.f90=.o)) \
		$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) 


