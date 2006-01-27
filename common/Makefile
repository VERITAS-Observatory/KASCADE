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


LIBTARGET = $(LIBDIR)/libKASCADECommon.a

F77SOURCES := $(notdir $(shell echo $(SRCDIR)/*.for))
F90SOURCES := $(notdir $(shell echo $(SRCDIR)/*.f90))
CPPSOURCES := $(notdir $(shell echo $(SRCDIR)/*.cpp))

INCLUDES := $(notdir $(shell ls $(INCDIR)/*.h ))


all: $(LIBTARGET) 

F77OBJECTS = $(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)) 
F90OBJECTS = $(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)) 
CPPOBJECTS = $(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) 

$(LIBTARGET): $(F77OBJECTS) $(F90OBJECTS) $(CPPOBJECTS)
	$(AR) r $@ $^
	ranlib $@

$(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.for
	$(F77) -o $@ -c $(F77FLAGS) $<

$(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.f90
	$(F90) -o $@ -c $(F90FLAGS) $<

$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) $<

.PHONY: clean

clean:
	$(RM) $(LIBTARGET) $(addsuffix /*~, $(ALLDIR)) \
		$(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)) \
		$(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)) \
		$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) 


