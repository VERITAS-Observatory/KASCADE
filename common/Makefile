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

#This is the No root version of the Makefile. To have a root version we would
# remove the   -D_NOROOT in  the cpp compile options.(see commented out 
#example) 
#The NOROOT version is usable with ksKascade and ksKasLight but root is needed 
#for the later stages: ksKasSort, ksKasTrigger and ksKasAomega

#Some of this f90 compiles get a little tricky. The order they are done is 
#important. Do those sepecial files first

#FIRSTF90SOURCES    := w10m_subs.f90
FIRSTF90USESOURCES := structures.f90



F77SOURCES    := $(notdir $(shell echo $(SRCDIR)/*.for))
#F90SOURCES    := $(FIRSTF90SOURCES) \
#	   $(notdir $(shell ls $(SRCDIR)/*.f90 | grep -v $(FIRSTF90SOURCES)))
CPPSOURCES    := $(notdir $(shell echo $(SRCDIR)/*.cpp))
F90USESOURCES := $(FIRSTF90USESOURCES) \
	   $(notdir $(shell ls $(INCDIR)/*.f90| grep -v $(FIRSTF90USESOURCES)))

F77OBJECTS = $(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)) 
F90OBJECTS = $(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)) 
#CPPOBJECTS = $(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o))  \
#	     $(TMPDIR)/root_dict.o
CPPOBJECTS = $(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o))  

F90USEOBJECTS = $(addprefix $(TMPDIR)/, $(F90USESOURCES:.f90=.o)) 

LIBTARGET := $(LIBDIR)/libKascadeCommon.a
LIBTARGETROOT := $(LIBDIR)/libKascadeCommonRoot.a
LIBSOURCES := $(F90USEOBJECTS) $(F77OBJECTS) $(F90OBJECTS) $(CPPOBJECTS) 

ifndef ROOTYES
	NOROOTFLAG := -D_NOROOT
else
	LIBTARGET := $(LIBTARGETROOT)
	CPPOBJECTS=$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) \
                   $(TMPDIR)/root_dict.o
	LIBSOURCES := $(CPPOBJECTS) 
	NOROOTFLAG :=
endif



INCLUDES  := $(notdir $(shell ls $(INCDIR)/*.h ))
CLASSDEFS := $(notdir $(shell grep ClassDef -l $(INCDIR)/*.h $(INCDIR)/*.hpp))

all: $(LIBTARGET)


$(LIBTARGET): $(LIBSOURCES)
	$(AR) r $@ $^
	ranlib $@

$(TMPDIR)/root_dict.cpp: $(addprefix $(INCDIR)/, $(CLASSDEFS))
	$(ROOTCINT) -v -f $@ -c -p $(ROOTCINTFLAGS) $^ include/LinkDef.h


$(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.for
	$(F77) -o $@ -c $(F77FLAGS) $<

$(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.f90
	$(F90) -o $@ -c $(F90FLAGS) $<

#$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
#	$(CXX) -o $@ -c $(CFLAGS) $<

#use below for no rot version of common library
$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) $(NOROOTFLAG) $<

$(addprefix $(TMPDIR)/, $(F90USESOURCES:.f90=.o)): $(TMPDIR)/%.o: \
            $(INCDIR)/%.f90
	$(F90) -o $@ -c $(F90FLAGS) $<

$(TMPDIR)/root_dict.o: $(TMPDIR)/root_dict.cpp
	$(CXX) -o $@ -c $(CFLAGS) $< 


.PHONY: clean

clean:
	$(RM) $(LIBTARGET) $(LIBTARGETROOT) \
	        $(addsuffix /*~, $(ALLDIR)) *.mod \
		$(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)) \
		$(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)) \
		$(addprefix $(TMPDIR)/, $(F90USESOURCES:.f90=.o)) \
		$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) \
		$(TMPDIR)/root_dict.o \
		$(TMPDIR)/root_dict.cpp $(TMPDIR)/root_dict.h \
		out/.depend

.PHONY: depend

depend: out/.depend

out/.depend: $(addprefix $(SRCDIR)/, $(SOURCES))
	$(CXX) -E -M $(CXXFLAGS) $^ | perl -pe 's/(^[^\s])/'$(TMPDIR)'\/$$1/' > $@

include out/.depend


.PHONY: withroot
withroot:
	$(RM) $(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)); \
	$(RM) $(LIBTARGETROOT); \
	export ROOTYES=yes; \
	make all ; \
        export ROOTYES=

