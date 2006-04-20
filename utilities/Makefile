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
CPPTARGET2   = ksPe2TFile.cpp

CPPTARGETS= $(CPPTARGET1) $(CPPTARGET2)
CPPTARGETOBJECTS   = $(addprefix $(TMPDIR)/, $(CPPTARGETS:.cpp=.o)) 
CPPEXES      = $(addprefix $(BINDIR)/, $(CPPTARGETS:.cpp=)) 

CPPSOURCES := $(notdir $(shell ls $(SRCDIR)/*.cpp | grep -v $(CPPTARGET1) | \
                 grep -v $(CPPTARGET2)  ))
CPPOBJECTS = $(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) 


F90TARGET1   = m2hdf5.f90
F90TARGETS   = $(F90TARGET1)
F90TARGETOBJECTS   = $(addprefix $(TMPDIR)/, $(F90TARGETS:.f90=.o)) 
F90EXES      = $(addprefix $(BINDIR)/, $(F90TARGETS:.f90=)) 

F90SOURCES := $(notdir $(shell ls $(SRCDIR)/*.f90 | grep -v $(F90TARGET1)))
F90OBJECTS = $(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)) 


#all: $(LIBTARGET) $(CPPTARGETOBJECT) $(F90TARGETOBJECT) $(CPPTARGET) \
#     $(F90TARGET)

all: $(LIBTARGET) $(CPPTARGETOBJECTS) $(F90TARGETOBJECTS) $(CPPTARGETS) \
     $(F90TARGETS)

$(LIBTARGET): $(F77OBJECTS) $(F90OBJECTS) $(CPPOBJECTS)
	$(AR) r $@ $^
	ranlib $@


$(F90TARGET1): $(LIBTARGET)
	$(F90) -o $(addprefix $(BINDIR)/,$(@:.f90=)) \
	          $(addprefix $(TMPDIR)/,$(@:.f90=.o)) $^ $(LDFLAGS)  \
		$(KASLDFLAGS) $(KASLIBS) \
		$(VHDF5LDFLAGS) $(VHDF5LIBS) \
		$(LIBVERITAS) $(VLIBS) 

$(CPPTARGET1): $(LIBTARGET) 
	$(F90) -o $(addprefix $(BINDIR)/,$(@:.cpp=)) \
	          $(addprefix $(TMPDIR)/,$(@:.cpp=.o)) $^  \
	          $(LDFLAGS) $(KASLDFLAGS) $(KASLIBS) 
$(CPPTARGET2): $(LIBTARGET) 
	$(CXX) -o $(addprefix $(BINDIR)/,$(@:.cpp=)) \
	          $(addprefix $(TMPDIR)/,$(@:.cpp=.o)) $^  \
	          $(CLDFLAGS) $(KASLDFLAGS) $(KASLIBSROOT) $(ROOTLIBS)



$(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.for
	$(F77) -o $@ -c $(F77FLAGS) $<

$(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.f90
	$(F90) -o $@ -c $(F90FLAGS) $<

$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) $<

$(addprefix $(TMPDIR)/, $(CPPTARGETS:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) $<

$(addprefix $(TMPDIR)/, $(F90TARGETS:.f90=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.f90
	$(F90) -o $@ -c $(F90FLAGS) $<



.PHONY: install

install:
	$(INSTALLCMD) $(F90EXE) $(TOPDIR)/bin; \
	$(INSTALLCMD) $(CPPEXE) $(TOPDIR)/bin



.PHONY: clean

clean:
	$(RM) $(LIBTARGET) $(addsuffix /*~, $(ALLDIR)) \
		$(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)) \
		$(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)) \
		$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) \
		$(F90TARGETOBJECTS) $(CPPTARGETOBJECTS) \
		$(F90EXES) $(CPPEXES)

