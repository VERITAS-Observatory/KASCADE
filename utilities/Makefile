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


CPPTARGET   = randomCreateRanluxSeed.cpp
CPPTARGETOBJECT   = $(addprefix $(TMPDIR)/, $(CPPTARGET:.cpp=.o)) 
CPPEXE      = $(addprefix $(BINDIR)/, $(CPPTARGET:.cpp=)) 

CPPSOURCES := $(notdir $(shell ls $(SRCDIR)/*.cpp | grep -v $(CPPTARGET)))

F90TARGET   = m2hdf5.f90
F90TARGETOBJECT   = $(addprefix $(TMPDIR)/, $(F90TARGET:.f90=.o)) 
F90EXE      = $(addprefix $(BINDIR)/, $(F90TARGET:.f90=)) 

F90SOURCES := $(notdir $(shell ls $(SRCDIR)/*.f90 | grep -v $(F90TARGET)))


#all: $(LIBTARGET) $(CPPTARGETOBJECT) $(F90TARGETOBJECT) $(CPPTARGET) \
#     $(F90TARGET)

all: $(LIBTARGET) $(CPPTARGETOBJECT) $(F90TARGETOBJECT) $(CPPTARGET) \
     $(F90TARGET)



F90OBJECTS = $(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)) 
CPPOBJECTS = $(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) 

$(LIBTARGET): $(F77OBJECTS) $(F90OBJECTS) $(CPPOBJECTS)
	$(AR) r $@ $^
	ranlib $@


$(F90TARGET): $(LIBTARGET)
	$(F90) -o $(F90EXE) $(F90TARGETOBJECT) $^ $(LDFLAGS)  \
		$(KASLDFLAGS) $(KASLIBS) \
		$(VHDF5LDFLAGS) $(VHDF5LIBS) \
		$(LIBVERITAS) $(VLIBS) 

#Use F90 here which uses absoft linker which knows of libU77
$(CPPTARGET): $(LIBTARGET)
	$(F90) -o $(CPPEXE) $(CPPTARGETOBJECT) $^ $(LDFLAGS)  \
		$(KASLDFLAGS) $(KASLIBS) \
		$(VHDF5LDFLAGS) $(VHDF5LIBS) \
		$(LIBVERITAS) $(VLIBS) 



$(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.for
	$(F77) -o $@ -c $(F77FLAGS) $<

$(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.f90
	$(F90) -o $@ -c $(F90FLAGS) $<

$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) $<

$(addprefix $(TMPDIR)/, $(CPPTARGET:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) $<

$(addprefix $(TMPDIR)/, $(F90TARGET:.f90=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.f90
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
		$(F90TARGETOBJECT) $(CPPTARGETOBJECT) \
		$(F90EXE) $(CPPEXE)

