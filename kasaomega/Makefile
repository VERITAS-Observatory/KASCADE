# -----------------------------------------------------------------------------
#
# KASCADE kastrigger directory makefile
# Original Author: Glenn Sembroski
# $Author$
# $Date$
# $Revision$
# $Tag$
#
# -----------------------------------------------------------------------------

TOPDIR = ..

include ../Makefile.common


LIBTARGET = $(LIBDIR)/libkasaomega.a
#EXETARGET = kasaomega.f90
EXETARGET = ksAomega.cpp

# ---------------------------------------------------------------------------
#Define various library sources. This is only for kasaomega and will soon be
# removed. Thats whay the defs are here and not in Makefile.common
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#VERITAS Havester code on amdahl
# ---------------------------------------------------------------------------
#VERITAS = /home/sembrosk/local/src/VERITAS
#INCVERITAS = -I$(VERITAS)
#LIBVERITAS = -L$(VERITAS)/lib
#	gaeaSerial=-lserialization
#	gaeaDefs=-D_GAEA
#VLIBS =  $(gaeaSerial) -ldast -ldatastream -larrayevent -lfpio -lrunbuilder \
#	-ldatum  -ldast -lvglobal $(gaeaSerial)



#F77SOURCES := $(notdir $(shell ls $(SRCDIR)/*.for | grep -v $(EXETARGET)))
#F90SOURCES := $(notdir $(shell ls $(SRCDIR)/*.f90 | grep -v $(EXETARGET)))
CPPSOURCES := $(notdir $(shell ls $(SRCDIR)/*.cpp | grep -v $(EXETARGET)))
#F90USESOURCES := $(notdir $(shell ls $(INCDIR)/*.f90))


INCLUDES := $(notdir $(shell ls $(INCDIR)/*.h ))
#echo INCLUDES: $(INCLUDES)

#EXEOBJECT = $(addprefix $(TMPDIR)/, $(EXETARGET:.f90=.o)) 
#EXE = $(addprefix $(BINDIR)/, $(EXETARGET:.f90=)) 

EXEOBJECT = $(addprefix $(TMPDIR)/, $(EXETARGET:.cpp=.o)) 
EXE = $(addprefix $(BINDIR)/, $(EXETARGET:.cpp=)) 

all: $(LIBTARGET) $(EXEOBJECT) $(EXETARGET)


#F90USEOBJECTS = $(addprefix $(TMPDIR)/, $(F90USESOURCES:.f90=.o)) 
#F77OBJECTS = $(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)) 
#F90OBJECTS = $(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)) 
CPPOBJECTS = $(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) 

#$(LIBTARGET): $(F90USEOBJECTS)  $(F90OBJECTS) $(F77OBJECTS)  $(CPPOBJECTS)
$(LIBTARGET): $(CPPOBJECTS)
	$(AR) r $@ $^
	ranlib $@

#$(EXETARGET): $(LIBTARGET)
#	$(F90) -o $(EXE) $(EXEOBJECT) $^ $(LDFLAGS)  \
#		$(VHDF5LDFLAGS) $(VHDF5LIBS) $(KASLDFLAGS) $(KASLIBS)

#$(EXETARGET): $(LIBTARGET)
#	$(F90) -o $(EXE) $(EXEOBJECT) $^ $(LDFLAGS)  \
#		$(KASLDFLAGS) $(KASLIBS) \
#		$(VHDF5LDFLAGS) $(VHDF5LIBS) \
#		$(LIBVERITAS) $(VLIBS) 

$(EXETARGET): $(LIBTARGET) 
	$(CXX) -o $(addprefix $(BINDIR)/,$(@:.cpp=)) \
	          $(addprefix $(TMPDIR)/,$(@:.cpp=.o)) $^  \
	          $(CLDFLAGS) $(KASLDFLAGS) $(KASLIBSROOT) $(ROOTLIBS) \
	          $(VEGASLDFLAGS)

#$(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.for
#	$(F77) -o $@ -c $(F77FLAGS) $<

#$(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.f90
#	$(F90) -o $@ -c $(F90FLAGS) $<

$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) $(VEGASCMNINCDIR) $<

$(addprefix $(TMPDIR)/, $(EXETARGET:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS)  $(VEGASCMNINCDIR) $<

#$(addprefix $(TMPDIR)/, $(EXETARGET:.f90=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.f90
#	$(F90) -o $@ -c $(F90FLAGS) $<

#$(addprefix $(TMPDIR)/, $(F90USESOURCES:.f90=.o)): $(TMPDIR)/%.o: \
#            $(INCDIR)/%.f90
#	$(F90) -o $@ -c $(F90FLAGS) $<


.PHONY: install

install:
	$(INSTALLCMD) $(EXE) $(TOPDIR)/bin 


.PHONY: clean

clean:
	$(RM) $(LIBTARGET) $(addsuffix /*~, $(ALLDIR))  *.mod \
#		$(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)) \
#		$(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)) \
		$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) \
#		$(addprefix $(TMPDIR)/, $(F90USESOURCES:.f90=.o)) \
		$(EXEOBJECT) \
		out/.depend \
		$(EXE)

