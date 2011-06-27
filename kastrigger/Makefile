# -----------------------------------------------------------------------------
#
# KASCADE kastrigger directory makefile
# 
# Original Author: Glenn Sembroski
# $Author$
# $Date$
# $Revision$
# $Tag$
#
# -----------------------------------------------------------------------------

TOPDIR = ..
#normally comment out next line(we don't want profileing)
#OPTPRF=-pg

include ../Makefile.common


LIBTARGET = $(LIBDIR)/libkastrigger.a
EXETARGET1 = ksTrigger.cpp
EXETARGET2 = makePSF.cpp

F90SOURCES := $(notdir $(shell ls $(SRCDIR)/*.f90 | grep -v $(EXETARGET1)))
CPPSOURCES := $(notdir $(shell ls $(SRCDIR)/*.cpp | grep -v $(EXETARGET1)| grep -v $(EXETARGET2) ))
F90USESOURCES := $(notdir $(shell ls $(INCDIR)/*.f90))


#INCLUDES := $(notdir $(shell ls $(INCDIR)/*.h ))
#echo INCLUDES: $(INCLUDES)

EXEOBJECT1 = $(addprefix $(TMPDIR)/, $(EXETARGET1:.cpp=.o)) 

EXEOBJECT2 = $(addprefix $(TMPDIR)/, $(EXETARGET2:.cpp=.o)) 

EXE1 = $(addprefix $(BINDIR)/, $(EXETARGET1:.cpp=)) 
EXE2 = $(addprefix $(BINDIR)/, $(EXETARGET2:.cpp=)) 

all: $(LIBTARGET) $(EXEOBJECT1) $(EXETARGET1) $(EXEOBJECT2) $(EXETARGET2)


F90USEOBJECTS = $(addprefix $(TMPDIR)/, $(F90USESOURCES:.f90=.o)) 
F90OBJECTS = $(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)) 
CPPOBJECTS = $(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) 

#$(LIBTARGET): $(F90USEOBJECTS)  $(F90OBJECTS) $(F77OBJECTS)  $(CPPOBJECTS)
$(LIBTARGET): $(F90USEOBJECTS)  $(F90OBJECTS) $(CPPOBJECTS)
	$(AR) r $@ $^
	ranlib $@

$(EXETARGET1): $(LIBTARGET) 
	$(F90) -o $(addprefix $(BINDIR)/,$(@:.cpp=)) \
	          $(addprefix $(TMPDIR)/,$(@:.cpp=.o)) $^  \
	          $(CLDFLAGS) $(KASLDFLAGS) $(KASLIBS)  $(ROOTLIBS) $(OPTPRF)

$(EXETARGET2): $(LIBTARGET) 
	$(F90) -o $(addprefix $(BINDIR)/,$(@:.cpp=)) \
	          $(addprefix $(TMPDIR)/,$(@:.cpp=.o)) $^  \
	          $(CLDFLAGS) $(KASLDFLAGS) $(KASLIBS)  $(ROOTLIBS) $(OPTPRF)


$(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.f90
	$(F90) -o $@ -c $(F90FLAGS) $< $(OPTPRF) 

$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) -D_NOROOT $< $(OPTPRF) 

$(addprefix $(TMPDIR)/, $(EXETARGET1:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) -D_NOROOT $< $(OPTPRF) 

$(addprefix $(TMPDIR)/, $(EXETARGET2:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) -D_NOROOT $< $(OPTPRF) 

$(addprefix $(TMPDIR)/, $(F90USESOURCES:.f90=.o)): $(TMPDIR)/%.o: \
            $(INCDIR)/%.f90
	$(F90) -o $@ -c $(F90FLAGS) $<


.PHONY: install

install:
	$(INSTALLCMD) $(EXE1) $(TOPDIR)/bin 	
	$(INSTALLCMD) $(EXE2) $(TOPDIR)/bin 


.PHONY: clean

clean:
	$(RM) $(LIBTARGET) $(addsuffix /*~, $(ALLDIR))  *.mod \
		$(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)) \
		$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) \
		$(addprefix $(TMPDIR)/, $(F90USESOURCES:.f90=.o)) \
		$(EXEOBJECT1)	$(EXEOBJECT2) \
		$(EXE1) $(EXE2)

