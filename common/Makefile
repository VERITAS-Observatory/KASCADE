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



#normally comment out next line(we don't want profileing)
#OPTPRF=-pg

TOPDIR = ..

include ../Makefile.common

BUILD_SHARED = $(CXX) -shared -Wl,-s


#This is the No root and root  version of the common Makefile. 
#It creats 2 libraries: libKascadeCommon.a libKascadeCommonRoot.a
#The difference is the specification of the -D_NOROOT in  the cpp compile 
#options. 
#The NOROOT version is usable with ksKascade and ksKasLight but root is needed 
#for the later stages: ksKasSort, ksKasTrigger and ksKasAomega
#If Root is not installed in the system (no ROOTSYS defined) only the non-root
#version of the common library (libKascadeCommon.a) will be built
#Some of this f90 compiles get a little tricky. The order they are done is 
#important. Do those sepecial files first

#FIRSTF90SOURCES    := w10m_subs.f90
FIRSTF90USESOURCES := structures.f90



F77SOURCES    := $(notdir $(shell echo $(SRCDIR)/*.for))
CPPSOURCES    := $(notdir $(shell echo $(SRCDIR)/*.cpp))
F90USESOURCES := $(FIRSTF90USESOURCES) \
	   $(notdir $(shell ls $(INCDIR)/*.f90| grep -v $(FIRSTF90USESOURCES)))

F77OBJECTS = $(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)) 
F90OBJECTS = $(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)) 
CPPOBJECTS = $(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o))  




F90USEOBJECTS = $(addprefix $(TMPDIR)/, $(F90USESOURCES:.f90=.o)) 

TARGET_SHARED = $(LIBDIR)/libKascadeCppCommon.so
LIBTARGET := $(LIBDIR)/libKascadeCommon.a
LIBTARGETROOT := $(LIBDIR)/libKascadeCommonRoot.a $(TARGET_SHARED)
LIBSOURCES := $(F90USEOBJECTS) $(F77OBJECTS) $(F90OBJECTS) $(CPPOBJECTS) 

ifndef ROOTYES
	NOROOTFLAG := -D_NOROOT
else
	LIBTARGET := $(LIBTARGETROOT)
	CPPOBJECTS=$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) \
                   $(TMPDIR)/root_dict_Kascade.o
	LIBSOURCES := $(CPPOBJECTS) 
	NOROOTFLAG :=
endif

SHAREDCPPOBJECTS= $(CPPOBJECTS)  out/root_dict_Kascade.o

INCLUDES  := $(notdir $(shell ls $(INCDIR)/*.h ))
CLASSDEFS := $(notdir $(shell grep ClassDef -l $(INCDIR)/*.h $(INCDIR)/*.hpp))

#In order to make the noroot and withroot libraries we have to do this
#fancy clean noroot and with root for all.

#ifdef ROOTSYS
all:  clean $(LIBTARGET) withroot
#else
#all:  clean $(LIBTARGET) norootcomment
#endif

norootcomment:
	@echo "***************common/Makefile*******************************"
	@echo "* ROOTSYS not defined:(ROOT not installed?)"
	@echo "* libKascadeCommonRoot.a not produced!"
	@echo "*************************************************************"


$(LIBTARGET): $(LIBSOURCES)
	$(AR) r $@ $^
	ranlib $@

$(TARGET_SHARED): $(SHAREDCPPOBJECTS)
	 $(BUILD_SHARED) -o $@ $^

$(TMPDIR)/root_dict_Kascade.cpp: $(addprefix $(INCDIR)/, $(CLASSDEFS))
	$(ROOTCINT) -v -f $@ -c -p $(ROOTCINTFLAGS) $^ include/LinkDef.h


$(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.for
	$(F77) -o $@ -c $(F77FLAGS) $< $(OPTPRF) 

$(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.f90
	$(F90) -o $@ -c $(F90FLAGS) $< $(OPTPRF)

#use below for no root version of common library
$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) $(NOROOTFLAG) -DUSE_RANLUX $< $(OPTPRF) 
#	$(CXX) -o $@ -c $(CFLAGS) $(NOROOTFLAG) $< $(OPTPRF) 


$(addprefix $(TMPDIR)/, $(F90USESOURCES:.f90=.o)): $(TMPDIR)/%.o: \
            $(INCDIR)/%.f90
	$(F90) -o $@ -c $(F90FLAGS) $<  

$(TMPDIR)/root_dict_Kascade.o: $(TMPDIR)/root_dict_Kascade.cpp
	$(CXX) -o $@ -c $(CFLAGS) $< 


.PHONY: clean

clean:
	$(RM) $(LIBTARGET) $(LIBTARGETROOT) \
	        $(addsuffix /*~, $(ALLDIR)) *.mod \
		$(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)) \
		$(addprefix $(TMPDIR)/, $(F90SOURCES:.f90=.o)) \
		$(addprefix $(TMPDIR)/, $(F90USESOURCES:.f90=.o)) \
		$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) \
		$(TMPDIR)/root_dict_Kascade.o \
		$(TMPDIR)/root_dict_Kascade.cpp $(TMPDIR)/root_dict_Kascade.h 
#\
#		out/.depend

#.PHONY: depend

#depend: out/.depend

#out/.depend: $(addprefix $(SRCDIR)/, $(SOURCES))
#	$(CXX) -E -M $(CXXFLAGS) $^ | perl -pe 's/(^[^\s])/'$(TMPDIR)'\/$$1/' ># $@

#include out/.depend


.PHONY: withroot
withroot:
	$(RM) $(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)); \
	$(RM) $(LIBTARGETROOT); \
	export ROOTYES=yes; \
	make $(LIBTARGETROOT) ; \
        export ROOTYES=
