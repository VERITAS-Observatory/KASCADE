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

CPPSOURCES := $(notdir $(shell ls $(SRCDIR)/*.cpp | grep -v $(EXETARGET)))
CPPOBJECTS = $(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) 




#INCLUDES := $(notdir $(shell ls $(INCDIR)/*.h ))

EXEOBJECT = $(addprefix $(TMPDIR)/, $(EXETARGET:.cpp=.o)) 
EXE = $(addprefix $(BINDIR)/, $(EXETARGET:.cpp=)) 
ifdef VEGAS
 all: $(LIBTARGET) $(EXEOBJECT) $(EXETARGET) 
else
 all: noVEGAScomment
endif


$(LIBTARGET): $(CPPOBJECTS)
	@echo CPPOBJECTS: $(CPPOBJECTS)
	$(AR) r $@ $^
	ranlib $@

$(EXETARGET): $(LIBTARGET) 
	$(F77) -o $(addprefix $(BINDIR)/,$(@:.cpp=)) \
	          $(addprefix $(TMPDIR)/,$(@:.cpp=.o)) $^  \
	          $(VEGASLDFLAGS) $(CLDFLAGS) $(KASLDFLAGS) $(KASLIBS) \
	          $(ROOTLIBS) $(VBFLDFLAGS) $(VBFLIBS)
	         

$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(VEGASCMNINCDIR) $(CFLAGS)   $(VBFCFLAGS) $<

# -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS 

$(addprefix $(TMPDIR)/, $(EXETARGET:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(VEGASCMNINCDIR) $(CFLAGS) $(VBFCFLAGS)  $<

.PHONY: install

ifdef VEGAS
#    ifdef ROOTSYS
install:
	$(INSTALLCMD) $(EXE) $(TOPDIR)/bin 
#    else
#install: norootcomment
#    endif
else
install: noVEGAScomment
endif


.PHONY: clean

clean:
	$(RM) $(LIBTARGET) $(addsuffix /*~, $(ALLDIR))  *.mod \
		$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) \
		$(EXEOBJECT) \
		$(EXE)


.PHONY: norootcomment
norootcomment:
	@echo "******************kasaomega/Makefile**************************"
	@echo "* ROOTSYS not defined (ROOT not installed?)"
	@echo "* Can't make or install " $(EXE)
	@echo "**************************************************************"

.PHONY: noVEGAScomment
noVEGAScomment:
	@echo "******************kasaomega/Makefile**************************"
	@echo "* VEGAS not defined (VEGAS not installed?)"
	@echo "* Can't make or install" $(EXE)
	@echo "**************************************************************"
