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

all: $(LIBTARGET) $(EXEOBJECT) $(EXETARGET)

$(LIBTARGET): $(CPPOBJECTS)
	@echo CPPOBJECTS: $(CPPOBJECTS)
	$(AR) r $@ $^
	ranlib $@

$(EXETARGET): $(LIBTARGET) 
	$(F90) -o $(addprefix $(BINDIR)/,$(@:.cpp=)) \
	          $(addprefix $(TMPDIR)/,$(@:.cpp=.o)) $^  \
	          $(CLDFLAGS) $(KASLDFLAGS) $(KASLIBS) $(ROOTLIBS) \
	          $(VEGASLDFLAGS)

$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) $(VEGASCMNINCDIR) $<

$(addprefix $(TMPDIR)/, $(EXETARGET:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS)  $(VEGASCMNINCDIR) $<

.PHONY: install

install:
	$(INSTALLCMD) $(EXE) $(TOPDIR)/bin 


.PHONY: clean

clean:
	$(RM) $(LIBTARGET) $(addsuffix /*~, $(ALLDIR))  *.mod \
		$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) \
		$(EXEOBJECT) \
		$(EXE)

