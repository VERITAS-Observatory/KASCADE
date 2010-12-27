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


LIBTARGET = $(LIBDIR)/libkascade.a

EXETARGET = ksKascade.cpp
EXEBENDTESTTARGET   = BendTest.cpp
EXEATMTESTTARGET   = AtmTest.cpp

LIBBENDTESTTARGET = $(LIBDIR)/libkascadeBendTest.a
BENDTESTOBJECTS   = $(TMPDIR)/bend.o


F77SOURCES := $(notdir $(shell ls $(SRCDIR)/*.for | grep -v $(EXETARGET)))
CPPSOURCES := $(notdir $(shell ls $(SRCDIR)/*.cpp | grep -v $(EXETARGET)| grep -v $(EXEBENDTESTTARGET)| grep -v $(EXEATMTESTTARGET) ))

EXEOBJECT = $(addprefix $(TMPDIR)/, $(EXETARGET:.cpp=.o)) 
EXE = $(addprefix $(BINDIR)/, $(EXETARGET:.cpp=)) 

EXEBENDTESTOBJECT = $(addprefix $(TMPDIR)/, $(EXEBENDTESTTARGET:.cpp=.o)) 
EXEBENDTEST = $(addprefix $(BINDIR)/, $(EXEBENDTESTTARGET:.cpp=)) 

EXEATMTESTOBJECT = $(addprefix $(TMPDIR)/, $(EXEATMTESTTARGET:.cpp=.o)) 
EXEATMTEST = $(addprefix $(BINDIR)/, $(EXEATMTESTTARGET:.cpp=)) 

all: $(LIBTARGET) $(EXEOBJECT) $(EXE)  

bendtest: $(LIBBENDTESTTARGET)  $(BENDTESTOBJECTS) $(EXEBENDTESTOBJECT) $(EXEBENDTEST)

atmtest:  $(EXEATMTESTOBJECT) $(EXEATMTEST)

F77OBJECTS = $(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)) 
CPPOBJECTS = $(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) 

$(LIBTARGET): $(F77OBJECTS) $(CPPOBJECTS)
	$(AR) r $@ $^
	ranlib $@

$(LIBBENDTESTTARGET): $(BENDTESTOBJECTS)
	$(AR) r $@ $^
	ranlib $@


$(EXE): $(LIBTARGET)
	$(F90) -o $(EXE) $(EXEOBJECT) $^ $(KASLDFLAGS) $(KASLIBS) $(CLDFLAGS)

$(EXEBENDTEST): $(LIBBENDTESTTARGET)
	$(F90) -o $(EXEBENDTEST) $(EXEBENDTESTOBJECT) $^ $(KASLDFLAGS) $(CLDFLAGS)

$(EXEATMTEST):
	$(F90) -o $(EXEATMTEST) $(EXEATMTESTOBJECT) $^ $(KASLDFLAGS) $(KASLIBS) $(CLDFLAGS)


$(addprefix $(TMPDIR)/, $(EXETARGET:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) -D_NOROOT $<

$(addprefix $(TMPDIR)/, $(EXEBENDTESTTARGET:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) -D_NOROOT $<

$(addprefix $(TMPDIR)/, $(EXEATMTESTTARGET:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) -D_NOROOT $<


$(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.for
	$(F77) -o $@ -c $(F77FLAGS) $<

$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) -D_NOROOT $<



.PHONY: install

install:
	$(INSTALLCMD) $(EXE) $(TOPDIR)/bin 

.PHONY: clean

clean:
	$(RM) $(LIBTARGET) $(addsuffix /*~, $(ALLDIR)) \
		$(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)) \
		$(addprefix $(TMPDIR)/, $(CPPSOURCES:.cpp=.o)) \
		$(EXEOBJECT)$(EXEBENDTESTOBJECT) $(EXEATMTESTOBJECT) \
		out/.depend \
		$(EXE) $(EXEBENDTEST) $(EXEATMTEST)

