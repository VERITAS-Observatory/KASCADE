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
EXEATMTESTTARGET    = AtmTest.cpp
EXESEGMENTDISPLAYTARGET   = SegmentDisplay.cpp
EXEPHOTONSONGROUNDTARGET = PhotonsOnGround.cpp
EXEPEROOTFILETARGET = PeRootFile.cpp

LIBBENDTESTTARGET = $(LIBDIR)/libkascadeBendTest.a
BENDTESTOBJECTS   = $(TMPDIR)/bend.o


F77SOURCES := $(notdir $(shell ls $(SRCDIR)/*.for | grep -v $(EXETARGET)))
CPPSOURCES := $(notdir $(shell ls $(SRCDIR)/*.cpp | grep -v $(EXETARGET)| grep -v $(EXEBENDTESTTARGET)| grep -v $(EXEATMTESTTARGET) |  grep -v $(EXESEGMENTDISPLAYTARGET) |grep -v $(EXEPHOTONSONGROUNDTARGET) | grep -v $(EXEPEROOTFILETARGET) ))

EXEOBJECT = $(addprefix $(TMPDIR)/, $(EXETARGET:.cpp=.o)) 
EXE = $(addprefix $(BINDIR)/, $(EXETARGET:.cpp=)) 

EXEBENDTESTOBJECT = $(addprefix $(TMPDIR)/, $(EXEBENDTESTTARGET:.cpp=.o)) 
EXEBENDTEST = $(addprefix $(BINDIR)/, $(EXEBENDTESTTARGET:.cpp=)) 

EXEATMTESTOBJECT = $(addprefix $(TMPDIR)/, $(EXEATMTESTTARGET:.cpp=.o)) 
EXEATMTEST = $(addprefix $(BINDIR)/, $(EXEATMTESTTARGET:.cpp=)) 

EXESEGMENTDISPLAYOBJECT = $(addprefix $(TMPDIR)/, $(EXESEGMENTDISPLAYTARGET:.cpp=.o)) 
EXESEGMENTDISPLAY = $(addprefix $(BINDIR)/, $(EXESEGMENTDISPLAYTARGET:.cpp=)) 

EXEPHOTONSONGROUNDOBJECT = $(addprefix $(TMPDIR)/, $(EXEPHOTONSONGROUNDTARGET:.cpp=.o)) 
EXEPHOTONSONGROUND = $(addprefix $(BINDIR)/, $(EXEPHOTONSONGROUNDTARGET:.cpp=)) 

EXEPEROOTFILEOBJECT = $(addprefix $(TMPDIR)/, $(EXEPEROOTFILETARGET:.cpp=.o)) 
EXEPEROOTFILE = $(addprefix $(BINDIR)/, $(EXEPEROOTFILETARGET:.cpp=)) 

all: $(LIBTARGET) $(EXEOBJECT) $(EXE)  

bendtest: $(LIBBENDTESTTARGET)  $(BENDTESTOBJECTS) $(EXEBENDTESTOBJECT) $(EXEBENDTEST)

atmtest:  $(EXEATMTESTOBJECT) $(EXEATMTEST)

SegmentDisplay:  $(EXESEGMENTDISPLAYOBJECT) $(EXESEGMENTDISPLAY)

PhotonsOnGround: $(EXEPHOTONSONGROUNDOBJECT) $(EXEPHOTONSONGROUND)

PeRootFile: $(EXEPEROOTFILEOBJECT) $(EXEPEROOTFILE)

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


$(EXESEGMENTDISPLAY):
	$(F90) -o $(EXESEGMENTDISPLAY) $(EXESEGMENTDISPLAYOBJECT) $^ $(KASLDFLAGS) $(KASLIBS) $(CLDFLAGS) $(ROOTLIBS)


$(EXEPHOTONSONGROUND):
	$(F90) -o $(EXEPHOTONSONGROUND) $(EXEPHOTONSONGROUNDOBJECT) $^ $(KASLDFLAGS) $(KASLIBS) $(CLDFLAGS) $(ROOTLIBS)


$(EXEPEROOTFILE):
	$(F90) -o $(EXEPEROOTFILE) $(EXEPEROOTFILEOBJECT) $^ $(KASLDFLAGS) $(KASLIBS) $(CLDFLAGS) $(ROOTLIBS)



$(addprefix $(TMPDIR)/, $(EXETARGET:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) -D_NOROOT $<

$(addprefix $(TMPDIR)/, $(EXEBENDTESTTARGET:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) -D_NOROOT $<

$(addprefix $(TMPDIR)/, $(EXEATMTESTTARGET:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) -D_NOROOT $<

$(addprefix $(TMPDIR)/, $(EXESEGMENTDISPLAYTARGET:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) -D_NOROOT $<

$(addprefix $(TMPDIR)/, $(EXEPHOTONSONGROUNDTARGET:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -o $@ -c $(CFLAGS) -D_NOROOT $<

$(addprefix $(TMPDIR)/, $(EXEPEROOTFILETARGET:.cpp=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.cpp
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
		$(EXEOBJECT) $(EXEBENDTESTOBJECT) $(EXEATMTESTOBJECT) \
		$(EXESEGMENTDISPLAYOBJECT) $(EXEPHOTONSONGROUNDOBJECT) \
		out/.depend \
		$(EXE) $(EXEBENDTEST) $(EXEATMTEST) $(EXESEGMENTDISPLAY) \
		$(EXEPHOTONSONGROUND) $(EXEPEROOTFILE)


