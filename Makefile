# -----------------------------------------------------------------------------
#
# KASCADE directory makefile
# 
# Original Author: Glenn Sembroski
# $Author$
# $Date$
# $Revision$
# $Tag$
#
# -----------------------------------------------------------------------------
include ./Makefile.common


LIBTARGET = $(LIBDIR)/libKASCADE.a
EXETARGET = kascade.for


F77SOURCES := $(notdir $(shell ls $(SRCDIR)/*.for | grep -v $(EXETARGET)))
#echo F77SOURCES: $(F77SOURCES)

INCLUDES := $(notdir $(shell ls $(INCDIR)/*.h ))
#echo INCLUDES: $(INCLUDES)

EXEOBJECT = $(addprefix $(TMPDIR)/, $(EXETARGET:.for=.o)) 
EXE = $(addprefix $(BINDIR)/, $(EXETARGET:.for=)) 

all: $(LIBTARGET) $(EXEOBJECT) $(EXETARGET)

F77OBJECTS = $(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)) 

$(LIBTARGET): $(OBJECTS)
	$(AR) r $@ $^
	ranlib $@

$(EXETARGET): $(LIBTARGET)
	$(LD) -o $(EXE) $(EXEOBJECT) $^ $(LDFLAGS) $(LIBSLITE) 

$(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.for
	$(F77) -o $@ -c $(F77FLAGS) $<

$(addprefix $(TMPDIR)/, $(EXETARGET:.for=.o)): $(TMPDIR)/%.o: $(SRCDIR)/%.for
	$(F77) -o $@ -c $(F77FLAGS) $<

.PHONY: clean

clean:
	$(RM) $(LIBTARGET) $(addsuffix /*~, $(ALLDIR)) \
		$(addprefix $(TMPDIR)/, $(F77SOURCES:.for=.o)) \
		$(EXEOBJECT) \
		out/.depend \
		$(EXE)

