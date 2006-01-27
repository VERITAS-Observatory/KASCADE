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

include ../../Makefile.common

TOPDIR = ../..

OUTDIR=../$(TMPDIR)

F90USESOURCES :=  $(shell echo *.f90)
F90USEOBJECTS = $(addprefix $(OUTDIR)/, $(F90USESOURCES:.f90=.o)) 

all: $(F90USEOBJECTS)

$(addprefix $(OUTDIR)/, $(F90USESOURCES:.f90=.o)): $(OUTDIR)/%.o: %.f90
	$(F90) -o $@ -c $(F90FLAGS) $<


