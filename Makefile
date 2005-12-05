# Makefile for Purdue Kascade Simulation Software
# Written by G.H.Sembroski
# For IDA.

# Unresolved ref to __MAIN from use of f2c library. Don't need it for anything
# as far as I can tell.

# Cern libraires need -lnsl (for yp* references), -lg2c (for lots of stuff),0
# and -lm

#In C++ code include <getopt.h> for command line stuff.
#In Fortran code include libU77 library. It has getarg and iargc

#Be sure to use the new 2001 cernlibraies



#debugA=A
#debugB=B





F77  =f77 
F90  =f90
CC   = gcc
CCC  = g++

#Absoft Cern lib compatable flags:
F90CERNFLAGS = -O  -B108 -YEXT_NAMES=LCS -YCFRL=1 -c -s -YCOM_SFX=_ \
 -YCOM_NAMES=LCS -YCOM_PFX= 
F77CERNFLAGS = -O  -V -f -N3 -B108 -N90 -C -c -s -W -N22

GDFFLAGS = -O -c -s -YCFRL=1 -YEXT_NAMES=LCS -YCOM_NAMES=LCS -YCOM_SFX=_ \
   -YCOM_PFX=  -B108

#Debug Flags
#F90FLAGS = -g  -c -s -YEXT_NAMES=LCS
#F77FLAGS = -g  -V -W -f -N3 -C -c -N1 -s 
#CFLAGS = -g -Wall -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -D_REENTRANT

#Standard (Normal flags to use)
F90FLAGS = -g -O -c -s -YEXT_NAMES=LCS
F77FLAGS = -g -O -V  -W -f -N3 -C -c -N1 -s
CFLAGS   = -g -O -Wall -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -D_REENTRANT

#debug for CERN LIBS
#F90FLAGS = -g  -B108 -YEXT_NAMES=LCS -YCFRL=1 -c -s -YCOM_SFX=_ \
# -YCOM_NAMES=LCS -YCOM_PFX= 
#F77FLAGS = -g  -V -f -N3 -B108 -N90 -C -c -s -W -N22

#INCSRC= -I/home/sembroski/source
LIB_PREFIX = /home/sembroski/local/src

# HDF5 (1.4.5) on ida.
#LIBHDF5 = -L/usr/local/lib
#INCHDF5 = -I/usr/local/include
#HDF5 = -lhdf5 -lz

#HDF5 1.6.1-post10(or whatever is installed locally) on amdahl
LIBHDF5 = -L/home/sembrosk/local/lib
INCHDF5 = -I/home/sembrosk/local/include
HDF5 = -lhdf5 -lz

#VERITAS Havester code on amdahl
VERITAS = /home/sembrosk/local/src/VERITAS
INCVERITAS = -I$(VERITAS)
LIBVERITAS = -L$(VERITAS)/lib
	gaeaSerial=-lserialization
	gaeaDefs=-D_GAEA

#VERITAS ida
#VERITAS = /usr/local/veritas
#INCVERITAS = -I$(VERITAS)/include
#LIBVERITAS = -L$(VERITAS)/lib

VLIBS =  $(gaeaSerial) -ldast -ldatastream -larrayevent -lfpio -lrunbuilder \
	-ldatum  -ldast -lvglobal $(gaeaSerial)

#CERN
LIBCERN = -L/cern/2001
CERN = -lmathlib -lpacklib -lkernlib

# HDF4
LIBHDF4 = -L/usr/local/hdf/lib
INCHDF4 = -I/usr/local/hdf/include
HDF4 = -ldf -ljpeg -lz

# granite data format 
LIBGDF = -L$(LIB_PREFIX)/gdf/vdev
INCGDF = -I$(LIB_PREFIX)/gdf/vdev
GDF = -lgdf 

# System dependent libraries
LIBS = -L. -lfio -lf77math -lf90math -lg2c  -lpthread 

# C++ stuf.
#CCLIB = -L/usr/lib/gcc-lib/i386-redhat-linux/egcs-2.91.66
#CCLIB = -L/opt/gcc-3.0.4/i686-pc-linux-gnu/libstdc++-v3/src/.libs
#CCLIB = -L/usr/gcc-3.3/lib
CCLIB=-L/usr/lib

#VHDF5  directory

#ida,gaea
#VHDF5DIR = /home/sembroski/VHDF5
#amdahl
VHDF5DIR = /home/sembrosk/local/src/VHDF5/
INCVHDF5=-I$(VHDF5DIR)

# starlink - SLA  (may need -lg2c for d_mod)
LIB_PREFIX = /home/sembrosk/local/libsrc
LIBSTAR = -L$(LIB_PREFIX)/star-i686/lib

all: kascade kaslite kassrtmrg kastrigger m2hdf5 kasSumMHDF5 kasaomega \
     cmsum_bin2ntpl kasArrayTrigger

kascade: datetime.o  Random.o ranlux.o kasnuc_lib.o kasatm.o atmosphere76.o \
	 kasmultscatt.o MultipleCoulombScattering.o kascade.o  
	$(F90) datetime.o  Random.o ranlux.o kasnuc_lib.o kasatm.o \
               atmosphere76.o kasmultscatt.o MultipleCoulombScattering.o \
               kascade.o\
        -o kascade \
	$(CCLIB)  -lstdc++ \
	-lU77 

kaslite: datetime.o ranlux.o  kasatm.o atmosphere76.o kaslite.o  
	$(F90) datetime.o ranlux.o  kasatm.o \
	       atmosphere76.o kaslite.o \
        -o kaslite \
	$(CCLIB)  -lstdc++ \
        -lU77 

kassrtmrg: datetime.o  kashdf5.o kassrtmrg.o
	$(F77) datetime.o  kashdf5.o kassrtmrg.o \
	-o kassrtmrg \
	$(LIBHDF5) $(HDF5) \
	$(CCLIB)  -lstdc++ \
	-lU77 \
	-lf90math 

benchmark:   benchmark.o
	$(F77)  benchmark.o \
	-o benchmark \
	$(CCLIB)  -lstdc++ \
	-lU77 \
	-lf90math 

kastrigger: structures.o wcamera_modules.o \
            whipple_telescope.o kastrigger_modules.o \
            kasaomega_def.o \
	    w10m_f77.o w10m_subs.o w10m_pepulse.o \
            Random.o ranlux.o \
	    VHDF5RunDataFile.o VHDF5KascadeRunDataFile.o \
	    Veritas.o VPulse.o  spectrumWeights.o \
	    VHDF5Event.o VHDF5Param.o VHDF5Kascade.o \
	    MHDF5SimDataFile.o exception.o  MHDF5Subs.o  \
	    kashdf5.o wcamera.o \
            VPattern.o VUTCGPS.o \
	    eventWeights.o \
	    kastrigger_sngl.o kastrigger.o
	$(F90) w10m_f77.o \
        structures.o \
	wcamera_modules.o spectrumWeights.o kastrigger_sngl.o \
        whipple_telescope.o kastrigger_modules.o \
	MHDF5SimDataFile.o exception.o  MHDF5Subs.o \
	kashdf5.o wcamera.o \
	kasaomega_def.o \
        w10m_subs.o  \
	w10m_pepulse.o \
        Random.o ranlux.o \
	VHDF5Event.o VHDF5Param.o  VHDF5Kascade.o \
	VHDF5RunDataFile.o VHDF5KascadeRunDataFile.o \
        VPattern.o VUTCGPS.o  \
        eventWeights.o \
        VPulse.o  Veritas.o  \
	kastrigger.o \
	-o kastrigger \
	-O -YCFRL=1 \
        $(LIBVERITAS) $(VLIBS) \
	$(LIBHDF5) $(HDF5) \
	$(LIBSTAR) -lsla \
	$(CCLIB)  -lstdc++ \
	-lU77 -lg2c

m2hdf5: kashdf5.o structures.o m2hdf5.o \
	ranlux.o \
	w10m_f77.o 
	$(F90) m2hdf5.o kashdf5.o structures.o \
	w10m_f77.o \
	ranlux.o \
	-o m2hdf5 \
	$(LIBHDF5) $(HDF5) \
	$(CCLIB)  -lstdc++ \
	-lU77 

kasSumMHDF5:  Random.o ranlux.o MHDF5SimDataFile.o exception.o  \
             kasSumMHDF5.o  eventWeights.o spectrumWeights.o 
	$(F90)  Random.o ranlux.o MHDF5SimDataFile.o eventWeights.o \
          spectrumWeights.o exception.o kasSumMHDF5.o  \
        -o kasSumMHDF5 \
	$(LIBHDF5) $(HDF5) \
	$(CCLIB)  -lstdc++ \
	-lU77 -lg2c 


kasaomega:  w10m_f77.o structures.o whipple_telescope.o \
	    wcamera_modules.o kastrigger_modules.o \
            w10m_subs.o \
	    kasaomega_def.o \
            Random.o ranlux.o \
	    VHDF5Kascade.o VHDF5Event.o VHDF5Param.o \
	    VPattern.o VUTCGPS.o VHDF5RunDataFile.o \
            VHDF5KascadeRunDataFile.o VPulse.o Veritas.o  \
	    w10m_pepulse.o \
	    kashdf5.o wcamera.o \
	    queue.o \
	    kasaomega_cmn_sub.o \
            kasaomega_sngl.o \
	    eventWeights.o spectrumWeights.o \
	    W10m.o Whipple.o \
	    kasaomega.o

	$(F90)  VPattern.o structures.o whipple_telescope.o \
	wcamera_modules.o kastrigger_modules.o kasaomega_def.o \
	kashdf5.o wcamera.o \
        w10m_f77.o \
        Random.o ranlux.o \
	queue.o w10m_pepulse.o \
	VHDF5Kascade.o VHDF5Event.o VHDF5Param.o \
	 VUTCGPS.o VHDF5RunDataFile.o \
	VHDF5KascadeRunDataFile.o VPulse.o Veritas.o  \
	kasaomega_cmn_sub.o \
	w10m_subs.o \
	W10m.o Whipple.o \
	kasaomega_sngl.o \
        eventWeights.o spectrumWeights.o \
	kasaomega.o \
        -o kasaomega \
	-O -YCFRL=1 \
	-lU77 \
        $(LIBVERITAS) $(VLIBS) \
	$(LIBCERN) $(CERN) \
	$(LIBHDF5) $(HDF5) \
	$(LIBSTAR) -lsla \
	$(CCLIB)  -lstdc++ \
	$(LIBS) \
	-lg2c \
	-lnsl \
	-lm 


cmsum_bin2ntpl: structures_cern.o cmsum_bin2ntpl.o
	$(F90) structures_cern.o cmsum_bin2ntpl.o \
	-o cmsum_bin2ntpl \
	-O -YCFRL=1 \
	-lU77 \
	$(LIBCERN) $(CERN) \
	$(CCLIB)  -lstdc++ \
	-lg2c \
	-lnsl \
	-lm 

kasArrayTrigger: kasArrayTrigger.o kasArrayTriggerClasses.o Random.o ranlux.o \
                 VPattern.o VUTCGPS.o \
		 VHDF5Event.o VHDF5Param.o  VHDF5Kascade.o \
		 VHDF5RunDataFile.o VHDF5KascadeRunDataFile.o \
                 exception.o
	$(CCC)  Random.o ranlux.o exception.o \
	VPattern.o VUTCGPS.o \
	VHDF5Event.o VHDF5Param.o  VHDF5Kascade.o \
	VHDF5RunDataFile.o VHDF5KascadeRunDataFile.o \
	RandomVVV.o \
	kasArrayTriggerClasses.o \
	kasArrayTrigger.o \
	-o kasArrayTrigger \
	$(CCLIB)  -lstdc++ \
	$(LIBHDF5) $(HDF5) 

############################################################################

eventWeightTest:   eventWeightTest.o eventWeights.o 
	$(CCC) eventWeights.o eventWeightTest.o \
        -o eventWeightTest \
	$(CCLIB)  -lstdc++ \
	-lnsl \
	-lm 


pst_file_gen:	pst_file_gen.o
	$(F90) pst_file_gen.o \
	-o pst_file_gen \
	-O -YCFRL=1 

spotsize: structures.o ranlux.o whipple_telescope.o  \
	kastrigger_modules.o w10m_f77.o \
	spotsize.o 
	$(F90) structures.o ranlux.o whipple_telescope.o \
	kastrigger_modules.o \
	w10m_f77.o \
	spotsize.o \
	-o spotsize \
	-O -YCFRL=1 

# glenn is generic compile and link
# use it for quicky copiles and links.
glenn:	glenn.o
	$(F90) glenn.o \
	-o glenn \
	-O -YCFRL=1 

#atm76
gmsydstst76:	gmsydstst.o kasatm.o atmosphere76.o
	$(F90) kasatm.o atmosphere76.o gmsydstst.o \
	-o gmsydstst76 \
	-O -YCFRL=1 \
	$(CCLIB)  -lstdc++

#atm68
gmsydstst68:	gmsydstst.o kasatm68.o 
	$(F90) kasatm68.o  gmsydstst.o \
	-o gmsydstst68 \
	-lU77

#MultipleCoulombScattering test program
multscattest:	 Random.o ranlux.o \
	         coulomb_scattering_ini.o spline.o \
                 coulomb_scattering.o kasmultscatt.o \
                MultipleCoulombScattering.o kasmultscattk.o multscattest.o
	$(F90)  Random.o ranlux.o \
                coulomb_scattering_ini.o spline.o \
	        coulomb_scattering.o kasmultscatt.o \
                MultipleCoulombScattering.o  multscattest.o \
	-o multscattest \
	-O -YCFRL=1 \
	$(CCLIB)  -lstdc++ \
	-lU77

 
ecal:	ecal.o
	$(F90) ecal.o \
	-o ecal \
	-O -YCFRL=1 

hdf2ntp:   structures_cern.o hdf2ntp.o 
	   $(F90) structures_cern.o hdf2ntp.o  \
	-o hdf2ntp \
	-O -YCFRL=1 \
	$(LIBCERN) $(CERN) \
	$(LIBHDF4) $(HDF4) \
	-lg2c \
	-lnsl \
	-lm 

outhdf2fz: outhdf2fz.o psthdf5.o
	   $(CC) outhdf2fz.o $(CFLAGS) \
	   -o outhdf2fz \
	$(LIBGDF) $(GDF) \
	$(LIBCERN) $(CERN) \
	$(LIBHDF4) $(HDF4) \
	$(LIBS) -lnsl -lm

pes2hdf5: pes2hdf5.o kashdf5.o
	$(F90) pes2hdf5.o kashdf5.o \
	-o pes2hdf5_$(arch) \
	$(LIBHDF5) $(HDF5) \
	$(CCLIB)  -lstdc++ \
	-lg2c \
	-lnsl \
	-lm 


pst_mult2hdf5: pst_mult2hdf5.o psthdf5.o
	$(F90) pst_mult2hdf5.o psthdf5.o \
	-o pst_mult2hdf5 \
	$(LIBHDF5) $(HDF5) \
	-lm

nflucttest:ranlux.o nflucttest.o
	$(F77) ranlux.o nflucttest.o \
	-o nflucttest \
	-O -YCFRL=1 


#####################################################################

strip3: strip3.o
	$(CCC) strip3.o \
	-o strip3 \
	$(LIBSTAR) -lsla \
	-L. -lg2c \
	$(CCLIB)  -lstdc++ \
	-lnsl \
	-lm 

randomCreateRanluxSeed:  ranlux.o randomCreateRanluxSeed.o 
	$(F90) ranlux.o randomCreateRanluxSeed.o  \
        -o randomCreateRanluxSeed \
	$(CCLIB)  -lstdc++ \
	-lU77 -lg2c 



strip3.o: strip3.cpp
	$(CCC) strip3.cpp $(CFLAGS) -c 

randomCreateRanluxSeed.o: randomCreateRanluxSeed.cpp
	$(CCC) randomCreateRanluxSeed.cpp $(CFLAGS) -c 

kasnuc_lib.o: kasnuc_lib.for
	$(F77) kasnuc_lib.for $(F77FLAGS)

nflucttest.o: nflucttest.for
	$(F77) nflucttest.for $(F77FLAGS)

fpni.o: fpni.for
	$(F90) fpni.for $(F90FLAGS)

glenn.o: glenn.f90
	$(F90) glenn.f90 $(F90FLAGS)
glenn1.o: glenn1.f90
	$(F90) glenn1.f90 $(F90FLAGS)

gmsydstst.o: gmsydstst.f90 gmsydstst.inc 
	$(F90) gmsydstst.f90 $(F90FLAGS)

multscattest.o: multscattest.f90 
	$(F90) multscattest.f90 $(F90FLAGS)

kasatm.o: kasatm.cpp
	$(CCC) kasatm.cpp $(CFLAGS) -c 

kasatm68.o: kasatm.for
	$(F77) kasatm.for $(F77FLAGS) -c -o kasatm68.o 

atmosphere76.o: atmosphere76.cpp atmosphere76.h
	$(CCC) atmosphere76.cpp $(CFLAGS) -c 

kasmultscatt.o: kasmultscatt.cpp
	$(CCC) kasmultscatt.cpp $(CFLAGS) -c 

kasmultscattk.o: kasmultscatt.for
	$(F77) kasmultscatt.for $(F77FLAGS) -o kasmultscattk.o

MultipleCoulombScattering.o: MultipleCoulombScattering.cpp \
	                     MultipleCoulombScattering.h
	$(CCC) MultipleCoulombScattering.cpp $(CFLAGS) -c 

spline.o: spline.for
	$(F77) spline.for $(F77FLAGS)

coulomb_scattering_ini.o: coulomb_scattering_ini.for
	$(F77) coulomb_scattering_ini.for $(F77FLAGS)

coulomb_scattering.o: coulomb_scattering.for
	$(F77) coulomb_scattering.for $(F77FLAGS)

mscat.o: mscat.f90
	$(F90) mscat.f90 $(F90FLAGS)



ecal.o: ecal.f90
	$(F90) ecal.f90 $(F90FLAGS)
charge.o: charge.f90
	$(F90) charge.f90 $(F90FLAGS)

spotsize.o: spotsize.f90
	$(F90) spotsize.f90 $(F90FLAGS)

kascade.o: kascade.for
	$(F77) kascade.for $(F77FLAGS)
kaslite.o: kaslite.for
	$(F77) kaslite.for $(F77FLAGS) -N51
kassrtmrg.o: kassrtmrg.for
	$(F77) kassrtmrg.for $(F77FLAGS) -N51

benchmark.o: benchmark.for
	$(F77) benchmark.for $(F77FLAGS) -N51

# -N51 Recl in i*4 words (f77 only)

kastrigger.o: kastrigger.f90 wcamera_modules.o whipple_telescope.o structures.o
	$(F90) kastrigger.f90 $(F90FLAGS)
kastrigger_mpi.o: kastrigger_mpi.f90
	$(F90) kastrigger_mpi.f90 $(F90FLAGS) 
kastrigger_sngl.o: kastrigger_sngl.f90 wcamera_modules.o structures.o
	$(F90) kastrigger_sngl.f90 $(F90FLAGS)

# -N51 Recl in i*4 words

kasSumMHDF5.o: kasSumMHDF5.cpp MHDF5SimDataFile.h
	$(CCC) kasSumMHDF5.cpp $(CFLAGS) -c \
	$(INCHDF5) -DUSE_RANLUX

eventWeightTest.o: eventWeightTest.cpp WSumInit.h eventWeights.h
	$(CCC) eventWeightTest.cpp $(CFLAGS) -c 

kassum_compress.o: kassum_compress.f90
	$(F90) kassum_compress.f90 $(F90FLAGS) 

kassum_hdf5.o: kassum_hdf5.f90 structures.o
	$(F90) kassum_hdf5.f90 $(F90FLAGS) 

kasaomega.o: kasaomega.f90 wcamera_modules.o whipple_telescope.o \
             structures.o kasaomega_def.o
	$(F90) kasaomega.f90 $(F90FLAGS) 
kasaomega_def.o: kasaomega_def.f90 structures.o
	$(F90) kasaomega_def.f90 $(F90FLAGS)
kasaomega_mpi.o: kasaomega_mpi.f90
	$(F90) kasaomega_mpi.f90 $(F90FLAGS)
kasaomega_sngl.o: kasaomega_sngl.f90 wcamera_modules.o kasaomega_cmn_sub.o \
		  structures.o  kasaomega_def.o
	$(F90) kasaomega_sngl.f90 $(F90FLAGS)
kasaomega_cmn_sub.o: kasaomega_cmn_sub.f90 wcamera_modules.o structures.o \
		     kasaomega_def.o
	$(F90) kasaomega_cmn_sub.f90 $(F90FLAGS)

kasArrayTrigger.o: exception.o \
                   $(VHDF5DIR)/VHDF5Kascade.h \
		   $(VHDF5DIR)/VHDF5KascadeRunDataFile.o \
                   $(VHDF5DIR)/VPattern.h \
                   $(VHDF5DIR)/VHDF5RunDataFile.h \
                   kasArrayTriggerClasses.h \
		   $(VHDF5DIR)/VUTCGPS.h Random.h kasArrayTrigger.cpp
	$(CCC) kasArrayTrigger.cpp $(CFLAGS) -c \
	$(INCHDF5)  $(INCVHDF5)

kasArrayTriggerClasses.o: exception.o \
                   $(VHDF5DIR)/VHDF5Kascade.h \
		   $(VHDF5DIR)/VHDF5KascadeRunDataFile.o \
		   $(VHDF5DIR)/VPattern.h \
                   $(VHDF5DIR)/VHDF5RunDataFile.h \
                   kasArrayTriggerClasses.h \
		   $(VHDF5DIR)/VUTCGPS.h Random.h \
                   kasArrayTriggerClasses.cpp
	$(CCC) kasArrayTriggerClasses.cpp $(CFLAGS) -c \
	$(INCHDF5)  $(INCVHDF5)

cmsum_bin2ntpl.o: cmsum_bin2ntpl.f90 structures_cern.o
	$(F90) cmsum_bin2ntpl.f90 $(F90CERNFLAGS)

datetime.o: datetime.f90
	$(F90) datetime.f90 $(F90FLAGS)

#kashdf5.o: kashdf5.c kashdf5.h
#	$(CCC) kashdf5.c $(CFLAGS) -c \
#	$(INCSRC) \
#	$(INCHDF5) 

kashdf5.o: kashdf5.cpp kashdf5.h
	$(CCC) kashdf5.cpp $(CFLAGS) -c \
	$(INCSRC) \
	$(INCHDF5) 

gdf.o: gdf.f
	$(F90) gdf.f -c $(GDFFLAGS)

hdf_test.o: hdf_test.c
	$(CC) hdf_test.c $(CFLAGS) -c \
	$(INCSRC) \
	$(INCHDF5) 


hdf5_test.o: hdf5_test.c
	$(CC) hdf5_test.c $(CFLAGS) -c \
	$(INCSRC) \
	$(INCHDF5) 

kastrigger_modules.o: kastrigger_modules.f90 structures.o
	$(F90) kastrigger_modules.f90 $(F90FLAGS) 
image.o: image.f90
	$(F90) image.f90 $(F90FLAGS)

m2hdf5.o: m2hdf5.f90 structures.o
	$(F90) m2hdf5.f90 $(F90FLAGS)
rgdf.o: rgdf.for
	$(F90) rgdf.for -c $(GDFFLAGS)

hdf2ntp.o: hdf2ntp.f90 structures_cern.o
	$(F90) hdf2ntp.f90 $(F90CERNFLAGS)

MHDF5SimDataFile.o: MHDF5SimDataFile.h MHDF5SimDataFile.cpp
	$(CCC) MHDF5SimDataFile.cpp $(CFLAGS) -c \
	$(INCHDF5)

MHDF5Subs.o: MHDF5Subs.cpp
	$(CCC) MHDF5Subs.cpp $(CFLAGS) -c \
	$(INCHDF5)

outhdf2fz.o: outhdf2fz.c
	$(CC) outhdf2fz.c $(CFLAGS) -c $(INCHDF4) $(INCGDF)
pes2hdf5.o: pes2hdf5.f90 structures.o
	$(F90) pes2hdf5.f90 $(F90FLAGS)

psthdf5.o: psthdf5.c
	$(CC) psthdf5.c $(CFLAGS) -c \
	$(INCSRC) \
	$(INCHDF5) 

pst_mult2hdf5.o: pst_mult2hdf5.f90
	$(F90) pst_mult2hdf5.f90 $(F90FLAGS)
pst_file_gen.o: pst_file_gen.f90
	$(F90) pst_file_gen.f90 $(F90FLAGS)
queue.o: queue.f90 structures.o
	$(F90) queue.f90 $(F90FLAGS) 
Random.o: Random.cpp
	$(CCC) Random.cpp  -c -DUSE_RANLUX

#RandomVVV.o: $(VHDF5DIR)/Random.cpp
#	$(CCC) $(VHDF5DIR)/Random.cpp  -c $(CFLAGS) -o RandomVVV.o

RandomVVV.o: Random.cpp
	$(CCC) Random.cpp  -c $(CFLAGS) -o RandomVVV.o

ranlux_mpi.o: ranlux_mpi.for
	$(F77) ranlux_mpi.for $(F77FLAGS)

ranlux_sngl.o: ranlux_sngl.for
	$(F77) ranlux_sngl.for $(F77FLAGS) 

ranlux.o: ranlux.for
	$(F77) ranlux.for $(F77FLAGS) 

structures.o: structures.f90
	$(F90) structures.f90 $(F90FLAGS) 

structures_cern.o: structures_cern.f90
	$(F90) structures_cern.f90 $(F90CERNFLAGS) 

#fpio.o:	fpio.h fpio.cpp
#	$(CCC) fpio.cpp  -c  
#datum.o: datum.cpp datum.h
#	$(CCC) datum.cpp  -c  
#datum_file_handler.o: datum_file_handler.cpp datum.o datum_file_handler.h
#	$(CCC) datum_file_handler.cpp  -c  

ShowVTaggedEvent.o: ShowVtaggedEvent.cpp VTaggedEvent.h datum_file_handler.o \
	           VPulse.o 
	$(CCC) ShowVtaggedEvent.cpp  -c  

VTaggedEvent.o: VTaggedEvent.cpp datum.o VTaggedEvent.h
	$(CCC) VTaggedEvent.cpp  -c  

VEventD.o: VEventD.cpp datum.o VEventD.h
	$(CCC) VEventD.cpp  -c  

Veritas.o: Veritas.cpp VPulse.o VHDF5KascadeRunDataFile.o \
           Pulse_defs.h
	$(CCC) Veritas.cpp  $(CFLAGS) -c $(INCVERITAS) $(INCHDF5) $(INCVHDF5)


VPulse.o: VPulse.cpp VPulse.h Pulse_defs.h
	$(CCC) VPulse.cpp  -c -Wall $(CFLAGS)


VPattern.o: $(VHDF5DIR)/VPattern.cpp
	$(CCC) $(VHDF5DIR)/VPattern.cpp $(CFLAGS) -c 

exception.o: exception.cpp exception.h
	$(CCC) exception.cpp $(CFLAGS) -c 

eventWeights.o:eventWeights.cpp
	$(CCC) eventWeights.cpp $(CFLAGS) -c 

spectrumWeights.o:spectrumWeights.cpp WSumInit.h
	$(CCC) spectrumWeights.cpp $(CFLAGS) -c 

VUTCGPS.o:$(VHDF5DIR)/VUTCGPS.cpp
	$(CCC) $(VHDF5DIR)/VUTCGPS.cpp $(CFLAGS) -c

VHDF5Event.o:$(VHDF5DIR)/VHDF5Event.cpp $(VHDF5DIR)/VHDF5Event.h
	$(CCC) $(VHDF5DIR)/VHDF5Event.cpp $(CFLAGS) -c $(INCVHDF5) \
	$(INCHDF5) 

VHDF5Param.o:$(VHDF5DIR)/VHDF5Param.cpp $(VHDF5DIR)/VHDF5Param.h
	$(CCC) $(VHDF5DIR)/VHDF5Param.cpp $(CFLAGS) -c $(INCVHDF5) \
	$(INCHDF5) 

VHDF5Kascade.o:$(VHDF5DIR)/VHDF5Kascade.cpp $(VHDF5DIR)/VHDF5Kascade.h
	$(CCC) $(VHDF5DIR)/VHDF5Kascade.cpp $(CFLAGS) -c $(INCVHDF5) \
	$(INCHDF5) 

VHDF5RunDataFile.o: $(VHDF5DIR)/VHDF5RunDataFile.cpp \
		    $(VHDF5DIR)/VHDF5RunDataFile.h 
	$(CCC)  $(VHDF5DIR)/VHDF5RunDataFile.cpp  \
	$(CFLAGS) -c \
	$(INCVERITAS) \
	$(INCHDF5) 

VHDF5KascadeRunDataFile.o: $(VHDF5DIR)/VHDF5KascadeRunDataFile.cpp \
		$(VHDF5DIR)/VHDF5KascadeRunDataFile.h \
                $(VHDF5DIR)/VHDF5RunDataFile.cpp \
                $(VHDF5DIR)/VHDF5RunDataFile.h \
		$(VHDF5DIR)/VHDF5Kascade.h $(VHDF5DIR)/VHDF5Param.h \
	        $(VHDF5DIR)/VHDF5Event.h 
	$(CCC) $(VHDF5DIR)/VHDF5KascadeRunDataFile.cpp $(CFLAGS) -c \
	$(INCVERITAS)  $(INCVHDF5) \
	$(INCHDF5) 

vms_linux.o: vms_linux.f90
	$(F90) vms_linux.f90 $(F90FLAGS) 
wcamera.o: wcamera$(debugA).f90 wcamera_modules.o whipple_telescope.o \
	   structures.o
	$(F90) wcamera$(debugA).f90 $(F90FLAGS) -o wcamera.o 

wcamera_modules.o: wcamera_modules.f90
	$(F90) wcamera_modules.f90 $(F90FLAGS)
whipple_telescope.o: whipple_telescope.f90
	$(F90) whipple_telescope.f90 $(F90FLAGS) 

w10m_f77.o: w10m_f77.for
	$(F77) w10m_f77.for $(F77FLAGS) 

w10m_pepulse.o: w10m_pepulse$(debugB).f90 wcamera_modules.o structures.o \
                kasaomega_def.o
	$(F90) w10m_pepulse$(debugB).f90 $(F90FLAGS) -o w10m_pepulse.o


w10m_subs.o: w10m_subs.f90 wcamera_modules.o whipple_telescope.o structures.o
	$(F90) w10m_subs.f90 $(F90FLAGS) 
W10m.o:	W10m.cpp W10m.h
	$(CCC) W10m.cpp -c -Wall $(CFLAGS)
Whipple.o: Whipple.cpp
	$(CCC) Whipple.cpp   -c -Wall $(CFLAGS)


clean:
	rm -f *.o  *~ "#"* core* kascade kaslite kassrtmrg kastrigger m2hdf5 \
		kasaomega cmsum_bin2ntpl kassum_hdf5 hdf2ntp ShowTaggedEvent \
                kasArrayTrigger kasSumMHDF5 benchmark;








