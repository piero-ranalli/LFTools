INC_FSON=../libextra/fson/build
LIB_FSON=../libextra/fson/dist
OPTPARSER=../libextra/m_option_parser
INCLUDE=-I$(INC_FSON) -I$(OPTPARSER)
BIN=../bin


CERNLIB:=-L/Users/piero/SOFTW/Cernlib/2006/lib/ -lmathlib -lkernlib  -lpacklib

FFLAGS=-g -ffpe-trap=invalid,zero,overflow $(INCLUDE) # -g $(FAST)

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
	CERNLIB:=-L/usr/local/lib64/2006b/x86_64-slc5-gcc43-opt/lib/ -lmathlib -lkernlib -lpacklib
	FAST=-march=native -mtune=native -O3
endif
# don't use -march=native on Darwin, because it throws strange errors when compiling unit.F90
# (seen on OS X Yosemite + GNU Fortran (MacPorts gcc49 4.9.3_0) 4.9.3)
ifeq ($(UNAME_S),Darwin)
	FAST=-mtune=native -O3
endif

FC=gfortran

ZEUS=-march=corei7 -mtune=corei7 -O3

# when set, CUBA make the integrals module use the CUBA library
CUBA=
#CUBA=-lcuba -L/usr/local/lib64 -DCUBA


OBJ=precision.o cosmology.o fileutils.o binarysearch.o curves.o areas.o u_marginal.o integrals.o lumf_funcs.o photoz.o ml.o  catalogueio.o startup.o lfmlconfig.o 

MOD=$(addsuffix .mod, $(basename $(OBJ)))
SRC0=$(addsuffix .f90, $(basename $(OBJ)))
SRC=$(subst integrals.f90,integrals.F90,$(SRC0))


all: lf-ml

include Makefile.depend



%.mod %.o: %.F90
	$(FC)  $(FFLAGS) -c $<

%.mod %.o: %.f90
	$(FC)  $(FFLAGS) -c $<




ml-test.o: ml-test.f90 $(MOD)
	$(FC)  $(FFLAGS) -c ml-test.f90

ml-fit.o: ml-fit.f90 $(MOD)
	$(FC)  $(FFLAGS) -c ml-fit.f90

# cernlib is needed by curves module
mltest: $(OBJ) ml-test.o
	$(FC)  $(FFLAGS)  -o ml ml-test.o $(OBJ) $(CUBA) $(CERNLIB) 

mlfit: $(OBJ) $(MOD) ml-fit.o fson/dist/libfson.so
	$(FC)  $(FFLAGS)    -o ml ml-fit.o  $(OBJ) $(CUBA) fson/dist/libfson.so $(CERNLIB)


fasttest:
	gfortran -mtune=native  -O3 -fomit-frame-pointer -fstrict-aliasing  -momit-leaf-frame-pointer -fno-tree-pre -falign-loops -o mlt ml-test.f90 catalogueIO.f90 integrals.F90 cosmology.f90 lumf_funcs.f90 ml.f90 areas.f90 curves.f90 binarysearch.f90 $(CERNLIB)

lf-ml: ml-fit.f90 $(MOD) unit.o $(LIB_FSON)/libfson.so 
	gfortran $(FAST)  -o lf-ml ml-fit.f90 unit.o $(CUBA) -L$(LIB_FSON) -lfson $(CERNLIB)
	cp lf-ml $(BIN)

.PHONY: unit
unit unit.o: unit.F90 $(SRC)
	gfortran -c $(FAST) $(INCLUDE) unit.F90

.PHONY: zeus
zeus zeus.o: unit.F90 $(SRC)
	gfortran -o zeus.o -c $(ZEUS) $(INCLUDE) unit.F90

.PHONY: clean
clean:
	rm -f lf-ml $(OBJ) $(MOD)


.PHONY: depend
depend Makefile.depend:
	$(FC) -M -cpp $(FFLAGS) $(SRC) > Makefile.depend
