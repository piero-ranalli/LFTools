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
endif

FC=gfortran
FAST=-march=native -mtune=native -O3
ZEUS=-march=corei7 -mtune=corei7 -O3
CUBA=-lcuba -L/usr/local/lib64


OBJ=precision.o cosmology.o fileutils.o binarysearch.o curves.o areas.o u_marginal.o integrals.o lumf_funcs.o photoz.o ml.o  catalogueio.o startup.o lfmlconfig.o 

MOD=$(addsuffix .mod, $(basename $(OBJ)))
SRC=$(addsuffix .f90, $(basename $(OBJ)))



all: lf-ml

include Makefile.depend




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
	gfortran -mtune=native  -O3 -fomit-frame-pointer -fstrict-aliasing  -momit-leaf-frame-pointer -fno-tree-pre -falign-loops -o mlt ml-test.f90 catalogueIO.f90 integrals.f90 cosmology.f90 lumf_funcs.f90 ml.f90 areas.f90 curves.f90 binarysearch.f90 $(CERNLIB)

lf-ml: ml-fit.f90 $(MOD) unit.o $(LIB_FSON)/libfson.so 
	gfortran $(FAST)  -o lf-ml ml-fit.f90 unit.o $(CUBA) $(LIB_FSON)/libfson.so $(CERNLIB)
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
