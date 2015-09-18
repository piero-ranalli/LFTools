INC_FSON=../../libextra/fson/build
LIB_FSON=../../libextra/fson/dist
OPTPARSER=../../libextra/m_option_parser
INCLUDE=-I$(INC_FSON) -I$(OPTPARSER) -I..
BIN=../../bin

FFLAGS=-g $(INCLUDE) $(FFMULTINEST)

FFMULTINEST=-I/usr/local/modules
LDMULTINEST=-L/usr/local/lib64 -lmultinest_mpi

CUBA=-lcuba

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
	FFMULTINEST=-I/Users/piero/SOFTW/MultiNest/modules
	LDMULTINEST=-L/Users/piero/SOFTW/MultiNest/lib -lmultinest
endif

FC=gfortran
FAST=-mtune=native -O3
ZEUS=-march=corei7 -mtune=corei7 -O3

OBJ=priors.o mn_glue.o params.o mn_like.o nestwrap.o lfmnconfig.o main.o 

MOD=$(addsuffix .mod, $(basename $(OBJ)))
SRC=$(addsuffix .f90, $(basename $(OBJ)))

VALIDATEOBJ=priors.o mn_glue.o params.o lfmnconfig.o lfvalidateconfig.o lppd_etc.o lf-validate.o m_option_parser.o
VALIDATEMOD=$(addsuffix .mod, $(basename $(OBJ)))
VALIDATESRC=$(addsuffix .f90, $(basename $(OBJ)))

all: lf-mn lf-validate

include Makefile.depend


%.mod %.o: %.f90
	$(FC)  $(FFLAGS) -c $<

m_option_parser.mod m_option_parser.o: m_option_parser.F90
	$(FC)  $(FFLAGS) -c m_option_parser.F90

test: $(OBJ) $(MOD)
	$(FC) $(INCLUDE)  -o test $(OBJ) ../unit.o $(LIB_FSON)/libfson.so $(LDMULTINEST) $(CUBA)

lf-validate: $(OBJ) $(MOD) $(VALIDATEOBJ) $(VALIDATEMOD) mnunit.o m_option_parser.o
	$(FC) $(FAST) -o lf-validate $(VALIDATEOBJ) ../unit.o $(LIB_FSON)/libfson.so $(CUBA) $(INCLUDE)
	cp lf-validate $(BIN)


lf-mn: $(OBJ) $(MOD) mnunit.o
	$(FC) $(FAST) -o lf-mn main.o mnunit.o ../unit.o $(LIB_FSON)/libfson.so $(CUBA) $(LDMULTINEST) $(INCLUDE)
	cp lf-mn $(BIN)

zeus: $(OBJ) $(MOD) mnzeus.o
	$(FC)  $(ZEUS) -o lf-mn-zeus main.o mnzeus.o ../zeus.o $(LIB_FSON)/libfson.so $(CUBA) $(LDMULTINEST)

.PHONY: clean
clean:
	rm -f lf-mn $(OBJ) $(VALIDATEOBJ) $(MOD) $(VALIDATEMOD)


.PHONY: mnunit
mnunit mnunit.o: mnunit.F90 $(SRC)
	gfortran -c $(FAST) $(INCLUDE) $(FFMULTINEST) mnunit.F90

.PHONY: mnzeus
mnzeus mnzeus.o: mnunit.F90 $(SRC)
	gfortran -o mnzeus.o -c $(ZEUS) $(INCLUDE) $(FFMULTINEST) mnunit.F90


.PHONY: depend
depend Makefile.depend:
	$(FC) -M -cpp $(FFLAGS) $(SRC) > Makefile.depend
