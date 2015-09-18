INC_FSON=../libextra/fson/build
LIB_FSON=../libextra/fson/dist
OPTPARSER=../libextra/m_option_parser
INCLUDE=-I$(INC_FSON) -I$(OPTPARSER)
FFLAGS=-g -ffpe-trap=invalid,zero,overflow $(INCLUDE) $(FAST)
BIN=../bin

#UNAME_S := $(shell uname -s)
# ifeq ($(UNAME_S),Linux)
# 	FFLAGS  += -static
# endif

FC=gfortran
FAST=-mtune=native -O3



OBJ0:=binarysearch.o u_marginal.o curves.o areas.o cosmology.o gehrels.o lumfunc.o photoz.o dataread.o catcorrectoptions.o fileutils.o
OBJ:=$(OBJ0)  lfbinnedconfig.o


MOD:=$(addsuffix .mod, $(basename $(OBJ)))
SRC:=$(addsuffix .f90, $(basename $(OBJ)))

all: lf-catcorrect lf-binned convert-cosmos

include Makefile.depend

%.mod %.o: %.f90
	$(FC)  $(FFLAGS) -g -c $<


$(OPTPARSER)/m_option_parser.mod $(OPTPARSER)/m_option_parser.o: $(OPTPARSER)/m_option_parser.F90
	$(FC)  $(FFLAGS) -c $<


lf-catcorrect: $(OBJ0) lf-catcorrect.f90
	$(FC) $(FAST) -g   -o lf-catcorrect lf-catcorrect.f90 $(OBJ0) $(OPTPARSER)/m_option_parser.o $(FFLAGS)
	cp lf-catcorrect $(BIN)

lf-binned: $(OBJ) lf-binned.f90
	$(FC) $(FAST) -g   -o lf-binned     lf-binned.f90     $(OBJ) $(OPTPARSER)/m_option_parser.o $(FFLAGS)  $(LIB_FSON)/libfson.so
	cp lf-binned $(BIN)

.PHONY: convert
convert convert-cosmos: convert-cosmos.f90 dataread.o binarysearch.o curves.o cosmology.o
	gfortran -o convert-cosmos  convert-cosmos.f90 dataread.o binarysearch.o curves.o cosmology.o
	cp convert-cosmos $(BIN)


.PHONY: clean
clean:
	rm -f ml $(OBJ) $(MOD)


.PHONY: depend
depend Makefile.depend:
	$(FC) -M -cpp $(FFLAGS) $(SRC) > Makefile.depend
