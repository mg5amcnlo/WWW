#############################################################################
# 									    #
# Makefile to compile and link FORTRAN programs                             #
#  									    #
# "make" compiles and links the specified main programs and modules,        #
# using the specified libraries (if any), and produces the executables      #
#									    #
# "make clean" removes all files generated by "make"                        #
#                                                                           #
#############################################################################

all: rmxeq  mkxeq
.PHONY: all

# main programs and modules to be compiled 
MAIN= vbf  makeruncard 
TOCP= $(MAIN) *-pl *.sh analyse

MODULES = libstructf libintf  libscale\
	 hplog vegas gridHT CALLING_cpHTO

# additional libraries
LHLIB = LHAPDF
LHLIBPATH = $(shell  /nfs/soft/madgraph/lhapdf/bin/lhapdf-config --prefix)/lib

#LT=ooptools
#LTPATH=/home/fynu/mzaro/LoopTools/lib
#CUBA=/home/chowa/Cuba-1.6

###########################################################

SHELL=/bin/bash

FF = gfortran
CPP = g++
FFLAGS =

FLINKER=$(FF) 

PGMS= $(MAIN) $(MODULES)

-include $(addsuffix .d,$(PGMS))

# rule to compile source programs

$(addsuffix .o,$(PGMS)): %.o: %.f Makefile
	$(FF) $< -c $(FFLAGS)

# rule to link object files
#


$(MAIN): %: %.o $(addsuffix .o,$(MODULES)) Makefile
	$(FLINKER) $< $(addsuffix .o,$(MODULES)) $(FFLAGS)  \
        $(addprefix -L,$(LHLIBPATH)) $(addprefix -l,$(LHLIB)) \
	$(addprefix -L,$(LTPATH)) $(addprefix -l,$(LT)) \
	     -o $@
#	      $(addprefix -L, $(CUBA)) -lcuba \
# produce executables

mkxeq: $(MAIN)

# remove old executables

rmxeq:
	@ -rm -f $(MAIN); \
        echo "delete old executables"		

# clean directory 

clean:
	@ -rm -rf *.o *~ *.log fort* *.mod  $(MAIN)
.PHONY: clean
 
################################################################################
