# usage: make target atom=xyz or
#        make target  
#
# version 2 (order of compilation for target formalsol changed)
#
# in case, change default value of ATOM
# you need to have dir inicalc installed in parallel with current directory
# and you need to have the atomic-data file (i.e. A10HHe.dat) in your
# current dir
# possible targets
# install : installs all programs
# all: compiles and links everything, in dependence of your data file
# nlte:      -> pnlte_x.eo
# formalsol: -> pformalsol_x.eo
# totout:    -> ptotout_x.eo
#               where x is the atomic-file name (without dat)
# clean: removes all .o and .mod files, all ~files etc.
# cleanall: removes also .eo files

ifdef atom
ATOM=$(atom)
else
ATOM=A10HHeNCOPSi
#ATOM=A10HHe
#ATOM=A10HHeSi
#ATOM=A10HHeN2345
#ATOM=A10HHeP
endif

ATOMFILE=$(ATOM).dat
INIDIR=../inicalc

.SUFFIXES: .f90

F90 = ifort

#standard
LD = ifort
CFLAGS  = -w -c -O3 -mp -assume byterecl

# for profiling
#LD = ifort -p
#CFLAGS  = -w -c -O3 -mp -assume byterecl -p

#for traceback
#CFLAGS  = -w -c -O3 -mp -assume byterecl -traceback
#CFLAGS  = -w -c -O3 -mp -assume byterecl -traceback -fpe0
#CFLAGS  = -w -c -O3 -mp -assume byterecl -traceback -fpe0 -check bounds

# for debugging
CFLAGS_db  = -w -c -mp -assume byterecl -g

all: nlte_dim.o nlte formalsol totout

install: 
	 cp $(INIDIR)/newprog/*.f90 .
	 
# the object files 
OBJS1  = nlte_type.o ffr.o dimes.o

OBJS2  = nlte_type.o nlte_dim.o princesa.o ffr.o nlte.o nlte_approx.o lambda_xray.o

OBJS3  = nlte_type.o nlte_dim.o princesa.o ffr.o preformal.o formalsol.o formalsol_add.o
OBJS4  = nlte_type.o nlte_dim.o ffr.o totout.o


nlte_dim.o: $(OBJS1)
	 $(LD) $(OBJS1) -o dimes.eo         	 

	 @ echo $(ATOMFILE) > ATOM_FILE 	 
	 @ echo thom_new.dat >> ATOM_FILE 	 
#	 @ echo LINES_N2345.dat >> ATOM_FILE 	 
# for both A10HHe and A10HHeSi
#	 @ echo LINES_Si.dat >> ATOM_FILE 	 
	 @ echo LINES_Si.dat >> ATOM_FILE 	 
	 ./dimes.eo
         
	 $(F90) $(CFLAGS) nlte_dim.f90         	 

nlte: $(OBJS2)
	 $(LD) $(OBJS2) -o pnlte_$(ATOM).eo         	 

formalsol: $(OBJS3)
	 $(LD) $(OBJS3) -o pformalsol_$(ATOM).eo         	 

totout: $(OBJS4)
	 $(LD) $(OBJS4) -o ptotout_$(ATOM).eo         	 


# rule for object files
.f90.o:		
		$(F90) $(CFLAGS) $<

clean:	
		rm -f *.o *~ *.mod \#*

cleanall: 
		rm -f *.eo *.o *~ *.mod \#*
