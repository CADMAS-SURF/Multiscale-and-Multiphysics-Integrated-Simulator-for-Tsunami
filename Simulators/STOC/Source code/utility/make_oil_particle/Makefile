#
# Makefile
#

LOAD = make_oil_particle.out

# compile option
FC   = ifort
COPT = -O3 -fpconstant -xHost -zero
#COPT = -O0 -C -warn all -traceback -nogen-interfaces
LOPT = $(COPT)
LIBS = 

# Source
SRCS   = mod_fault.f90 mod_dmfile.f90 mod_oilfile.f90 \
         make_oil_particle.f90 hsort.f90

# Generated
OBJS   = $(SRCS:.f90=.o)
MODS   = $(SRCS:.f90=.mod)

#rule
$(LOAD): $(OBJS)
	$(FC) $(COPT) $(OBJS) -o $@

.SUFFIXES:.f90 .mod .o

%.o: %.f90
	$(FC) $(COPT) -c $<

%.o: %.mod
	:

clean:;	rm -f $(OBJS) $(MODS) $(LOAD) core

###
