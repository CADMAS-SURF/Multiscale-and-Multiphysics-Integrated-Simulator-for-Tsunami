#
# Makefile
#

LOAD = make_drift_file.out

# compile option
FC   = ifort
COPT = -O3 -fpconstant -xHost -zero
LOPT = $(COPT)
LIBS = 

# Source
SRCS   = make_drift_file.f \
         read_control_file.f read_stoc_file.f read_str_file.f \
         check_end_file.f read_end_file.f generate_debris.f \
         instoc.f inmethd.f indrift.f inboun.f incase.f ingrid.f \
         init_random.f abort.f defalt.f errmsg.f flopen.f get1.f \
         get2.f getc.f getc2.f geti.f getr.f mgetc.f mgeti.f mgetr.f

# Generated
OBJS   = $(SRCS:.f=.o)

#Rule
$(LOAD): $(OBJS)
	$(FC) $(COPT) $(OBJS) -o $@

.SUFFIXES:.f .o

%.o: %.f
	$(FC) $(COPT) -c $<

clean:;	rm -f $(LIBRARY) $(OBJS) $(LOAD) core

###
