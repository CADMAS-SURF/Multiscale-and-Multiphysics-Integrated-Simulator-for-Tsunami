##################
# Make File 
##################

# Target
EXE	= a.gfconv

# Objects comprising the target
OBJS	=gf_a1main.o gf_a2init.o gf_a3grid.o gf_a4tran.o \
	 gf_bci.o    gf_bcd.o    gf_vald.o   gf_vali.o

# Specification of compiler and compile options, etc.
FC		= mpif90
FFLAGS		= -Oss -noscope -nolimit -noparallel

LIBRARY		=
COMPILE.f	= $(FC) $(FFLAGS) -c
LINK.f		= $(FC) $(FFLAGS)

# Default command
.SUFFIXES: .f
.f.o:
	$(COMPILE.f) $<

# Command
$(EXE) : $(OBJS)
	$(LINK.f) -o $@ $(OBJS) $(LIBRARY)
