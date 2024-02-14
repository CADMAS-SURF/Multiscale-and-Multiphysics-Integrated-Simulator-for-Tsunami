#
# Makefile
#

LOAD = dm.out-opt
INC = -I../Include

#INTEL FORTRAN 11-
FC = mpif90
#Release
COPT = -O3 -fpconstant -xHost -zero
#Debug
#COPT = -O0 -C -fpconstant -zero -fpe0 -traceback -warn all -nogen-interfaces
#Kens
#COPT = -O1 -fpconstant -assume nostd_minus0_rounding

DUMMY = 
LOPT = $(COPT)

SRCS = \
       block.f       collis1.f     collis2.f     collis3.f \
       collision.f   com_stoc0.f   com_stoc1.f   com_stoc2.f \
       com_stoc3.f   com_stoc4.f   drag1.f       drag2.f \
       dragwin.f     drift.f       dshift.f      errmsg.f \
       init_mpienv.f inputc.f      inputd.f      inputf.f \
       inputr.f      interp_h.f    interp_v.f    interp_win.f \
       main.f        mkdlist.f     modvert.f     output.f \
       refer.f       setarea.f     setdt.f       setfence.f \
       setflw.f      setkd.f       setlvl.f      setwin.f \
       sink.f        sink2.f       solver.f      vertex.f \
       vertex2.f     vmass.f       xyside.f      ftimer.f

SRCSM= \
       mod_comm.f90   m_com_stoc.f90 m_drift.f90    m_fence.f90 \
       m_filein.f90   m_fluid.f90    m_geom.f90     m_grid.f90 \
       m_model.f90    m_output.f90   m_time.f90     m_wind.f90 \
       m_subgrid.f90

OBJS = $(SRCS:.f=.o)
OBJSM= $(DUMMY) $(SRCSM:.f90=.o)
MODLS= $(SRCSM:.f90=.mod)

$(LOAD): $(OBJSM) $(OBJS)
	$(FC) -o $@ $(OBJSM) $(OBJS) $(LOPT)

dummy.o: dummy.f90
	$(FC) -c $(INC) $(COPT) $< -o $@

.SUFFIXES:.f90 .o .mod

%.o: %.f
	$(FC) -c $(INC) $(COPT) $< 

%.o: %.f90
	$(FC) -c $(INC) $(COPT) $< 

%.mod: %.f90 %.o
	@:

.f90.mod:
	$(FC) -c $(INC) $(COPT) $< 

clean:;	rm -f $(MODLS) $(OBJSM) $(OBJS) $(LOAD) core


###
block.o: m_grid.mod m_drift.mod m_geom.mod
collis1.o: m_grid.mod m_geom.mod m_drift.mod m_time.mod m_output.mod
collis2.o: m_drift.mod
collis3.o: m_drift.mod m_fence.mod
collision.o: m_time.mod m_grid.mod m_drift.mod m_geom.mod m_output.mod \
	m_fence.mod
com_stoc0.o: mod_comm.mod m_grid.mod m_com_stoc.mod m_output.mod m_filein.mod
com_stoc1.o: m_grid.mod m_fluid.mod m_com_stoc.mod m_output.mod m_filein.mod \
	mod_comm.mod
com_stoc2.o: m_grid.mod m_geom.mod m_filein.mod m_com_stoc.mod m_output.mod \
	mod_comm.mod
com_stoc3.o: m_com_stoc.mod m_output.mod mod_comm.mod
com_stoc4.o: mod_comm.mod
drag1.o: m_time.mod m_grid.mod m_drift.mod m_fluid.mod m_model.mod
drag2.o: m_time.mod m_grid.mod m_drift.mod m_fluid.mod
dragwin.o: m_drift.mod m_wind.mod m_model.mod
drift.o: m_time.mod m_grid.mod m_drift.mod m_fluid.mod m_model.mod \
	m_output.mod m_wind.mod
dshift.o: m_drift.mod
errmsg.o: m_time.mod
init_mpienv.o: m_com_stoc.mod mod_comm.mod
inputc.o: m_time.mod m_grid.mod m_fluid.mod m_model.mod m_output.mod \
	m_filein.mod m_wind.mod m_subgrid.mod
inputd.o: m_grid.mod m_drift.mod m_fluid.mod m_model.mod m_output.mod
inputf.o: m_fence.mod m_output.mod
inputr.o: m_time.mod m_drift.mod m_grid.mod m_filein.mod m_output.mod \
	m_model.mod m_fluid.mod
interp_h.o: m_grid.mod m_fluid.mod m_geom.mod
interp_v.o: m_grid.mod m_fluid.mod
interp_win.o: m_grid.mod m_fluid.mod m_geom.mod
main.o: m_grid.mod m_filein.mod m_fluid.mod m_geom.mod m_time.mod \
	m_com_stoc.mod m_output.mod mod_comm.mod
mkdlist.o: m_grid.mod m_output.mod
modvert.o: m_drift.mod
output.o: m_time.mod m_model.mod m_drift.mod m_fence.mod m_grid.mod \
	m_filein.mod m_output.mod m_fluid.mod
refer.o: m_grid.mod m_geom.mod m_filein.mod m_com_stoc.mod
setarea.o: m_grid.mod m_drift.mod
setdt.o: m_grid.mod m_time.mod m_drift.mod m_output.mod
setfence.o: m_grid.mod m_geom.mod m_fence.mod m_output.mod
setflw.o: m_grid.mod m_time.mod m_filein.mod m_fluid.mod m_output.mod
setkd.o: m_grid.mod
setlvl.o: m_grid.mod m_geom.mod m_drift.mod m_fluid.mod m_time.mod \
	m_model.mod m_output.mod
setwin.o: m_grid.mod m_time.mod m_com_stoc.mod m_filein.mod m_fluid.mod \
	m_output.mod m_wind.mod
sink.o: m_time.mod m_drift.mod m_fluid.mod
sink2.o: m_time.mod m_drift.mod m_fluid.mod
solver.o: m_time.mod m_grid.mod m_model.mod m_drift.mod m_fluid.mod \
	m_output.mod m_filein.mod m_wind.mod
vertex.o: m_drift.mod
vmass.o: m_time.mod m_grid.mod m_drift.mod m_fluid.mod m_model.mod
xyside.o: m_drift.mod
ftimer.o: m_output.mod
m_filein.o: m_com_stoc.mod
m_grid.o: m_com_stoc.mod
m_wind.o: m_com_stoc.mod
