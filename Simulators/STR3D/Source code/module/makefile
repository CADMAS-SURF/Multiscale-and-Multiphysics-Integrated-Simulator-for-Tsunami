FFLAGS = -c $(FOPT)

OBJS = input_work.o m_val.o mpc_work.o m_part.o mused.o mavbl.o m_mumps.o mpi_param.o mod_comm.o

all : $(OBJS)

mused.o : mused.f90
	$(FCOMP) $(FFLAGS) mused.f90

mavbl.o : mavbl.f90
	$(FCOMP) $(FFLAGS) mavbl.f90

input_work.mod input_work.o : input_work.f90
	$(FCOMP) $(FFLAGS) $?

m_val.mod m_val.o : m_val.f90
	$(FCOMP) $(FFLAGS) $?

mpc_work.mod mpc_work.o : mpc_work.f90
	$(FCOMP) $(FFLAGS) $?

m_mumps.mod m_mumps.o : m_mumps.f90
	$(FCOMP) $(FFLAGS) $?

mpi_param.mod mpi_param.o : mpi_param.f90
	$(FCOMP) $(FFLAGS) $?

m_part.mod m_part.o : m_part.f90
	$(FCOMP) $(FFLAGS) $?

mod_comm.mod mod_comm.o : mod_comm.f90
	$(FCOMP) $(FFLAGS) $?


