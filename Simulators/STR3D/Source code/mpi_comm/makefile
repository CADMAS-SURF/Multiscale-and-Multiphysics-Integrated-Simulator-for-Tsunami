MODDIR = ../module 
 
FFLAGS = -c $(FOPT) -I $(MODDIR) 
 
OBJS = cg_mpi_allreduce_d.o cg_mpi_allreduce_i.o cg_mpi_irecv_d.o     cg_mpi_irecv_i.o     \
       cg_mpi_isend_d.o     cg_mpi_isend_i.o     comm_cg.o            comm_cg2.o           \
       comm_cg2x.o          comm_cgx.o           comm_dbl.o           comm_dblx.o          \
       comm_intx.o          comm_tblx.o          c_mpi_allreduce_i.o  c_mpi_barrier.o      \
       c_mpi_init.o         c_mpi_recv_d.o       c_mpi_recv_i.o       c_mpi_send_d.o       \
       c_mpi_send_i.o       c_mpi_waitall.o      m_mpi_bcast_d.o      m_mpi_bcast_i.o      \
       m_mpi_recv_d.o       m_mpi_recv_i.o       m_mpi_reduce_d.o     m_mpi_send_d.o       \
       m_mpi_send_i.o       recv_kk.o            recv_pres.o          recv_rr.o            \
       recv_rst.o           recv_surf.o          send_cont.o          send_kk.o            \
       send_pos.o           send_rr.o            remesh.o
 
all : $(OBJS) 
 
.SUFFIXES: .f .o 
 
.f.o : 
	$(FCOMP) $(FFLAGS) $< 
