MODDIR = ../module 
 
FFLAGS = -c $(FOPT) -I $(MODDIR) 
CFLAGS = -c 
 
OBJS = bcast_tbl.o       cont_tbl_g.o      dealloc.o         ens_geom_p.o      \
       ex_add.o          ex_pload4.o       ex_spc.o          ex_spc1.o         \
       part.o            partition.o       part_c.o          part_geom.o       \
       part_send_tblx.o  part_send_tblx1.o part_send_tblx2.o recv_tbl.o        \
       recv_tblx.o       send_tbl.o        metis_partition.o
 
all : $(OBJS) 
 
.SUFFIXES: .f .c .o 
 
.f.o : 
	$(FCOMP) $(FFLAGS) $< 
 
.c.o : 
	$(CCOMP) $(CFLAGS) $< 
