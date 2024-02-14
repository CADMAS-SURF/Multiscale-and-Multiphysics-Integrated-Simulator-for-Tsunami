MODDIR = ../module 
 
FFLAGS = -c $(FOPT) -I $(MODDIR) 
 
OBJS = bctset0.o         contact0.o        cont_init_0.o     ens_geom0.o       \
       femap_out0.o      fmp_bar0.o        fmp_nodal0.o      fmp_ous0.o        \
       fmp_rod0.o        fmp_soil0.o       fmp_sol0.o        fricchk0.o        \
       fric_updt.o       gather_nodal_d.o  gather_nodal_i.o  gather_surface.o  \
       glb_comm.o        islv_updt.o       istchk0.o         mdpress0.o        \
       npflow0.o         outnod0.o         output0.o         outsoil0.o        \
       outsol0.o         rdpress0.o        rd_restart0.o     recv_pres0.o      \
       recv_rst0.o       recv_surf0.o      remesh0.o         resfrc0.o         \
       rmsh_init0.o      scatter_nodal_d.o scatter_nodal_i.o scatter_surface.o \
       send_cont0.o      send_pos0.o       stickchk0.o       wt_restart0.o    
 
all : $(OBJS) 
 
.SUFFIXES: .f .o 
 
.f.o : 
	$(FCOMP) $(FFLAGS) $< 
