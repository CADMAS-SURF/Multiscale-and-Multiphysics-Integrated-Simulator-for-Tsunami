MODDIR = ../module 
 
FFLAGS = -c $(FOPT) -I $(MODDIR) 
CFLAGS = -c 
 
OBJS = alloc.o            astea_mechanical.o bar_geom.o         \
       contact_geom.o     dirinit.o          elem_init.o        ens_case.o         \
       ens_case_t.o       ens_geom.o         flnset1.o          getprm.o           \
       main.o             mumps_para.o       ncgset.o           part_renum.o       \
       part_surf.o        rdpress.o          rmsh_init.o        rmsh_initp.o       \
       rod_geom.o         rst_cntl.o         shell_geom.o       solid_geom.o       \
       surf_geom.o        tstepset.o         mk_dir.o          
 
all : $(OBJS) 
 
.SUFFIXES: .f .c .o 
 
.f.o : 
	$(FCOMP) $(FFLAGS) $< 
 
.c.o : 
	$(CCOMP) $(CFLAGS) $< 
