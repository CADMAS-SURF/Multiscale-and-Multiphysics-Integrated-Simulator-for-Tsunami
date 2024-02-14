MODDIR = ../module 
 
FFLAGS = -c $(FOPT) -I $(MODDIR) 
 
OBJS = blk_e.o      blk_s.o      femap_geom.o femap_out.o  \
       fmp_bar.o    fmp_elem.o   fmp_mat.o    fmp_nodal.o  \
       fmp_node.o   fmp_ous.o    fmp_prop.o   fmp_rod.o    \
       fmp_soil.o   fmp_sol.o    wt_data.o    wt_vec.o     \
       wt_zero_i.o  wt_zero_r.o 
 
all : $(OBJS) 
 
.SUFFIXES: .f .o 
 
.f.o : 
	$(FCOMP) $(FFLAGS) $< 
