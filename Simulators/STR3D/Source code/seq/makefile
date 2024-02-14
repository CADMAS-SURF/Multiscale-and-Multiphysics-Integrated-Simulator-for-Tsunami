MODDIR = ../module 
 
FFLAGS = -c $(FOPT) -I $(MODDIR) 
 
OBJS = cgadmg_s.o       cgindx2_s.o      clsindex_s.o     geomtx_s.o       \
       glbstfg_s.o      gmtxhx2_s.o      gmtxpn2_s.o      gmtxte1_s.o      \
       gmtxte2_s.o      gnpflw_s.o       mkindex_s.o      nl_static_s.o    \
       nl_transient_s.o npflow_s.o       npforce_s.o      spcdrhvg_s.o    
 
all : $(OBJS) 
 
.SUFFIXES: .f .o 
 
.f.o : 
	$(FCOMP) $(FFLAGS) $< 
