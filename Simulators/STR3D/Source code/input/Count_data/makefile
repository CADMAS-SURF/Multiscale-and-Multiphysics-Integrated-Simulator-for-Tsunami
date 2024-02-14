MODDIR = ../../module 
 
FFLAGS = -c $(FOPT) -I $(MODDIR) 
 
OBJS = Count_data.o Ct_BCTSET.o  Ct_BSURFS.o  Ct_LOAD.o    \
       Ct_MPC.o     Ct_PLOAD2.o  Ct_SPC.o     Ct_SPC1.o    \
       Ct_SPCADD.o  Ct_TABLEM1.o Ct_TEMP.o    Ct_TEMPD.o   \
       Ct_TEMPP1.o  Ct_TSTEP.o   endt.o       tb_end.o    
 
all : $(OBJS) 
 
.SUFFIXES: .f .o 
 
.f.o : 
	$(FCOMP) $(FFLAGS) $< 
