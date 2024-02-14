MODDIR = ../../module 
 
FFLAGS = -c $(FOPT) -I $(MODDIR) 
 
OBJS = nm_count.o   Set_table.o  sid_count.o  sid_count2.o \
       St_BCTPARA.o St_BCTSET.o  St_BSURFS.o  St_ELEM.o    \
       St_FORCE.o   St_MATS1.o   St_MATT1.o   St_MPC.o     \
       St_PLOAD.o   St_PLOAD1.o  St_PLOAD4.o  St_SID.o     \
       St_SPC.o     St_SPC1.o    St_TEMP.o    St_TEMPP1.o  \
       tb_count.o  
 
all : $(OBJS) 
 
.SUFFIXES: .f .o 
 
.f.o : 
	$(FCOMP) $(FFLAGS) $< 
