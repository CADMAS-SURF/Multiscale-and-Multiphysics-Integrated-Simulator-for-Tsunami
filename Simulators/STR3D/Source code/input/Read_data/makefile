MODDIR = ../../module 
 
FFLAGS = -c $(FOPT) -I $(MODDIR) 
 
OBJS = get_comm.o   inqtxt.o     Rd_BCTSET.o  Rd_BSURFS.o  \
       Rd_CONTACT.o Rd_CORD2C.o  Rd_CORD2R.o  Rd_FORCE.o   \
       Rd_FRIC.o    Rd_GRAV.o    Rd_GRID.o    Rd_LOAD.o    \
       Rd_MAT1.o    Rd_MAT9.o    Rd_MATS1.o   Rd_MATT1.o   \
       Rd_MPC.o     Rd_NLPARM.o  Rd_PARAM.o   Rd_PBAR.o    \
       Rd_PBARL.o   Rd_PLOAD.o   Rd_PLOAD1.o  Rd_PLOAD2.o  \
       Rd_PLOAD4.o  Rd_REST.o    Rd_RFORCE.o  Rd_RLOAD1.o  \
       Rd_SHELL.o   Rd_SOLID.o   Rd_SPC.o     Rd_SPC1.o    \
       Rd_SPCADD.o  Rd_SUBCASE.o Rd_TABLEM1.o Rd_TEMP.o    \
       Rd_TEMPD.o   Rd_TEMPP1.o  Rd_TLOAD1.o  Rd_TSTEP.o   \
       Rd_TSTEPNL.o Read_data.o  resfr.o      thru.o      
 
all : $(OBJS) 
 
.SUFFIXES: .f .o 
 
.f.o : 
	$(FCOMP) $(FFLAGS) $< 
