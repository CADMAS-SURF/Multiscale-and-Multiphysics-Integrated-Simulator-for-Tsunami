MODDIR = ../module 
 
FFLAGS = -c $(FOPT) -I $(MODDIR) 
 
OBJS = addset.o  addset2.o addset3.o addset4.o \
       addvec.o  atxb.o    axb.o     chtnsr.o  \
       clear0.o  clear1.o  cross.o   cross1.o  \
       cross2.o  dircos.o  errstp.o  harfab.o  \
       harfb.o   harfc.o   harfta.o  harfta2.o \
       intpl.o   length.o  matml.o   mean2.o   \
       mean3.o   mean4.o   order.o   order2.o  \
       psfwt.o   rmult1.o  rmult2.o  rmult3.o  \
       rmult4.o  rmult5.o  shift0.o  shift1.o  \
       shiftb0.o subvec.o  sumvec.o  vecml1.o  \
       vecml2.o  vecml3.o  areacd2.o
 
all : $(OBJS) 
 
.SUFFIXES: .f .o 
 
.f.o : 
	$(FCOMP) $(FFLAGS) $< 
