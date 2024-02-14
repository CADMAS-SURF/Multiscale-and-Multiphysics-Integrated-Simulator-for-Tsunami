MODDIR = ../module 
 
FFLAGS = -c $(FOPT) -I $(MODDIR) 
 
OBJS = cgadmg.o   cgindx1g.o geomtx.o   glbstfg.o  \
       gmtx.o     gmtxhx2.o  gmtxpn2.o  gmtxte1.o  \
       gmtxte2.o  gnpfhx2.o  gnpflw.o   gnpflwd.o  \
       gnpfpn2.o  gnpfte1.o  gnpfte2.o  mergdp.o   \
       mpctrnsg.o npflow.o   npflowd.o  outsoil.o  \
       pcnstr.o   pcnstri.o  spcdrhvg.o vavrg.o    \
       velhx2.o   velpn2.o   velte1.o   velte2.o   \
       vintpn1.o  vintpn2.o  wtsoil.o  
 
all : $(OBJS) 
 
.SUFFIXES: .f .o 
 
.f.o : 
	$(FCOMP) $(FFLAGS) $< 
