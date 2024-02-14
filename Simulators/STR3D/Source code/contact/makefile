MODDIR = ../module 
 
FFLAGS = -c $(FOPT) -I $(MODDIR) 
 
OBJS = areacd.o      areain.o      bctset.o      circhk.o      \
       cntrfc.o      contact.o     contactp.o    cont_init.o   \
       cont_init_p.o cont_tbl.o    cont_tbl_p.o  corredge.o    \
       corrface1.o   corrface2.o   corrface3.o   crh2edge.o    \
       crh2face.o    crh2set.o     crh2stke.o    crh2stkf.o    \
       crh2vrtx.o    csfhx.o       csfpn.o       csfte.o       \
       edgechk.o     edgedl.o      edgeout.o     edgeslip.o    \
       edgestk.o     edgquad.o     elcquad.o     enpfrcd.o     \
       estfedge.o    estfface.o    facechk.o     faceon.o      \
       faceout.o     facerl.o      facesepa.o    facestk.o     \
       fricchk.o     fricedge.o    fricface.o    fricint.o     \
       fricset.o     fricstf.o     grazeel.o     gsfhx.o       \
       gsfpn.o       gsurf.o       gsurfq.o      gsurfx.o      \
       initidx.o     istchk.o      lencd.o       lhsidx.o      \
       mktehx.o      mktepn.o      mpccorr.o     mpccorr11.o   \
       mpccorr12.o   mpccorr13.o   mpccorr2.o    mpccorr3.o    \
       mpcedge.o     mpcface.o     mpcpreb.o     mpcrank.o     \
       mpcset.o      mpcset2.o     mpcsize.o     mpcslv.o      \
       mpcstke.o     mpcstkf.o     mpcvrtx.o     mstnod.o      \
       npfedge.o     npfface.o     npforced.o    npfric.o      \
       npfricd.o     nrmvec.o      penchk.o      penst.o       \
       resfric.o     rfcedge.o     rfcface.o     rhsidxb.o     \
       rhsidxf.o     rhsidxr.o     rlcorr.o      rmpccal.o     \
       slvnrm.o      spcdedge.o    spcdface1.o   spcdface2.o   \
       spcdface3.o   stickchk.o    tredge.o      trface.o      \
       volcd.o       volin.o       volin3.o      vrtxchk.o     \
       vrtxsepa.o    vrtxslip.o    vrtxstk.o     wrksize.o    
 
all : $(OBJS) 
 
.SUFFIXES: .f .o 
 
.f.o : 
	$(FCOMP) $(FFLAGS) $< 
