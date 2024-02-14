MODDIR = ../module 
 
FFLAGS = -c $(FOPT) -I $(MODDIR) 
 
OBJS = bcgstb.o       bdfbm.o        bdfhx2.o       bdfpn1.o       \
       bdfpn2.o       bdfte1.o       bdfte2.o       bdftrs.o       \
       bdyfc.o        beam0.o        beam2.o        blmbm.o        \
       blmhx1.o       blmpn1.o       blmpn1s.o      blmtx.o        \
       cg3fwb.o       cg3fwb2.o      cg3ilu.o       cg3ilu2.o      \
       cg3ml2.o       cg3ml3.o       cgadm.o        cgindx1.o      \
       cgindx2.o      cgindx3.o      clsindex.o     dbeam0.o       \
       dbeam2.o       derhx2.o       derpn2.o       derqu2.o       \
       derte2.o       dertr2.o       derxbm.o       derxbm0.o      \
       derxhx1.o      derxhx2.o      derxpn2.o      derxte1.o      \
       derxte2.o      det3.o         dfdsg.o        dflt_damp.o    \
       dirupdt.o      dsol0.o        dsol1.o        dtprdct.o      \
       efphx2.o       efppn2.o       efpte1.o       efpte2.o       \
       efrcp.o        emassbm.o      emasshx2.o     emassmtx.o     \
       emasspn1.o     emasspn2.o     emasste1.o     emasste2.o     \
       emasstrs.o     enfdisp.o      enpfadd.o      enpfaddg.o     \
       enpfaddm.o     enpfbm.o       enpfhx1.o      enpfhx2.o      \
       enpfpn1.o      enpfpn2.o      enpfrc.o       enpfte1.o      \
       enpfte2.o      enpftrs.o      estfbm.o       estfhx1.o      \
       estfhx2.o      estfmtx.o      estfpn1.o      estfpn2.o      \
       estfte1.o      estfte2.o      estftrs.o      etabm.o        \
       etahx1.o       etamtx.o       etapn1.o       etapn1s.o      \
       evolhx2.o      evolpn2.o      evolte2.o      findint.o      \
       fnorm.o        force.o        frcadd.o       glbstf.o       \
       green.o        green_bm.o     green_hx1.o    green_pn1.o    \
       green_pn1s.o   iccg.o         ichsky.o       idcrset.o      \
       intplpn1.o     intplpn2.o     inv3.o         ist_beam.o     \
       ist_sol.o      ist_trs.o      ist_updt.o     jacob3.o       \
       jacobi.o       koji3d.o       koji3e.o       lccbm.o        \
       lcchx1.o       lccpn1.o       loadqu2.o      loadtr1.o      \
       loadtr2.o      mdpress.o      mdpressp.o     mergdu.o       \
       mergs.o        mkindex.o      mpctrns.o      mumps.o        \
       ncrset.o       ncrset2.o      nl_static.o    nl_transient.o \
       nodset1.o      nodset2.o      nodset3.o      nodset5.o      \
       npforce.o      ordrnf.o       outnod.o       outnodw.o      \
       output.o       outsol.o       parslv.o       pld_cadmas.o   \
       pload4.o       prnase.o       prncg.o        prnmumps.o     \
       prnpar.o       prnstr.o       rd_restart.o   rd_restartp.o  \
       rearrange.o    resfrc.o       scalsy.o       \
       scalus.o       sfnhx2.o       sfnpn2.o       sfnqu2.o       \
       sfnte2.o       sfntr2.o       sol0.o         sol2.o         \
       spcdrhv.o      spcdsetdu.o    spcdseti.o     spcset.o       \
       stfgl.o        stldset.o      stpress.o      strhx1.o       \
       strhx2.o       strpn1.o       strpn2.o       strte1.o       \
       strte2.o       tetvol.o       tloadset.o     trnsemass.o    \
       trnsmtx3.o     trnsmtx6.o     trnsmtx7.o     trpn1s.o       \
       tspcdseti.o    tspcdsetu.o    vecmid.o       vecmid2.o      \
       vecmid3.o      wtsol.o        wtsurf.o       wt_restart.o   \
       wt_restartp.o  yfunc.o       
 
all : $(OBJS) 
 
.SUFFIXES: .f .o 
 
.f.o : 
	$(FCOMP) $(FFLAGS) $< 
