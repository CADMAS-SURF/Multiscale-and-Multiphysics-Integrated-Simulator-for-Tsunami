##################
# make file #
##################

# target
EXE	= a.3d-mg10-opt

# Specify compiler and compile options, etc.

FC = mpif90
FFLAGS = -O -fpconstant

LIBRARY = $(FFLAGS)

# Sources comprising the target
SRCS	=db_ini.f db_trn.f \
         vf_a1main.f                                     \
         vf_a2clos.f vf_a2cput.f vf_a2dflt.f vf_a2err.f  \
         vf_bspp.f   vf_bsppfl.f vf_bsss.f   vf_bsuwem.f \
         vf_bsuwn.f  vf_bsuwn3.f vf_bsuwt.f  vf_bsuwt2.f \
         vf_bwff.f   vf_bwffsf.f vf_bwke.f   vf_bwkelg.f \
         vf_bwpp.f   vf_bwss.f   vf_bwuwn.f  vf_bwuwt.f  \
         vf_careap.f vf_cdd00.f  vf_cdiv00.f vf_cdtcal.f \
         vf_cforce.f vf_cggv.f   vf_cggxyz.f vf_cglv.f   \
         vf_cglxyz.f vf_cgrid.f  vf_cindb.f  vf_cindc.f  \
         vf_cindx.f  vf_cinit.f  vf_clm00.f  vf_clogks.f \
         vf_cloglw.f vf_cnfdfl.f vf_cnu00.f  vf_cnut0.f  \
         vf_comak0.f vf_cpara.f  vf_croot2.f vf_croot3.f \
         vf_csetup.f vf_cvort.f  vf_cwlvl.f  vf_cwmak0.f \
         vf_cwmak1.f vf_cwmak2.f vf_cwmsrc.f             \
         vf_cwmtb1.f  vf_cwmtb2.f                        \
         vf_cwmtb12.f vf_cwmtb22.f                       \
         vf_cwzero.f vf_f1cal.f  vf_fbubup.f             \
         vf_fconv.f  vf_fconvs.f vf_fcut01.f vf_fdropf.f \
         vf_feuler.f vf_fgene.f  vf_fmod1.f  vf_fnfini.f \
         vf_fnfprv.f vf_fpvcip.f vf_fpvcpp.f vf_fslp2a.f \
         vf_fslp2f.f vf_fslp3a.f vf_fslp3f.f vf_ii1inp.f \
         vf_iiboun.f vf_iicomp.f vf_iidbg.f  vf_iiequa.f \
         vf_iifile.f vf_iigrid.f vf_iimate.f vf_iimdl.f  \
         vf_iiobst.f vf_iiopt.f  vf_iipara.f vf_iiporo.f \
         vf_iitime.f vf_im1inp.f vf_im2inp.o vf_ip1inp.f vf_ir1inp.f \
         vf_k1cal.f  vf_kgene.f  vf_m1bcgs.f vf_mzax.f   \
         vf_mzbkwd.f vf_mzdcmp.f vf_mzfrwd.f vf_mzip.f   \
         vf_mzminv.f vf_og1ini.f vf_og1trn.f vf_ogbcin.f \
         vf_ogbcnm.f vf_ogbcvl.f vf_ol1ini.f vf_ol1trn.f \
         vf_ol3di.f  vf_ol3dnf.f vf_ol3dr.f  vf_olbc.f   \
         vf_om1ini.f vf_om1trn.f vf_or1ini.f vf_or1trn.f \
         vf_ot1ini.f vf_ot1trn.f vf_p0barr.f vf_p0bcsc.f \
         vf_p0bcsi.f vf_p0end.f  vf_p0enda.f vf_p0init.f \
         vf_p0mind.f vf_p0sumi.f vf_p0time.f vf_p1bcsc.f \
         vf_p1bcsd.f vf_p1bcsi.f vf_p1maxd.f vf_p1mind.f \
         vf_p1mini.f vf_p1sumd.f vf_p1sumi.f vf_p3srd1.f \
         vf_p3srd2.f vf_p3sri1.f vf_p3sri2.f vf_pmgbc0.f \
         vf_pmgc2p.f vf_pmginp.f vf_pmgp2c.f vf_pmgset.f \
         vf_fseabt.f vf_faulti.f vf_faultt.f vf_hsort.f  \
         \
         vf_pmgc2p_cf.f \
         vf_pmgggt.f vf_pmggpt.f \
         vf_pmgp2c_cf.f vf_pmgp2c_pf.f \
         vf_pmgst2.f vf_pmgsts.f \
         vf_pmgflx.f vf_pmgfly.f \
         \
         vf_s1cal.f  vf_sconvd.f vf_sdiff.f  vf_seuler.f \
         vf_t1cal.f  vf_v1cal.f  vf_veuler.f vf_vflxdu.f \
         vf_vflxdv.f vf_vflxdw.f vf_vgene.f  vf_vmodif.f \
         vf_vpcoef.f vf_vpsol.f  vf_wcnd0.f  vf_wsfmb2.f \
         vf_wstk0.f  vf_zgetln.f vf_zgetim.f vf_zseti1.f vf_zseti2.f \
         vf_zseti3.f vf_zsetr1.f vf_zsetr2.f vf_zsetr3.f \
         vf_zstoi.f  vf_zstor.f  vf_zstows.f vf_ztimec.f \
         vf_stoc_1d.f     vf_stoc_area.f   vf_stoc_fconv.f  vf_stoc_init.f   \
         vf_stoc_obst.f   vf_stoc_pors.f   vf_stoc_recv.f   vf_stoc_send.f   \
         vf_zxmg_abort.f  vf_zxmg_allmnd.f vf_zxmg_allsmi.f vf_zxmg_barri.f  \
         vf_zxmg_bcastc.f vf_zxmg_bcasti.f vf_zxmg_crank.f  vf_zxmg_csize.f  \
         vf_zxmg_final.f  vf_zxmg_init.f   vf_zxmg_irecvd.f vf_zxmg_irecvi.f \
         vf_zxmg_isendd.f vf_zxmg_isendi.f vf_zxmg_split.f  vf_zxmg_wait.f   \
         vf_zxmg_wtime.f                                                     \
         vf_zxmp_allmnd.f vf_zxmp_allmni.f vf_zxmp_allmxd.f vf_zxmp_allsmd.f \
         vf_zxmp_allsmi.f vf_zxmp_bcastc.f vf_zxmp_bcastd.f vf_zxmp_bcasti.f \
         vf_zxmp_crank.f  vf_zxmp_csize.f  vf_zxmp_irecvd.f vf_zxmp_irecvi.f \
         vf_zxmp_isendd.f vf_zxmp_isendi.f vf_zxmp_wait.f \
         vf_zxmp_barri.f  vf_zxmp_gatheri.f vf_zxmp_gathervi.f \
         vf_zxmp_gathervd.f vf_zxmp_scattervd.f

OBJS   = $(SRCS:.f=.o) $(DUMMY)

SRCSM = mod_comm.f90 mod_fault.f mod_apara.f
OBJSM  = mod_comm.o mod_fault.o mod_apara.o
MDLSM  = mod_comm.mod mod_fault.mod mod_apara.mod

# command
$(EXE): $(OBJSM) $(OBJS)
	$(FC) -o $@ $(OBJSM) $(OBJS) $(LIBRARY)

.SUFFIXES: .f .o .f90
%.o: %.f
	$(FC) -c $(FFLAGS) $<

%.o: %.f90
	$(FC) -c $(FFLAGS) $<

%.mod: %.f90 %.f %.o
	@:

clean:
	rm -f $(EXE) $(OBJS) $(OBJSM) $(MDLSM) core

###
vf_a1main.o: mod_comm.mod VF_A0PRM.h VF_ACOMPI.h VF_ACOMPR.h VF_ACPUTR.h \
	VF_AFILEI.h VF_AFILER.h VF_ANUMBI.h VF_APARAI.h VF_APHYSI.h \
	VF_APHYSR.h VF_ATIMEI.h VF_ATIMER.h VF_ASTOCI.h VF_ASTOCR.h \
	VF_ASEABT.h
vf_a2clos.o: VF_A0PRM.h VF_AFILEI.h
vf_a2cput.o: VF_A0PRM.h VF_ACPUTR.h VF_APARAI.h
vf_a2dflt.o: VF_A0PRM.h VF_ACOMPI.h VF_ACOMPR.h VF_ADBGI.h VF_ADBGR.h \
	VF_AFILEI.h VF_AFILER.h VF_ANUMBI.h VF_APHYSI.h VF_APHYSR.h \
	VF_ATIMEI.h VF_ATIMER.h VF_ASEABT.h
vf_a2err.o: VF_A0PRM.h VF_AFILEI.h VF_APARAI.h
vf_bspp.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h VF_APHYSR.h
vf_bsppfl.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h
vf_bsss.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h
vf_bsuwem.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h
vf_bsuwn.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h VF_APHYSI.h
vf_bsuwn3.o: VF_A0PRM.h VF_ACOMPI.h VF_ANUMBI.h VF_APARAI.h
vf_bsuwt.o: VF_A0PRM.h VF_ACOMPI.h VF_ANUMBI.h VF_APARAI.h
vf_bsuwt2.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h
vf_bwff.o: VF_A0PRM.h VF_ANUMBI.h VF_ASTOCI.h VF_ASTOCR.h
vf_bwffsf.o: VF_A0PRM.h VF_AFILEI.h VF_ANUMBI.h VF_APARAI.h VF_APHYSI.h \
	VF_APHYSR.h VF_ATIMER.h
vf_bwke.o: VF_A0PRM.h VF_ANUMBI.h
vf_bwkelg.o: VF_A0PRM.h VF_ANUMBI.h VF_APHYSR.h VF_APARAI.h
vf_bwpp.o: VF_A0PRM.h VF_ANUMBI.h
vf_bwss.o: VF_A0PRM.h VF_ANUMBI.h
vf_bwuwn.o: VF_A0PRM.h VF_AFILEI.h VF_ANUMBI.h VF_APARAI.h VF_APHYSI.h \
	VF_APHYSR.h VF_ATIMEI.h VF_ATIMER.h VF_ASTOCI.h VF_ASTOCR.h
vf_bwuwt.o: VF_A0PRM.h VF_AFILEI.h VF_ANUMBI.h VF_APHYSI.h
vf_careap.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h
vf_cdd00.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h VF_APHYSR.h
vf_cdiv00.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h
vf_cdtcal.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h VF_APHYSR.h VF_ATIMEI.h \
	VF_ATIMER.h
vf_cforce.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h
vf_cggv.o: VF_A0PRM.h VF_AFILEI.h VF_AFILER.h VF_ANUMBI.h VF_APARAI.h
vf_cggxyz.o: VF_A0PRM.h VF_AFILEI.h VF_ANUMBI.h VF_APARAI.h
vf_cglv.o: VF_A0PRM.h VF_ANUMBI.h
vf_cglxyz.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h
vf_cgrid.o: VF_A0PRM.h VF_APARAI.h
vf_cindb.o: VF_A0PRM.h VF_ANUMBI.h
vf_cindc.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h
vf_cindx.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h
vf_cinit.o: VF_A0PRM.h VF_ACOMPI.h VF_ACOMPR.h VF_ADBGI.h VF_ADBGR.h \
	VF_AFILEI.h VF_ANUMBI.h VF_APARAI.h VF_APHYSR.h VF_ASEABT.h
vf_clm00.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h VF_APHYSR.h
vf_clogks.o: VF_A0PRM.h VF_APHYSR.h
vf_cloglw.o: VF_A0PRM.h VF_APHYSR.h
vf_cnfdfl.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h
vf_cnu00.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h VF_APHYSR.h
vf_cnut0.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h VF_APHYSR.h
vf_cpara.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h
vf_croot3.o: VF_A0PRM.h
vf_csetup.o: VF_A0PRM.h VF_AFILEI.h VF_AFILER.h VF_ANUMBI.h VF_APARAI.h \
	VF_APHYSI.h VF_APHYSR.h VF_ASTOCI.h VF_ASTOCR.h
vf_cvort.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h
vf_cwlvl.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h VF_APHYSR.h
vf_cwmsrc.o: VF_A0PRM.h VF_AFILEI.h VF_ANUMBI.h VF_APARAI.h VF_APHYSI.h \
	VF_APHYSR.h VF_ATIMER.h
vf_cwmtb1.o: VF_A0PRM.h VF_AFILEI.h
vf_cwmtb2.o: VF_A0PRM.h VF_AFILEI.h
vf_cwmtb12.o: VF_A0PRM.h VF_AFILEI.h
vf_cwmtb22.o: VF_A0PRM.h VF_AFILEI.h
vf_f1cal.o: VF_A0PRM.h VF_ACOMPI.h VF_ACPUTR.h VF_AFILEI.h VF_ANUMBI.h \
	VF_APARAI.h VF_APHYSI.h VF_APHYSR.h VF_ASTOCI.h VF_ASTOCR.h \
	VF_ASEABT.h
vf_fbubup.o: VF_A0PRM.h VF_ACOMPR.h VF_ANUMBI.h VF_APARAI.h VF_APHYSR.h \
	VF_ATIMER.h
vf_fconv.o: VF_A0PRM.h VF_ACOMPR.h VF_ANUMBI.h VF_APARAI.h VF_ATIMER.h
vf_fconvs.o: VF_A0PRM.h VF_ACOMPR.h VF_ANUMBI.h VF_APARAI.h VF_ATIMER.h
vf_fcut01.o: VF_A0PRM.h VF_ACOMPR.h VF_ANUMBI.h VF_APARAI.h
vf_fdropf.o: VF_A0PRM.h VF_ACOMPR.h VF_ACPUTR.h VF_ANUMBI.h VF_APARAI.h \
	VF_APHYSR.h VF_ATIMER.h
vf_feuler.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h VF_ATIMEI.h VF_ATIMER.h
vf_fgene.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h VF_APHYSI.h
vf_fmod1.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h
vf_fnfini.o: VF_A0PRM.h VF_ACOMPR.h VF_ANUMBI.h VF_APARAI.h
vf_fnfprv.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h
vf_fpvcip.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h VF_APHYSI.h
vf_fpvcpp.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h VF_APHYSI.h VF_APHYSR.h \
	VF_ATIMER.h
vf_fslp2a.o: VF_A0PRM.h VF_AFILEI.h
vf_fslp3a.o: VF_A0PRM.h VF_AFILEI.h
vf_ii1inp.o: VF_A0PRM.h VF_AFILEI.h VF_ANUMBI.h VF_APARAI.h
vf_iiboun.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h
vf_iicomp.o: VF_A0PRM.h VF_ACOMPI.h VF_ACOMPR.h VF_ANUMBI.h
vf_iidbg.o: VF_A0PRM.h VF_ADBGI.h VF_ADBGR.h VF_ANUMBI.h VF_APARAI.h
vf_iiequa.o: VF_A0PRM.h VF_ANUMBI.h
vf_iifile.o: VF_A0PRM.h VF_AFILEI.h VF_AFILER.h VF_ANUMBI.h VF_APARAI.h
vf_iigrid.o: VF_A0PRM.h VF_AFILEI.h VF_APARAI.h VF_APARAR.h VF_ASTOCI.h \
	VF_ASTOCR.h
vf_iimate.o: VF_A0PRM.h VF_ANUMBI.h VF_APHYSR.h
vf_iimdl.o: VF_A0PRM.h VF_AFILEI.h VF_ANUMBI.h VF_APARAI.h VF_APHYSI.h \
	VF_APHYSR.h
vf_iiobst.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h
vf_iiopt.o: mod_fault.mod VF_A0PRM.h VF_ACOMPI.h VF_APHYSI.h VF_APHYSR.h \
	VF_ATIMEI.h VF_ASEABT.h
vf_iipara.o: VF_A0PRM.h VF_APARAI.h
vf_iiporo.o: VF_A0PRM.h VF_ACOMPR.h VF_ANUMBI.h VF_APARAI.h
vf_iitime.o: VF_A0PRM.h VF_ATIMEI.h VF_ATIMER.h
vf_im1inp.o: VF_A0PRM.h VF_AFILEI.h VF_AFILER.h VF_APARAI.h
vf_ip1inp.o: VF_A0PRM.h VF_ACOMPR.h VF_AFILEI.h VF_AFILER.h VF_ANUMBI.h \
	VF_APARAI.h
vf_ir1inp.o: VF_A0PRM.h VF_ACOMPI.h VF_ACOMPR.h VF_AFILEI.h VF_ANUMBI.h \
	VF_APARAI.h VF_APHYSI.h VF_APHYSR.h VF_ATIMEI.h VF_ATIMER.h \
	VF_ASTOCI.h VF_ASEABT.h
vf_k1cal.o: VF_A0PRM.h VF_ACOMPI.h VF_AFILEI.h VF_ANUMBI.h VF_APARAI.h \
	VF_APHYSR.h
vf_kgene.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h VF_APHYSR.h
vf_m1bcgs.o: VF_A0PRM.h VF_ACPUTR.h VF_APARAI.h
vf_mzax.o: VF_A0PRM.h VF_APARAI.h
vf_mzbkwd.o: VF_A0PRM.h VF_APARAI.h
vf_mzdcmp.o: VF_A0PRM.h VF_APARAI.h
vf_mzfrwd.o: VF_A0PRM.h VF_APARAI.h
vf_mzip.o: VF_A0PRM.h VF_APARAI.h
vf_og1ini.o: VF_A0PRM.h VF_AFILEI.h VF_ANUMBI.h VF_APARAI.h VF_APARAR.h
vf_og1trn.o: VF_A0PRM.h VF_AFILEI.h VF_AFILER.h VF_ANUMBI.h VF_APARAI.h \
	VF_ATIMEI.h VF_ATIMER.h
vf_ogbcin.o: VF_A0PRM.h VF_AFILEI.h VF_ANUMBI.h VF_APARAI.h
vf_ogbcnm.o: VF_A0PRM.h VF_AFILEI.h VF_ANUMBI.h VF_APARAI.h
vf_ogbcvl.o: VF_A0PRM.h VF_AFILEI.h VF_ANUMBI.h VF_APARAI.h
vf_ol1ini.o: VF_A0PRM.h VF_ACOMPI.h VF_ACOMPR.h VF_AFILEI.h VF_AFILER.h \
	VF_ANUMBI.h VF_APARAI.h VF_APHYSI.h VF_APHYSR.h VF_ATIMEI.h \
	VF_ATIMER.h
vf_ol1trn.o: VF_A0PRM.h VF_AFILEI.h VF_AFILER.h VF_ANUMBI.h VF_APHYSR.h \
	VF_ATIMEI.h VF_ATIMER.h
vf_ol3di.o: VF_A0PRM.h VF_AFILEI.h VF_ANUMBI.h VF_APARAI.h
vf_ol3dnf.o: VF_A0PRM.h VF_AFILEI.h VF_ANUMBI.h VF_APARAI.h
vf_ol3dr.o: VF_A0PRM.h VF_AFILEI.h VF_ANUMBI.h VF_APARAI.h
vf_olbc.o: VF_A0PRM.h VF_AFILEI.h VF_ANUMBI.h VF_APARAI.h
vf_om1ini.o: mod_comm.mod VF_A0PRM.h VF_AFILEI.h VF_ANUMBI.h VF_APARAI.h
vf_om1trn.o: mod_comm.mod VF_A0PRM.h VF_ACOMPI.h VF_ACOMPR.h VF_AFILEI.h \
	VF_AFILER.h VF_ANUMBI.h VF_APHYSI.h VF_ATIMEI.h VF_ATIMER.h \
	VF_APARAI.h
vf_or1ini.o: VF_A0PRM.h VF_AFILEI.h VF_ANUMBI.h VF_APARAI.h VF_APHYSI.h
vf_or1trn.o: VF_A0PRM.h VF_ACOMPI.h VF_ACOMPR.h VF_AFILEI.h VF_AFILER.h \
	VF_ANUMBI.h VF_APHYSI.h VF_ATIMEI.h VF_ATIMER.h VF_APARAI.h \
	VF_ASTOCI.h VF_ASEABT.h
vf_ot1ini.o: VF_A0PRM.h VF_AFILEI.h VF_ANUMBI.h VF_APARAI.h
vf_ot1trn.o: VF_A0PRM.h VF_AFILEI.h VF_AFILER.h VF_ANUMBI.h VF_APARAI.h \
	VF_APHYSR.h VF_ATIMEI.h VF_ATIMER.h
vf_p0barr.o: VF_A0PRM.h VF_ACPUTR.h VF_APARAI.h
vf_p0bcsc.o: VF_A0PRM.h VF_ACPUTR.h VF_APARAI.h
vf_p0bcsi.o: VF_A0PRM.h VF_ACPUTR.h VF_APARAI.h
vf_p0init.o: VF_A0PRM.h VF_APARAI.h VF_APARAR.h
vf_p0mind.o: VF_A0PRM.h VF_ACPUTR.h VF_APARAI.h
vf_p0sumi.o: VF_A0PRM.h VF_ACPUTR.h VF_APARAI.h
vf_p1bcsc.o: VF_A0PRM.h VF_ACPUTR.h VF_APARAI.h
vf_p1bcsd.o: VF_A0PRM.h VF_ACPUTR.h VF_APARAI.h
vf_p1bcsi.o: VF_A0PRM.h VF_ACPUTR.h VF_APARAI.h
vf_p1maxd.o: VF_A0PRM.h VF_ACPUTR.h VF_APARAI.h
vf_p1mind.o: VF_A0PRM.h VF_ACPUTR.h VF_APARAI.h
vf_p1mini.o: VF_A0PRM.h VF_ACPUTR.h VF_APARAI.h
vf_p1sumd.o: VF_A0PRM.h VF_ACPUTR.h VF_APARAI.h
vf_p1sumi.o: VF_A0PRM.h VF_ACPUTR.h VF_APARAI.h
vf_p3srd1.o: VF_A0PRM.h VF_ACPUTR.h VF_ANUMBI.h VF_APARAI.h
vf_p3srd2.o: VF_A0PRM.h VF_ACPUTR.h VF_ANUMBI.h VF_APARAI.h
vf_p3sri1.o: VF_A0PRM.h VF_ACPUTR.h VF_ANUMBI.h VF_APARAI.h
vf_p3sri2.o: VF_A0PRM.h VF_ACPUTR.h VF_ANUMBI.h VF_APARAI.h
vf_pmgbc0.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h
vf_pmgc2p.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h
vf_pmginp.o: VF_A0PRM.h VF_AFILEI.h VF_APARAI.h VF_ASTOCI.h
vf_pmgp2c.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h
vf_pmgset.o: mod_apara.mod VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h
vf_fseabt.o: mod_fault.mod VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h VF_APHYSI.h \
	VF_ATIMER.h VF_AFILEI.h VF_ASEABT.h
vf_faulti.o: mod_fault.mod VF_A0PRM.h VF_AFILEI.h
vf_faultt.o: mod_fault.mod VF_A0PRM.h VF_AFILEI.h VF_ANUMBI.h VF_APARAI.h
vf_pmgc2p_cf.o: VF_A0PRM.h VF_ACOMPR.h VF_ANUMBI.h VF_APARAI.h VF_ATIMER.h
vf_pmgggt.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h
vf_pmggpt.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h
vf_pmgp2c_cf.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h
vf_pmgp2c_pf.o: VF_A0PRM.h VF_ACOMPR.h VF_ANUMBI.h VF_APARAI.h VF_ATIMER.h
vf_pmgst2.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h
vf_pmgsts.o: VF_A0PRM.h
vf_s1cal.o: VF_A0PRM.h VF_ACOMPI.h VF_AFILEI.h VF_APARAI.h VF_ANUMBI.h
vf_sconvd.o: VF_A0PRM.h VF_ACOMPR.h VF_ANUMBI.h VF_APARAI.h
vf_sdiff.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h VF_APHYSR.h
vf_seuler.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h VF_ATIMEI.h VF_ATIMER.h
vf_t1cal.o: VF_A0PRM.h VF_ACOMPI.h VF_AFILEI.h VF_APARAI.h VF_ANUMBI.h
vf_v1cal.o: VF_A0PRM.h VF_ACOMPI.h VF_ACPUTR.h VF_AFILEI.h VF_ANUMBI.h \
	VF_APARAI.h
vf_veuler.o: VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h VF_APARAR.h VF_APHYSI.h \
	VF_APHYSR.h VF_ATIMER.h
vf_vflxdu.o: VF_A0PRM.h VF_ACOMPR.h VF_ANUMBI.h VF_APARAI.h
vf_vflxdv.o: VF_A0PRM.h VF_ACOMPR.h VF_ANUMBI.h VF_APARAI.h
vf_vflxdw.o: VF_A0PRM.h VF_ACOMPR.h VF_ANUMBI.h VF_APARAI.h
vf_vgene.o: VF_A0PRM.h VF_ACOMPR.h VF_ANUMBI.h VF_APARAI.h VF_APHYSI.h \
	VF_APHYSR.h
vf_vmodif.o: VF_A0PRM.h VF_AFILEI.h VF_ANUMBI.h VF_APARAI.h VF_APHYSI.h \
	VF_APHYSR.h VF_ATIMER.h
vf_vpcoef.o: VF_A0PRM.h VF_AFILEI.h VF_ANUMBI.h VF_APARAI.h VF_APHYSI.h \
	VF_APHYSR.h VF_ATIMER.h
vf_vpsol.o: VF_A0PRM.h VF_ACOMPI.h VF_ACOMPR.h VF_ANUMBI.h VF_APARAI.h
vf_zgetln.o: VF_A0PRM.h VF_APARAI.h
vf_zgetim.o: VF_A0PRM.h VF_APARAI.h
vf_stoc_1d.o: VF_A0PRM.h VF_ACPUTR.h VF_ANUMBI.h VF_APARAI.h
vf_stoc_area.o: mod_comm.mod VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h VF_ASTOCI.h \
	VF_AFILEI.h
vf_stoc_fconv.o: VF_A0PRM.h VF_ACOMPR.h VF_ANUMBI.h VF_APARAI.h VF_ATIMER.h \
	VF_ASTOCI.h VF_ASTOCR.h
vf_stoc_init.o: mod_comm.mod VF_ASTOCI.h VF_ASTOCR.h
vf_stoc_obst.o: mod_comm.mod VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h VF_ASTOCI.h \
	VF_ASTOCR.h
vf_stoc_pors.o: mod_comm.mod VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h VF_ASTOCI.h \
	VF_ASTOCR.h
vf_stoc_recv.o: mod_comm.mod VF_ASTOCI.h VF_ASTOCR.h
vf_stoc_send.o: mod_comm.mod VF_A0PRM.h VF_ANUMBI.h VF_APARAI.h VF_ATIMEI.h \
	VF_ATIMER.h VF_ASTOCI.h VF_ASTOCR.h VF_AFILEI.h
vf_zxmg_allmnd.o: mod_comm.mod
vf_zxmg_allsmi.o: mod_comm.mod
vf_zxmg_barri.o: mod_comm.mod
vf_zxmg_bcastc.o: mod_comm.mod
vf_zxmg_bcasti.o: mod_comm.mod
vf_zxmg_crank.o: mod_comm.mod
vf_zxmg_csize.o: mod_comm.mod
vf_zxmg_irecvd.o: mod_comm.mod
vf_zxmg_irecvi.o: mod_comm.mod
vf_zxmg_isendd.o: mod_comm.mod
vf_zxmg_isendi.o: mod_comm.mod
vf_zxmg_split.o: mod_comm.mod
vf_zxmp_allmnd.o: VF_A0PRM.h VF_APARAI.h
vf_zxmp_allmni.o: VF_A0PRM.h VF_APARAI.h
vf_zxmp_allmxd.o: VF_A0PRM.h VF_APARAI.h
vf_zxmp_allsmd.o: VF_A0PRM.h VF_APARAI.h
vf_zxmp_allsmi.o: VF_A0PRM.h VF_APARAI.h
vf_zxmp_bcastc.o: VF_A0PRM.h VF_APARAI.h
vf_zxmp_bcastd.o: VF_A0PRM.h VF_APARAI.h
vf_zxmp_bcasti.o: VF_A0PRM.h VF_APARAI.h
vf_zxmp_crank.o: VF_A0PRM.h VF_APARAI.h
vf_zxmp_csize.o: VF_A0PRM.h VF_APARAI.h
vf_zxmp_irecvd.o: VF_A0PRM.h VF_APARAI.h
vf_zxmp_irecvi.o: VF_A0PRM.h VF_APARAI.h
vf_zxmp_isendd.o: VF_A0PRM.h VF_APARAI.h
vf_zxmp_isendi.o: VF_A0PRM.h VF_APARAI.h
vf_zxmp_gatheri.o: VF_A0PRM.h VF_APARAI.h
vf_zxmp_gathervi.o: VF_A0PRM.h VF_APARAI.h
vf_zxmp_gathervd.o: VF_A0PRM.h VF_APARAI.h
