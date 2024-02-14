#
# Makefile
#

LOAD = ns.out-opt
INC = -I../Include
VPATH = ../COM:../Include

#INTEL FORTRAN 11-
FC = mpif90
#Release
COPT = -O3 -fpconstant -xHost -zero
#Debug
#COPT = -O0 -C -fpconstant -zero -fpe0 -traceback -warn all -nogen-interfaces
#Kens
#COPT = -O1 -fpconstant -assume nostd_minus0_rounding

DUMMY =
LOPT = $(COPT)

SRCS   = \
         ns_cleeq.f         ns_clkegn.f        ns_clkeps.f        ns_clkeq.f \
         ns_clsge.f         ns_clsgsm.f        ns_clueq.f         ns_clveq.f \
         ns_clweq.f         ns_main.f          ns_mkcoe1.f        ns_mkcoe2.f \
         ns_solver.f        ns_upuvwp.f        ns_walbnd.f        ns_wlcont.f \
         abort.f            abrydon.f          akndsn.f           akndsn2.f \
         archld.f           arcone.f           bccsrf.f           bcinlf.f \
         bcinlv.f           bcsur2.f           bcsurf.f           bctide.f \
         bctsrf.f           bcuuyz.f           bcvvxz.f           bcwwxy.f \
         bound.f            bounds.f           calgxy.f           cellsc.f \
         chkcns.f           chkval.f           cistn.f            clbed.f \
         clbrkw.f           clconc.f           cldens.f \
         cldp.f             clengy.f           clfallw.f \
         clhuvw.f           clhuvwd.f          clhuvwr.f \
         clsedi.f           clshl.f            clsimp.f \
         clsnew.f           clsurf.f           cltmu.f            clwex.f \
         comout.f           ctime.f            db_ini.f           db_trn.f \
         dbwi2d.f           dbwixy.f           dbwr2d.f \
         dbwrxy.f           defalt.f           errms2.f \
         errmsg.f           faulti.f           faultt.f           flnam.f \
         flopen.f           fluxpl.f           fluxsx.f           fluxsy.f \
         fluxsz.f           fluxup.f           fluxux.f           fluxuy.f \
         fluxuz.f           fluxvp.f           fluxvx.f           fluxvy.f \
         fluxvz.f           fluxwp.f           fluxwx.f           fluxwy.f \
         fluxwz.f           ftimer.f           get1.f             get2.f \
         getc.f             getc2.f            getd.f             geti.f \
         getr.f             hsort.f            inair.f            inboun.f \
         incase.f           incnct.f \
         ingrid.f           ininit.f           init_mpienv.f \
         inmodl.f           inmtrx.f           inobst.f           inoutp.f \
         inprop.f           input.f            insedi.f           intime.f \
         intyph.f           kfsurf.f           loglaw.f           mgetc.f \
         mgetd.f            mgeti.f            mgetr.f            mkdprs.f \
         mkgrid.f           mkind1.f           mkind2.f           mkind4.f \
         mkind5.f           mkinisd.f          mkinit.f \
         mkpors.f           modhuv.f           modij.f            outgrp.f \
         outhst.f           outlst.f           outofflnsd.f       output.f \
         outrst.f           prsini.f           qax.f              qbicgs.f \
         qiludc.f           qip.f              qminv.f            rdsub.f \
         runupx.f           runupy.f           seabot.f           setdt.f \
         settbl.f           setzb.f            solver_offlinesd.f srgset.f \
         ubcsr2.f           ubcsrf.f           wincl3.f           xycv.f \
         zdnest.f           zercli.f           zerclr.f \
         cp_aveqbc.f        cp_aveuvw.f        cp_bchhml.f        cp_bchhns.f \
         cp_bcml2ns.f       cp_bcns2ml.f       cp_bcqbci.f        cp_bcqbcj.f \
         cp_bcqbcml.f       cp_bcqbcml2ns.f    cp_bcqbcns2ml.f    cp_bcuvwi.f \
         cp_bcuvwj.f        cp_bcvvml.f        cp_comhhml.f       cp_dsr_dc2.f \
         cp_dsr_fff.f       cp_hhml2ns.f       cp_kfexpnd.f       cp_list_cmmbnd.f \
         cp_list_indcmm.f   cp_mkind3.f        cp_moddep.f        cp_modzbd.f \
         cp_neibcom.f       cp_neibind.f       cp_neibrev.f       cp_rcvdep.f \
         cp_rcvind.f        cp_rcvml2ns.f      cp_rcvns2ml.f      cp_rcvqbcml2ns.f \
         cp_rcvqbcns2ml.f   cp_rcvzbd.f        cp_seaflg.f        cp_snddep.f \
         cp_sndind.f        cp_sndml2ns.f      cp_sndns2ml.f      cp_sndqbcml2ns.f \
         cp_sndqbcns2ml.f   cp_sndzbd.f        cp_uvml2ns.f \
         cadmas_area.f      cadmas_init.f      cadmas_pors.f      cadmas_recv.f \
         cadmas_send.f \
         com_drift1.f       com_drift2.f       com_drift3.f       com_drift4.f \
         com_ma1.f          com_ma2.f \
         oil_comm_grid.f    oil_comm_phys.f    oil_comm_step.f \
         bcsurf_air.f       mkcoe1_air.f       mkcoe2_air.f       mkind_air.f \
         mkinit_air.f       mkpors_air.f       upuvwp_air.f       cltmu_air.f \
         bcinlv_air.f       kfsurf_air.f       clhuvw_air.f \
         clueq_air.f        clveq_air.f        clweq_air.f \
         fluxux_air.f       fluxuy_air.f       fluxuz_air.f \
         fluxvx_air.f       fluxvy_air.f       fluxvz_air.f \
         fluxwx_air.f       fluxwy_air.f       fluxwz_air.f \
         bcuuyz_air.f       bcvvxz_air.f       bcwwxy_air.f \
         outgrp_air.f       clkeps_air.f       clkeq_air.f        cleeq_air.f  \
         clkegn_air.f       walbnd_air.f \
         fluxsx_air.f       fluxsy_air.f       fluxsz_air.f

SRCSM  = cp_module_indcmm.f \
         mod_map.f mod_wind.f mod_list.f mod_gather.f mod_fault.f
SRCS90M= mod_comm.f90

OBJS   = $(SRCS:.f=.o)
OBJSM  = $(DUMMY) $(SRCS90M:.f90=.o) $(SRCSM:.f=.o)
MDLSM  =  $(SRCS90M:.f90=.mod) $(SRCSM:.f=.mod)

$(LOAD): $(OBJSM) $(OBJS)
	$(FC) -o $@ $(OBJSM) $(OBJS) $(LOPT)

.SUFFIXES:.f90

.f.o:
	$(FC) -c $(INC) $(COPT) $< 

.f90.o:
	$(FC) -c $(INC) $(COPT) $< 

.f.mod:
	$(FC) -c $(INC) $(COPT) $< 

mod_comm.o: mod_comm.f90
	$(FC) -c $(INC) $(COPT) $<

mod_comm.mod: mod_comm.f90
	$(FC) -c $(INC) $(COPT) $< 

clean:;	rm -f $(OBJSM) $(OBJS) $(LOAD) $(MDLSM) core

###
ns_cleeq.o: DOMAIN.h MODELI.h MODELR.h PROPTY.h TURBR.h CP_NESTBC.h
ns_clkegn.o: DOMAIN.h BOUNDI.h MODELI.h MODELR.h TURBR.h PROPTY.h
ns_clkeps.o: DOMAIN.h CP_NESTBC.h TIMEI.h
ns_clkeq.o: DOMAIN.h MODELI.h MODELR.h PROPTY.h TURBR.h CP_NESTBC.h
ns_clsge.o: DOMAIN.h CP_NESTBC.h PROPTY.h MODELI.h MODELR.h TURBR.h MYCNST.h
ns_clsgsm.o: DOMAIN.h CP_NESTBC.h TIMEI.h MODELI.h MODELR.h TURBR.h MYCNST.h
ns_clueq.o: AREA.h BOUNDI.h DOMAIN.h PROPTY.h TIMER.h TIMEI.h MODELI.h \
	MODELR.h CP_NESTBC.h MYCNST.h GRID.h VVMAX.h
ns_clveq.o: AREA.h BOUNDI.h DOMAIN.h PROPTY.h TIMER.h TIMEI.h MODELI.h \
	MODELR.h CP_NESTBC.h MYCNST.h GRID.h VVMAX.h
ns_clweq.o: AREA.h BOUNDI.h DOMAIN.h MODELI.h MODELR.h PROPTY.h TIMER.h \
	TIMEI.h CP_NESTBC.h MYCNST.h VVMAX.h
ns_main.o: mod_comm.mod mod_list.mod DOMAIN.h FILE.h FILEC.h FILEI.h CPUCHK.h \
	BOUNDI.h OBSTI.h CP_NESTBC.h CONNEC.h MODELI.h MODELR.h INITL.h \
	GLOBAL.h TIMEI.h SEDIMENT.h CADMAS.h AIRI.h OUTPUT.h
ns_mkcoe1.o: AREA.h BOUNDI.h DOMAIN.h
ns_mkcoe2.o: DOMAIN.h BOUNDI.h MODELI.h MODELR.h PROPTY.h TIMER.h TIMEI.h
ns_solver.o: mod_comm.mod mod_list.mod DOMAIN.h FILE.h FILEC.h MATRIX.h \
	MODELI.h MODELR.h TURBR.h TIMEI.h TIMER.h BOUNDI.h CONNEC.h \
	CP_NESTBC.h OUTPUT.h PROPTY.h MYCNST.h DRIFT.h CADMAS.h SEDIMENT.h \
	AIRI.h AIRR.h AGENT.h
ns_upuvwp.o: DOMAIN.h MODELI.h MODELR.h FILE.h PROPTY.h TIMER.h TIMEI.h
ns_walbnd.o: DOMAIN.h PROPTY.h TURBR.h
ns_wlcont.o: DOMAIN.h
abort.o: mod_comm.mod FILE.h
archld.o: mod_comm.mod CONNEC.h GRID.h DOMAIN.h FILE.h GLOBAL.h
arcone.o: mod_comm.mod CONNEC.h GRID.h DOMAIN.h FILE.h GLOBAL.h
bccsrf.o: DOMAIN.h MODELI.h BOUNDI.h TIMER.h CONSRV.h
bcinlf.o: AREA.h BOUNDI.h BOUNDR.h DOMAIN.h TABLER.h FILE.h TIMER.h \
	CP_NESTBC.h MODELI.h MODELR.h CONNEC.h CADMAS.h RGWAVE.h
bcinlv.o: AREA.h BOUNDI.h BOUNDR.h DOMAIN.h TABLER.h TIMER.h CP_NESTBC.h \
	CONNEC.h CADMAS.h RGWAVE.h
bcsur2.o: DOMAIN.h MODELI.h MODELR.h FILE.h TIMEI.h TIMER.h
bcsurf.o: mod_wind.mod BOUNDI.h BOUNDR.h DOMAIN.h TABLER.h FILE.h TIMER.h \
	PROPTY.h MODELI.h MODELR.h OIL.h
bctide.o: AREA.h BOUNDI.h BOUNDR.h DOMAIN.h MODELI.h MODELR.h TABLER.h \
	TABLEI.h TIMEI.h TIMER.h CP_NESTBC.h
bctsrf.o: BOUNDI.h BOUNDR.h DOMAIN.h MODELI.h PROPTY.h TIMER.h
bcuuyz.o: AREA.h BOUNDI.h BOUNDR.h DOMAIN.h PROPTY.h TABLER.h TIMEI.h TIMER.h \
	CP_NESTBC.h CONNEC.h CADMAS.h
bcvvxz.o: AREA.h BOUNDI.h BOUNDR.h DOMAIN.h PROPTY.h TABLER.h TIMEI.h TIMER.h \
	CP_NESTBC.h CONNEC.h CADMAS.h
bcwwxy.o: AREA.h BOUNDI.h BOUNDR.h DOMAIN.h PROPTY.h TABLER.h TIMEI.h TIMER.h \
	CP_NESTBC.h CONNEC.h CADMAS.h RGWAVE.h
bound.o: BOUNDI.h DOMAIN.h MODELI.h MODELR.h TIMEI.h TIMER.h FILE.h BOUNDR.h \
	TABLER.h
bounds.o: BOUNDI.h DOMAIN.h MODELI.h MODELR.h TIMEI.h TIMER.h FILE.h BOUNDR.h \
	TABLER.h
calgxy.o: OBSTI.h OBSTR.h
cellsc.o: DOMAIN.h
chkcns.o: DOMAIN.h
chkval.o: DOMAIN.h
clbed.o: BOUNDI.h CONNEC.h DOMAIN.h MODELI.h MODELR.h SEDIMENT.h TIMER.h
clbrkw.o: CONNEC.h GLOBAL.h DOMAIN.h MODELI.h MODELR.h TIMER.h
clconc.o: DOMAIN.h MODELI.h MODELR.h PROPTY.h CP_NESTBC.h MYCNST.h CONSRV.h
cldens.o: DOMAIN.h MODELI.h MODELR.h PROPTY.h
cldp.o: DOMAIN.h MODELI.h MODELR.h PROPTY.h
clengy.o: DOMAIN.h BOUNDI.h MODELI.h MODELR.h PROPTY.h TIMER.h TIMEI.h \
	CP_NESTBC.h MYCNST.h CONSRV.h
clfallw.o: DOMAIN.h MODELI.h MODELR.h TIMEI.h FILE.h FILEC.h
clhuvw.o: DOMAIN.h MODELI.h MODELR.h OBSTI.h CADMAS.h VVMAX.h TIMER.h
clhuvwd.o: DOMAIN.h MODELI.h MODELR.h OBSTI.h CADMAS.h VVMAX.h TIMER.h
clhuvwr.o: DOMAIN.h MODELI.h MODELR.h OBSTI.h CADMAS.h VVMAX.h TIMER.h
clsedi.o: FILE.h DOMAIN.h MODELI.h MODELR.h PROPTY.h CP_NESTBC.h MYCNST.h \
	SEDIMENT.h TIMER.h
clshl.o: DOMAIN.h MODELI.h MODELR.h SEDIMENT.h
clsimp.o: DOMAIN.h TIMER.h
clsnew.o: DOMAIN.h MODELI.h TIMER.h
clsurf.o: DOMAIN.h FILE.h MODELI.h MODELR.h TIMEI.h TIMER.h BOUNDI.h AREA.h
cltmu.o: DOMAIN.h BOUNDI.h MODELI.h MODELR.h TURBR.h
clwex.o: DOMAIN.h MODELI.h MODELR.h SEDIMENT.h TIMER.h
comout.o: AREA.h BOUNDI.h BOUNDR.h CONNEC.h GLOBAL.h DOMAIN.h FILE.h FILEC.h \
	FILEI.h GRID.h INITL.h MATRIX.h MODELI.h MODELR.h OBSTI.h OBSTR.h \
	OUTPUT.h PROPTY.h SEDIMENT.h TABLER.h TABLEI.h TIMEI.h TIMER.h \
	RGWAVE.h TYPHOI.h TYPHOR.h TURBR.h MYCNST.h CP_NESTBC.h DRIFT.h \
	VVMAX.h CADMAS.h AIRI.h AIRR.h AGENT.h
db_ini.o: CONNEC.h DOMAIN.h MODELI.h
db_trn.o: DOMAIN.h MODELI.h MODELR.h TIMEI.h TIMER.h
dbwi2d.o: DOMAIN.h FILE.h
dbwixy.o: DOMAIN.h FILE.h
dbwr2d.o: FILE.h
dbwrxy.o: DOMAIN.h FILE.h
defalt.o: AREA.h BOUNDI.h BOUNDR.h DOMAIN.h FILE.h FILEC.h FILEI.h GRID.h \
	INITL.h MATRIX.h MODELI.h MODELR.h OBSTI.h OBSTR.h OUTPUT.h PROPTY.h \
	SEDIMENT.h TABLER.h TABLEI.h TIMEI.h TIMER.h RGWAVE.h TYPHOI.h \
	TYPHOR.h TURBR.h MYCNST.h CP_NESTBC.h OIL.h VVMAX.h AIRI.h AIRR.h \
	AGENT.h
errms2.o: FILE.h
errmsg.o: FILE.h
faulti.o: mod_fault.mod FILE.h
faultt.o: mod_fault.mod FILE.h GRID.h DOMAIN.h CONNEC.h
flnam.o: FILEI.h FILEC.h CONNEC.h GLOBAL.h
flopen.o: CONNEC.h
fluxpl.o: AREA.h DOMAIN.h MODELI.h
fluxsx.o: AREA.h BOUNDI.h BOUNDR.h DOMAIN.h TABLER.h PROPTY.h MODELI.h \
	MODELR.h CP_NESTBC.h CONNEC.h TIMEI.h
fluxsy.o: AREA.h BOUNDI.h BOUNDR.h DOMAIN.h TABLER.h PROPTY.h MODELI.h \
	MODELR.h CP_NESTBC.h CONNEC.h TIMEI.h
fluxsz.o: AREA.h BOUNDI.h BOUNDR.h DOMAIN.h TABLER.h PROPTY.h MODELI.h \
	MODELR.h TIMEI.h
fluxup.o: BOUNDI.h BOUNDR.h DOMAIN.h PROPTY.h TABLER.h
fluxux.o: DOMAIN.h MODELI.h MODELR.h PROPTY.h
fluxuy.o: DOMAIN.h MODELI.h MODELR.h PROPTY.h
fluxuz.o: BOUNDI.h DOMAIN.h MODELI.h MODELR.h PROPTY.h TIMER.h
fluxvp.o: BOUNDI.h BOUNDR.h DOMAIN.h PROPTY.h TABLER.h
fluxvx.o: DOMAIN.h MODELI.h MODELR.h PROPTY.h
fluxvy.o: DOMAIN.h MODELI.h MODELR.h PROPTY.h
fluxvz.o: BOUNDI.h DOMAIN.h MODELI.h MODELR.h PROPTY.h TIMER.h
fluxwp.o: BOUNDI.h BOUNDR.h DOMAIN.h PROPTY.h TABLER.h
fluxwx.o: DOMAIN.h MODELI.h MODELR.h PROPTY.h
fluxwy.o: DOMAIN.h MODELI.h MODELR.h PROPTY.h
fluxwz.o: DOMAIN.h MODELI.h MODELR.h PROPTY.h
ftimer.o: CPUCHK.h mod_comm.mod
get1.o: FILE.h FILEC.h
get2.o: FILE.h FILEC.h
getc2.o: FILE.h FILEC.h
getd.o: FILE.h FILEC.h
inair.o: FILE.h FILEC.h MODELI.h AIRI.h AIRR.h
inboun.o: mod_fault.mod AREA.h BOUNDI.h BOUNDR.h FILE.h FILEI.h FILEC.h \
	TABLER.h TABLEI.h MODELI.h CONNEC.h RGWAVE.h
incase.o: FILE.h FILEC.h FILEI.h
incnct.o: mod_comm.mod CONNEC.h FILE.h GLOBAL.h DRIFT.h CADMAS.h GRID.h \
	AUTODECOMP.h DOMAIN.h
ingrid.o: mod_comm.mod DOMAIN.h FILE.h FILEC.h GRID.h DRIFT.h CONNEC.h \
	GLOBAL.h AUTODECOMP.h
ininit.o: FILE.h FILEC.h INITL.h
init_mpienv.o: mod_comm.mod CONNEC.h DRIFT.h OIL.h
inmodl.o: mod_comm.mod FILE.h FILEC.h MODELI.h MODELR.h TURBR.h MYCNST.h \
	GRID.h VVMAX.h
inmtrx.o: FILE.h FILEC.h MATRIX.h
inobst.o: FILE.h FILEC.h OBSTI.h OBSTR.h
inoutp.o: FILE.h FILEC.h OUTPUT.h OIL.h AGENT.h
inprop.o: FILE.h FILEC.h PROPTY.h MODELI.h MODELR.h
input.o: FILE.h FILEC.h
insedi.o: FILE.h FILEC.h MODELI.h SEDIMENT.h
intime.o: FILE.h FILEC.h TIMEI.h TIMER.h
intyph.o: TYPHOI.h TYPHOR.h FILE.h FILEC.h
kfsurf.o: DOMAIN.h
loglaw.o: FILE.h TURBR.h
mgetc.o: FILE.h FILEC.h
mgetd.o: FILE.h FILEC.h
mgeti.o: FILE.h FILEC.h
mgetr.o: FILE.h FILEC.h
mkdprs.o: DOMAIN.h OBSTI.h OBSTR.h FILE.h FILEC.h FILEI.h CP_NESTBC.h \
	CONNEC.h GLOBAL.h
mkgrid.o: mod_comm.mod GRID.h DOMAIN.h MODELI.h MODELR.h AIRI.h AIRR.h \
	CONNEC.h GLOBAL.h
mkind1.o: AREA.h BOUNDI.h DOMAIN.h FILE.h FILEC.h FILEI.h OBSTI.h CP_NESTBC.h \
	CONNEC.h CADMAS.h GLOBAL.h MODELI.h
mkind2.o: AREA.h BOUNDI.h DOMAIN.h FILE.h
mkind4.o: mod_comm.mod FILE.h FILEI.h FILEC.h DOMAIN.h OBSTI.h MODELI.h \
	CONNEC.h GLOBAL.h
mkind5.o: mod_comm.mod FILE.h FILEC.h FILEI.h DOMAIN.h SEDIMENT.h CONNEC.h \
	GLOBAL.h
mkinisd.o: DOMAIN.h FILE.h FILEI.h FILEC.h PROPTY.h MODELI.h MODELR.h \
	SEDIMENT.h CONNEC.h GLOBAL.h
mkinit.o: OBSTI.h DOMAIN.h FILE.h FILEC.h FILEI.h INITL.h MODELI.h MODELR.h \
	PROPTY.h TIMEI.h TIMER.h BOUNDI.h GRID.h TURBR.h MYCNST.h CONNEC.h \
	CP_NESTBC.h GLOBAL.h DOMAIN.h FILE.h
mkpors.o: DOMAIN.h FILE.h OBSTI.h OBSTR.h MODELI.h MODELR.h BOUNDI.h \
	CP_NESTBC.h CONNEC.h GLOBAL.h
modhuv.o: FILE.h DOMAIN.h MODELI.h MODELR.h OBSTI.h OBSTR.h
modij.o: CONNEC.h GLOBAL.h CONNEC.h GLOBAL.h CONNEC.h GLOBAL.h
outgrp.o: DOMAIN.h FILE.h GRID.h OBSTI.h OUTPUT.h TIMER.h MODELI.h FILE.h
outhst.o: mod_gather.mod DOMAIN.h FILE.h OUTPUT.h TIMEI.h TIMER.h MODELI.h \
	CONNEC.h GLOBAL.h
outlst.o: DOMAIN.h FILE.h OUTPUT.h TIMEI.h TIMER.h MODELI.h
outofflnsd.o: DOMAIN.h FILE.h FILEI.h FILEC.h TIMER.h SEDIMENT.h
output.o: mod_gather.mod DOMAIN.h FILE.h FILEC.h FILEI.h MODELI.h MODELR.h \
	OUTPUT.h TIMEI.h TIMER.h BOUNDI.h CP_NESTBC.h CONNEC.h GLOBAL.h \
	AIRI.h
outrst.o: DOMAIN.h FILE.h FILEC.h FILEI.h MODELI.h TIMEI.h TIMER.h CONNEC.h \
	CP_NESTBC.h
prsini.o: DOMAIN.h TYPHOI.h TYPHOR.h
qax.o: DOMAIN.h
qbicgs.o: DOMAIN.h FILE.h MATRIX.h CONNEC.h GLOBAL.h TIMEI.h
qiludc.o: DOMAIN.h
qip.o: DOMAIN.h
qminv.o: DOMAIN.h
rdsub.o: DOMAIN.h CONNEC.h GLOBAL.h DOMAIN.h CONNEC.h GLOBAL.h DOMAIN.h \
	CONNEC.h GLOBAL.h DOMAIN.h CONNEC.h GLOBAL.h DOMAIN.h CONNEC.h \
	GLOBAL.h DOMAIN.h CONNEC.h GLOBAL.h DOMAIN.h CONNEC.h GLOBAL.h \
	DOMAIN.h CONNEC.h GLOBAL.h DOMAIN.h CONNEC.h GLOBAL.h DOMAIN.h \
	CONNEC.h GLOBAL.h DOMAIN.h CONNEC.h GLOBAL.h DOMAIN.h CONNEC.h \
	GLOBAL.h
runupx.o: DOMAIN.h MODELI.h MODELR.h VVMAX.h
runupy.o: DOMAIN.h MODELI.h MODELR.h VVMAX.h
seabot.o: mod_fault.mod DOMAIN.h FILE.h BOUNDI.h MODELI.h MODELR.h TIMER.h \
	CONNEC.h GLOBAL.h FILEC.h
setdt.o: DOMAIN.h FILE.h MODELI.h MODELR.h PROPTY.h TIMEI.h TIMER.h TURBR.h \
	SEDIMENT.h
settbl.o: TABLER.h TABLEI.h TIMER.h
setzb.o: mod_list.mod DOMAIN.h CONNEC.h FILE.h GLOBAL.h CP_NESTBC.h BOUNDI.h \
	MODELI.h MODELR.h SEDIMENT.h TIMEI.h
solver_offlinesd.o: mod_comm.mod DOMAIN.h FILE.h FILEI.h FILEC.h MATRIX.h \
	MODELI.h MODELR.h TIMEI.h TIMER.h BOUNDI.h CONNEC.h CP_NESTBC.h \
	OUTPUT.h DRIFT.h AIRI.h
srgset.o: DOMAIN.h
ubcsr2.o: mod_comm.mod DOMAIN.h FILE.h FILEC.h FILEI.h TIMER.h BOUNDI.h \
	BOUNDR.h MODELI.h CONSRV.h mod_comm.mod DOMAIN.h CONNEC.h GLOBAL.h
ubcsrf.o: DOMAIN.h FILE.h FILEC.h FILEI.h TIMER.h CONNEC.h GLOBAL.h
wincl3.o: DOMAIN.h TIMEI.h TIMER.h MODELI.h MODELR.h PROPTY.h TYPHOI.h \
	TYPHOR.h
zdnest.o: DOMAIN.h MODELI.h MODELR.h
cp_aveuvw.o: TIMEI.h MODELI.h MODELR.h VVMAX.h
cp_bchhml.o: CP_NESTBC.h MODELI.h MODELR.h CONNEC.h
cp_bchhns.o: CP_NESTBC.h MODELI.h MODELR.h TIMEI.h CONNEC.h
cp_bcml2ns.o: CP_NESTBC.h TIMEI.h FILE.h CONNEC.h
cp_bcns2ml.o: CP_NESTBC.h TIMEI.h CONNEC.h BOUNDI.h
cp_bcqbci.o: FILE.h
cp_bcqbcj.o: FILE.h
cp_bcqbcml.o: TIMEI.h CONNEC.h
cp_bcqbcml2ns.o: CP_NESTBC.h TIMEI.h FILE.h CONNEC.h DOMAIN.h
cp_bcqbcns2ml.o: BOUNDI.h CONNEC.h
cp_bcuvwi.o: FILE.h TIMEI.h TIMER.h MODELI.h MODELR.h VVMAX.h
cp_bcuvwj.o: FILE.h TIMEI.h TIMER.h MODELI.h MODELR.h VVMAX.h
cp_bcvvml.o: TIMEI.h CONNEC.h
cp_comhhml.o: mod_comm.mod CONNEC.h
cp_dsr_dc2.o: cp_module_indcmm.mod CONNEC.h GLOBAL.h
cp_dsr_fff.o: mod_comm.mod DOMAIN.h CONNEC.h
cp_hhml2ns.o: DOMAIN.h CP_NESTBC.h CONNEC.h BOUNDI.h MODELI.h MODELR.h
cp_kfexpnd.o: mod_comm.mod DOMAIN.h CONNEC.h
cp_list_cmmbnd.o: cp_module_indcmm.mod CONNEC.h GLOBAL.h FILE.h
cp_mkind3.o: mod_comm.mod FILE.h BOUNDI.h CP_NESTBC.h CONNEC.h
cp_moddep.o: CONNEC.h CP_NESTBC.h MODELI.h MODELR.h PROPTY.h TIMEI.h GLOBAL.h
cp_modzbd.o: CONNEC.h
cp_neibcom.o: DOMAIN.h MODELI.h
cp_neibind.o: DOMAIN.h
cp_neibrev.o: mod_comm.mod DOMAIN.h CONNEC.h
cp_rcvdep.o: mod_comm.mod
cp_rcvind.o: mod_comm.mod CP_NESTBC.h CONNEC.h FILE.h BOUNDI.h
cp_rcvml2ns.o: mod_comm.mod MODELI.h CP_NESTBC.h CONNEC.h BOUNDI.h FILE.h \
	TIMER.h
cp_rcvns2ml.o: mod_comm.mod MODELI.h CP_NESTBC.h CONNEC.h GLOBAL.h
cp_rcvqbcml2ns.o: mod_comm.mod CONNEC.h
cp_rcvqbcns2ml.o: mod_comm.mod CP_NESTBC.h CONNEC.h GLOBAL.h
cp_rcvzbd.o: mod_comm.mod
cp_seaflg.o: OBSTI.h FILE.h
cp_snddep.o: mod_comm.mod CONNEC.h GLOBAL.h
cp_sndind.o: mod_comm.mod CP_NESTBC.h CONNEC.h FILE.h OUTPUT.h
cp_sndml2ns.o: mod_comm.mod MODELI.h CP_NESTBC.h CONNEC.h FILE.h OUTPUT.h \
	TIMER.h
cp_sndns2ml.o: mod_comm.mod MODELI.h CP_NESTBC.h CONNEC.h
cp_sndqbcml2ns.o: mod_comm.mod CONNEC.h
cp_sndqbcns2ml.o: mod_comm.mod CP_NESTBC.h CONNEC.h
cp_sndzbd.o: mod_comm.mod CONNEC.h GLOBAL.h
cp_uvml2ns.o: DOMAIN.h CONNEC.h CP_NESTBC.h
cadmas_area.o: mod_comm.mod DOMAIN.h GRID.h AREA.h OBSTI.h BOUNDI.h CADMAS.h \
	CONNEC.h GLOBAL.h AUTODECOMP.h
cadmas_init.o: mod_comm.mod DOMAIN.h AREA.h OBSTI.h BOUNDI.h CADMAS.h
cadmas_pors.o: mod_comm.mod DOMAIN.h OBSTI.h CADMAS.h
cadmas_recv.o: mod_comm.mod DOMAIN.h CADMAS.h TIMEI.h
cadmas_send.o: mod_comm.mod DOMAIN.h TIMER.h CADMAS.h
com_drift1.o: mod_comm.mod CONNEC.h DOMAIN.h PROPTY.h DRIFT.h
com_drift2.o: mod_comm.mod CONNEC.h DOMAIN.h DRIFT.h FILE.h TIMER.h MODELI.h
com_drift3.o: mod_comm.mod CONNEC.h DRIFT.h
com_drift4.o: mod_comm.mod
com_ma1.o: mod_comm.mod DOMAIN.h AGENT.h FILE.h FILEC.h FILEI.h
com_ma2.o: mod_comm.mod DOMAIN.h AGENT.h FILE.h TIMEI.h TIMER.h
oil_comm_grid.o: mod_comm.mod CONNEC.h FILEI.h FILEC.h OIL.h
oil_comm_phys.o: mod_comm.mod CONNEC.h FILEI.h FILEC.h TIMER.h OIL.h
oil_comm_step.o: mod_comm.mod CONNEC.h TIMEI.h TIMER.h OIL.h
bcsurf_air.o: DOMAIN.h AIRI.h AIRR.h FILE.h
mkcoe1_air.o: DOMAIN.h AIRI.h FILE.h
mkcoe2_air.o: DOMAIN.h AIRI.h AIRR.h TIMER.h FILE.h TIMEI.h
mkind_air.o: DOMAIN.h AIRI.h AIRR.h FILE.h
mkinit_air.o: DOMAIN.h AIRI.h AIRR.h FILE.h TURBR.h
mkpors_air.o: DOMAIN.h AIRI.h
upuvwp_air.o: DOMAIN.h MODELI.h MODELR.h AIRI.h AIRR.h FILE.h TIMER.h TIMEI.h
cltmu_air.o: DOMAIN.h TURBR.h MODELI.h MODELR.h AIRI.h
bcinlv_air.o: DOMAIN.h CP_NESTBC.h FILE.h FILEC.h FILEI.h TIMER.h AIRI.h \
	AIRR.h
kfsurf_air.o: DOMAIN.h AIRI.h FILE.h
clhuvw_air.o: DOMAIN.h AIRI.h AIRR.h FILE.h
clueq_air.o: DOMAIN.h CP_NESTBC.h TIMER.h GRID.h AIRI.h AIRR.h FILE.h TIMEI.h
clveq_air.o: DOMAIN.h CP_NESTBC.h TIMER.h GRID.h AIRI.h AIRR.h FILE.h
clweq_air.o: DOMAIN.h TIMER.h AIRI.h AIRR.h MODELI.h MODELR.h FILE.h TIMEI.h
fluxux_air.o: DOMAIN.h AIRI.h AIRR.h
fluxuy_air.o: DOMAIN.h AIRI.h AIRR.h
fluxuz_air.o: DOMAIN.h AIRI.h AIRR.h
fluxvx_air.o: DOMAIN.h AIRI.h AIRR.h
fluxvy_air.o: DOMAIN.h AIRI.h AIRR.h
fluxvz_air.o: DOMAIN.h AIRI.h AIRR.h
fluxwx_air.o: DOMAIN.h AIRI.h AIRR.h
fluxwy_air.o: DOMAIN.h AIRI.h AIRR.h
fluxwz_air.o: DOMAIN.h AIRI.h AIRR.h
bcuuyz_air.o: DOMAIN.h CP_NESTBC.h AIRI.h AIRR.h
bcvvxz_air.o: DOMAIN.h CP_NESTBC.h AIRI.h AIRR.h
bcwwxy_air.o: DOMAIN.h AIRI.h AIRR.h
outgrp_air.o: DOMAIN.h FILE.h OUTPUT.h TIMER.h GRID.h AIRI.h AIRR.h
clkeps_air.o: DOMAIN.h CP_NESTBC.h AIRI.h TURBR.h
clkeq_air.o: DOMAIN.h TURBR.h CP_NESTBC.h AIRI.h TIMER.h TIMEI.h FILE.h
cleeq_air.o: DOMAIN.h TURBR.h CP_NESTBC.h AIRI.h TIMER.h TIMEI.h FILE.h
clkegn_air.o: DOMAIN.h TURBR.h AIRI.h AIRR.h TIMEI.h FILE.h
walbnd_air.o: DOMAIN.h AIRI.h AIRR.h TURBR.h FILE.h TIMEI.h
fluxsx_air.o: DOMAIN.h CP_NESTBC.h AIRI.h AIRR.h TIMEI.h
fluxsy_air.o: DOMAIN.h CP_NESTBC.h AIRI.h AIRR.h TIMEI.h
fluxsz_air.o: DOMAIN.h AIRI.h AIRR.h TIMEI.h
cp_module_indcmm.o: FILE.h
mod_wind.o: mod_map.mod
