      SUBROUTINE DEFALT
C======================================================================
C     コモン変数の初期化を行う
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'AREA.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'BOUNDR.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'FILEI.h'
      INCLUDE 'GRID.h'
      INCLUDE 'INITL.h'
      INCLUDE 'MATRIX.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'OBSTI.h'
      INCLUDE 'OBSTR.h'
      INCLUDE 'OUTPUT.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'SEDIMENT.h'
      INCLUDE 'TABLER.h'
      INCLUDE 'TABLEI.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'RGWAVE.h'
      INCLUDE 'TYPHOI.h'
      INCLUDE 'TYPHOR.h'
      INCLUDE 'TURBR.h'
      INCLUDE 'MYCNST.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'OIL.h'
      INCLUDE 'VVMAX.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
      INCLUDE 'AGENT.h'
      INCLUDE 'DESTROY.h'
C
      REAL(8)::PAI,PAI2
      INTEGER::I,I1,J,N
C
C
C ... AREA
      NAREA = 0
      DO 100 J=1,NARASZ
      DO 100 I=1,7
         IAREA(I,J) = 0
  100 CONTINUE
C
C ... BOUNDI
      ISURF(1) = 0
      ISURF(2) = 0
      ISURF(3) = 0
      NINLT    = 0
      NOUTLT   = 0
      NWALL    = 0
      MDWALV   = 0
      MDWALT   = 0
      IDWALT   = 0
      IDWALC   = 0
      NTIDE    = 0
      IPFLG    = 0
      NOVRLP(1:4)=0
      NSOMER(1:4)=0
      ILGLWL   = 0
      ILGLWP   = 0
      NBOT     = 0
      LVPNAB   = 0
      IVPNAB   = 1
      LNTANG   = 0
      LMODDEP  = 1
      DO 200 J=1,NINLSZ
         MINLT(J)  = 0
         MOUTLT(J) = 0
  200 CONTINUE
      DO 210 J=1,NINLSZ
      DO 210 I=1,8
         IINLT(I,J) = 0
  210 CONTINUE
      DO 215 J=1,NOTFSZ
      DO 215 I=1,2
         IOUTLT(I,J) = 0
  215 CONTINUE
      DO 220 J=1,NWLLSZ
         DO 230 I=1,3
            MWALL(I,J) = 0
  230    CONTINUE
         DO 240 I=1,5
            IWALL(I,J) = 0
  240    CONTINUE
  220 CONTINUE
C
C ... BOUNDR
      RSURF(1) = 0.0D0
      RSURF(2) = 0.0D0
      RSURF(3) = 0.0D0
      RDWALT   = 0.0D0
      RDWALC   = 0.0D0
      DO 250 J=1,NINLSZ
      DO 250 I=1,8
         RINLT(I,J) = 0.0D0
  250 CONTINUE
      DO 255 J=1,NOTFSZ
      DO 255 I=1,2
         ROUTLT(I,J) = 0.0D0
  255 CONTINUE
      DO 260 J=1,NWLLSZ
      DO 260 I=1,5
         RWALL(I,J) = 0.0D0
  260 CONTINUE
      PAI  = 3.14159265358979D0
      PAI2 = 3.14159265358979D0*2.0D0
      DO 270 J=1,NOTFSZ
         DO 275 I=1,8
            RTIDE(1,I,J) = 0.0D0
            RTIDE(2,I,J) = -PAI*0.5D0
 275     CONTINUE
         HTIDE(1,J) = 0.0D0
         HTIDE(2,J) = 0.0D0
 270  CONTINUE
C     分潮角速度(S2,M2,K1,O1,N2,K2,P1,Q1:rad.)
      ROMEG(1) = PAI2/(3600.0D0*12.00D0)  ! S2
      ROMEG(2) = PAI2/(3600.0D0*12.42D0)  ! M2
      ROMEG(3) = PAI2/(3600.0D0*23.93D0)  ! K1
      ROMEG(4) = PAI2/(3600.0D0*25.82D0)  ! O1
      ROMEG(5) = PAI2/(3600.0D0*12.66D0)  ! N2
      ROMEG(6) = PAI2/(3600.0D0*11.97D0)  ! K2
      ROMEG(7) = PAI2/(3600.0D0*24.07D0)  ! P1
      ROMEG(8) = PAI2/(3600.0D0*26.87D0)  ! Q1
      HTIDE0 = 0.0D0
      AMP = 0.0D0
      TTT = 0.0D0
      ALL = 0.0D0
      HHH = 0.0D0
      AXX = 0.0D0
      AWIND10 = 1.0D0
      ALBEDO  = 0.07D0
      AKEXT   = 0.64D0
C
C ... DOMAIN
      MX   = 0
      MY   = 0
      MZ   = 0
      MXM  = 0
      MYM  = 0
      MZM  = 0
      MXYZ = 0
      MXY  = 0
      NXYZ = 0
      MLWALL = 0
      MLWALL1= 0
      MLWALP = 0
      MLWALB = 0
      MLWALBX= 0
      MLOFL  = 0
      MLOFLX = 0
C
C ... FILE
      IFLTM = 17
      IFLRI = 11
      IFLST = 12
      IFLSF = 13
      IFINI = 14
      IFLRO = 21
      IFLGR = 22
      IFLHS = 23
      IFLEN = 24
      IFLDB = 29
      IFLBO = 26
      IFLBI = 27
      IFLSB = 25
      IFLDP = 30
      IFLOF = 31
      IFLSD = 32
      IFLZB = 33
      IFLBD = 34
      IFLSB2= 35
      IFLAR = 36
      IFLFW = 37
      IFLABC= 38
      IFLMA = 39
      IFLLP = 40
C
C ... FILEC
      CNUL(1:44)   = '                                            '
      CNUL(45:88)  = '                                            '
      CNUL(89:132) = '                                            '
      CLINE = CNUL
      CFLNM = 'test                                '
C
C ... FILEI
      IFLNM = 8
C
C ... GRID
      DO 300 I=1,NGRDSZ
         XGRID(I) = 0.0D0
         YGRID(I) = 0.0D0
         ZGRID(I) = 0.0D0
  300 CONTINUE
      HLMT = 0.0D0
      XCEN = 0.0D0
      YCEN = 0.0D0
      REGION(1) = 0.0D0
      REGION(2) = 0.0D0
      REGION(3) = 0.0D0
      REGION(4) = 0.0D0
      ICORDTYPE = 1
C
C ... INITLI
      NHINIT = 0
      DO 400 J=1,NINTSZ
      DO 400 I=1,4
         IHINIT(I,J) = 0
  400 CONTINUE
C
C ... INITLR
      UUINIT = 0.0D0
      VVINIT = 0.0D0
      WWINIT = 0.0D0
      AKINIT = 0.0D0
      EPINIT = 0.0D0
      TTINIT = 0.0D0
      CCINIT = 0.0D0
      DO 410 I=1,NINTSZ
         HHINIT(I) = 0.0D0
  410 CONTINUE
C
C ... MATRXI
      ITRMTX = 0
      MAXMTX = 100
      LPRMTX = 0
C
C ... MATRXR
      EPSMTX = 1.0D-10
      EPRMTX = 1.0D-10
      RNRMTX = 0.0D0
C
C ... MODELI
      LSURF = 0
      LTYPH = 0
      LTURB = 0
      LTEMP = 0
      LCONC = 0
      LDENS = 0
      DO 420 N=1,5
        ISW(N) = 0
  420 CONTINUE
      ISW(3) = 1
      NFN     =0
      DO 425 N=1,NFNSIZ
        IFNTBL(1,N) = 0
        IFNTBL(2,N) = 0
        IFNTBL(3,N) = 0
        IFNTBL(4,N) = 0
  425 CONTINUE
      IGM2S = -1
      IGM2B = -1
      ISEAWL = 0
      IHONMA = 0
      IAIDA  = 0
      IBKSTP = 0
      LDISS = 0
      LSEDI = 0
      LBRKW = 0
      LAIR  = 0
      IFALLW= 0
      JFALLW= 0
      LDISP = 0
      LSTOCDS = 0
C
C ... MODELR
      GRAV   = -9.80D0
      GM2S   = -1.0D0
      GM2B   = -1.0D0
      PARAMF = 1.0D0
      PARAMF2= 1.0D0-PARAMF
      PARAMV = 1.0D0
      PARAMV2= 1.0D0-PARAMV
      PARAMK = 1.0D0
      PARAMK2= 1.0D0-PARAMK
      PARAMT = 1.0D0
      PARAMT2= 1.0D0-PARAMT
      PARAMC = 1.0D0
      PARAMC2= 1.0D0-PARAMC
      PRT    = 0.9D0
      SCT    = 0.7D0
      CORI   = 0.0D0
      CSMG   = 0.2D0
      GXB    = 1.0D-4
      GLH    = 1.0D+4
      GZH    = 1.0D-3
      EPSH   = 1.0D-5
      EPST   = 1.0D-2
      RIMP   = 0.5D0
      HAIDA  = 0.1D0
      DKENNEDY= 6.5D0
      DKENN1 = 8.0D0
      DKENN2 = 0.65D0
      DKENN3 = 0.08D0
      BETAIWA= 0.37D0
      SAFEBRKW=0.5D0
      CFALLW0= 1.0D0
      DFALLWN0= 1.0D0
      DFALLWT0= 0.0D0
      DO 430 N=1,NFNSIZ
        FNVAL(N) = 0.0D0
  430 CONTINUE
      DISPBETA=1.D0/15.D0
      DISPLIM =-0.5D0
      DISPBCLIM=-2.0D0
C
C ... TURBR
      AKAR = 0.4D0
      TAA  = 5.5D0
      CMU  = 0.09D0
      SGK  = 1.0D0
      SGE  = 1.3D0
      SGT  = 1.0D0
      TC1  = 1.44D0
      TC2  = 1.92D0
      TC3  = 0.0D0
      TCE  = 0.0D0
      AKMIN = 1.0D-20
      EPMIN = 1.0D-20
      TVSMAX = 1.0D2
      TVSMIN = 1.0D-6
C
C ... MYCNST
      RKAR = 0.4D0
      RA1  = 0.92D0
      RA2  = 0.74D0
      RB1  = 16.6D0
      RB2  = 10.1D0
      RC1  = 0.08D0
      RCC  = 0.01D0
      RE1  = 1.8D0
      RE2  = 1.33D0
      PARAMQ = 1.0D0
      PARAMQ2 = 1.0D0-PARAMQ
      Q2MIN = 1.0D-20
      RLMIN = 1.0D-20
      QLMIN = Q2MIN*RLMIN
      TVSVMX = 1.0D-1
C
C ... TYPHOI
      IHNC1 = 0
      IHNCM = 1
      IHNDS = 0
C ... TYPHOR
      XLN = 135.0D0
      YLT = 0.0D0
      C1  = 0.66D0
      C2  = 0.66D0
C
C ... OBSTI
      LOBST = 0
      NOBSS = 0
      NOBSP = 0
      NPORS = 0
      NFRIC = 0
      NSEA  = 0
      LFOBS = 0
      NFOBS = 0
      LDPRS = 0
      DO 500 J=1,NOBSSZ
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     DO 510 I=1,7
      DO 510 I=1,6
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IOBSS(I,J) = 0
  510 CONTINUE
  500 CONTINUE
      DO 520 J=1,NOBPSZ
      DO 530 I=1,7
         IOBSP(I,J) = 0
  530 CONTINUE
  520 CONTINUE
      DO 540 J=1,NPRSSZ
      DO 545 I=1,6
         IPORS(I,J) = 0
  545 CONTINUE
  540 CONTINUE
      DO 550 J=1,NFRCSZ
      DO 555 I=1,6
         IFRIC(I,J) = 0
  555 CONTINUE
  550 CONTINUE
      DO 560 J=1,NSEASZ
      DO 565 I=1,2
         ISEA(I,J) = 0
  565 CONTINUE
  560 CONTINUE
C
C ... OBSTR
      DO 570 J=1,NPRSSZ
      DO 575 I=1,15
         RPORS(I,J) = 0.0D0
         IF(I.EQ.15) RPORS(I,J) = 1.0D0
         IF(I.LE.7) FPORS(I,J)=0.0D0
  575 CONTINUE
  570 CONTINUE
      DO 580 J=1,NFRCSZ
         RFRIC(J) = 0.0D0
  580 CONTINUE
C
C ... OUTPUC
      DO 600 I=1,30
         CLIST(I) = '        '
         CGRPH(I) = '        '
         CHIST(I) = '        '
  600 CONTINUE
C
C ... OUTPUI
      LREST  = 0
      NREST  = 0
      IREST0 = 0
      LLIST  = 0
      NLIST  = 0
      ILIST0 = 0
      MLIST  = 0
      LISTT  = 0
      NLSECT = 0
      LGRPH  = 0
      NGRPH  = 0
      IGRPH0 = 0
      MGRPH  = 0
      LHIST  = 0
      IHIST0 = 0
      IHIST  = 0
      MHIST  = 0
      NHCELL = 0
      NHCELLSUM = 0
      LENDF  = 0
      NENDF  = 0
      IENDF0 = 0
      IALFAFLOW = 0
      KENSAMODE = 0
      NFRAGL = 0
      DO 610 I=1,NRSTSZ
         IREST(I)    = 0
  610 CONTINUE
      DO 615 I=1,NPNTSZ
         ILSECT(1,I) = 0
         ILSECT(2,I) = 0
         IHCELL(1,I) = 0
         IHCELL(2,I) = 0
         IHCELL(3,I) = 0
         LHCELL(I)   = 0
  615 CONTINUE
      DO 620 I=1,NOUTSZ
         ILIST(I)    = 0
         IGRPH(I) = 0
         IENDF(I) = 0
  620 CONTINUE
C
C ... OUTPUR
      RHIST0 = 0.0D0
      RHIST  = 0.0D0
      DO 630 I=1,NRSTSZ
         RREST(I) = 0.0D0
  630 CONTINUE
      DO 640 I=1,NOUTSZ
         RLIST(I) = 0.0D0
         RGRPH(I) = 0.0D0
         RENDF(I) = 0.0D0
  640 CONTINUE
      RFILE(1) = 0.0D0
      RFILE(2) = 0.0D0
      RFILE(3) = 0.0D0
      ETIME=1.0D30
      DFRAGL(:) = 0.0D0
C
C ... PROPTY
      RHO  = 1.026D3
      RHOA = 1.22D0
      ADRHO= RHOA/RHO
      AMUH = RHO*1.0D2
      AMUV = RHO*1.0D-2
      CP   = 3.898D3            ! 大気圧(17.5°C)
C      COND = 0.596D0            ! 大気圧(20°C)
      ANUH = AMUH/RHO          ! 水平渦動粘性係数
      ANUV = AMUV/RHO          ! 鉛直渦動粘性係数
      ALPH = ANUH/PRT
      ALPV = ANUV/PRT
      CNDH = ALPH*RHO*CP
      CNDV = ALPV*RHO*CP
      DIFH = ANUH/SCT
      DIFV = ANUV/SCT
C
C ... TABLEI
      NTABLE = 0
      DO 700 I=1,NTBLSZ
         ITABLE(I) = 0
  700 CONTINUE
C
C ... TABLER
      DO 710 J=1,NTBLSZ
      DO 710 I=1,NTIMSZ
         TTABLE(I,J) = 0.0D0
         VTABLE(I,J) = 0.0D0
  710 CONTINUE
      DO 720 I=1,NTBLSZ
         TABLE(I) = 0.0D0
  720 CONTINUE
C
C ... TIMEI
      ISTEP  = 0
      MAXSTP = 0
      IDT    = 0
      NITER  = 0
      MXITER = 1
      LSTART = 0
C
C ... TIMER
      TIME   = 0.0D0
      DT     = 0.0D0
      DTOLD  = 0.0D0
      DTV    = 0.0D0
      RSTART = 0.0D0
      REND   = 0.0D0
      DTCNST = 0.0D0
      DTSAFE = 0.4D0
      DTMIN  = 1.0D-20
      DTMAX  = 1.0D0
C
C ... CP_NESTBCR
      TIMVF = 0.0D0
      TIMVB = 0.0D0
      TIMHF = 0.0D0
      TIMHB = 0.0D0
C
C ... CP_NESTBCI
      NXY = 0
      DO 730 N=1,4
      NESNS(N) = 0
      NESML(N) = 0
  730 CONTINUE  
C
C ... OIL
      NP_OIL=-1
      OIL_WIN=0
      ROILF(1)=1.0D10
      ROILF(2)=0.0D0
      ROILF(3)=0.0D0
C
C ... SEDIMENT
      ZLIMSD   = 0.01D0
      SSAND    = 1.65D0
      DSAND    = 2.0D-4
      GVSAND   = 0.4D0
      DIFHSD   = 0.0D0
      DIFVSD   = 0.0D0
      SCTHSD   = 1.0D0
      SCTVSD   = 1.0D0
      AEXSD    = 0.15D0
      CMAXSD   = 1.0D0
      BEDINI   = 100.0D0
      PARAMSD  = 1.0D0
      PARAMSD2 = 1.0D0-PARAMSD
      TSOFFLN  = 0.0D0
      TEOFFLN  = 1.0D9
      DTOFFLN  = 1.0D0
      PHIS     = 0.872664625997164D0    ! 50°相当
      KCMIN    = 0.1D0
      MWEXSD   = 0
      MCONCSD  = 0
      MDIFSD   = 1
      MSETSD   = 0
      MUSTSD   = 0
      MRGHSD   = 0
      MSHLSD   = 0
      IBEDINI  = 0
      IBEDSTR  = 0
      MFDBCKSD = 1
      MOFFLNSD = 0
      MBDSLP   = 0
C
C ... VVMAX
      VVMAX=2.0D1
C
C ... AGENT
      NB_SM = -1
      IMMTYP= 0
      IMAMS =0
      IMAME =-1
      IMAMI =999999
      RMAMS =0.0D0
      RMAME =-1.0D0
      RMAMI =1.D10
      RMAMR =0.0D0
C
C ... AIRI
      MZA    = 0
      MZMA   = 0
      LTURBA = 0
      IBCAIRWES = 0
      IBCAIREAS = 0
      IBCAIRSOU = 0
      IBCAIRNOR = 0
      IBCAIRTOP = -2
C
C ... AIRR
      ZGRIDA(:)=0.0D0
      UBCAIRWES=0.0D0
      UBCAIREAS=0.0D0
      UBCAIRSOU=0.0D0
      UBCAIRNOR=0.0D0
      VBCAIRWES=0.0D0
      VBCAIREAS=0.0D0
      VBCAIRSOU=0.0D0
      VBCAIRNOR=0.0D0
      AKBCAIRWES=1.0D-20
      AKBCAIREAS=1.0D-20
      AKBCAIRSOU=1.0D-20
      AKBCAIRNOR=1.0D-20
      EPBCAIRWES=1.0D-20
      EPBCAIREAS=1.0D-20
      EPBCAIRSOU=1.0D-20
      EPBCAIRNOR=1.0D-20
      PBCAIR   =0.0D0
      UINITAIR =0.0D0
      VINITAIR =0.0D0
      AKINITAIR =1.0D-20
      EPINITAIR =1.0D-20
      RHOAIR   =1.2D0
      AMUAIR   =2.0D-5
      PARAMAIR =1.0D0
      PARAMAIR2=1.0D0-PARAMAIR
      VVMAXAIR =1.0D2
      GZHAIR   =0.1D0
      VELMAXAIR=0.0D0
      TIMEABC1 =0.0D0
      TIMEABC2 =0.0D0
C
C ... DESTROY
      DSTLMT(1)=2.D0
      DSTLMT(2:5)=99.9D0
C
      RETURN
      END
