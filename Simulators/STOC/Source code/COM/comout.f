      SUBROUTINE COMOUT
C======================================================================
C     コモン変数をデバッグ出力する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'AREA.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'BOUNDR.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
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
      INCLUDE 'DRIFT.h'
      INCLUDE 'VVMAX.h'
      INCLUDE 'CADMAS.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
      INCLUDE 'AGENT.h'
      INCLUDE 'DESTROY.h'
C
      INTEGER::M,N
      REAL*8 ::A2RAD=3.14159265358979D0/180.0D0
C
      WRITE(IFLDB,*) 'COMMON BLOCK: AREA'
      WRITE(IFLDB,*) '   NAREA      =',NAREA
      DO 100 N=1,NAREA
      WRITE(IFLDB,*) '   IAREA(1,',N,') =',IAREA(1,N)
      WRITE(IFLDB,*) '   IAREA(2,',N,') =',IAREA(2,N)
      WRITE(IFLDB,*) '   IAREA(3,',N,') =',IAREA(3,N)
      WRITE(IFLDB,*) '   IAREA(4,',N,') =',IAREA(4,N)
      WRITE(IFLDB,*) '   IAREA(5,',N,') =',IAREA(5,N)
      WRITE(IFLDB,*) '   IAREA(6,',N,') =',IAREA(6,N)
      WRITE(IFLDB,*) '   IAREA(7,',N,') =',IAREA(7,N)
  100 CONTINUE
C
      WRITE(IFLDB,*) 'COMMON BLOCK: BOUNDI'
      WRITE(IFLDB,*) '   ISURF(1)   = ',ISURF(1)
      WRITE(IFLDB,*) '   ISURF(2)   = ',ISURF(2)
      WRITE(IFLDB,*) '   ISURF(3)   = ',ISURF(3)
      WRITE(IFLDB,*) '   NINLT      = ',NINLT
      DO 200 N=1,NINLT
      WRITE(IFLDB,*) '   MINLT(',N,')   =',MINLT(N)
      WRITE(IFLDB,*) '   IINLT(1,',N,') =',IINLT(1,N)
      WRITE(IFLDB,*) '   IINLT(2,',N,') =',IINLT(2,N)
      WRITE(IFLDB,*) '   IINLT(3,',N,') =',IINLT(3,N)
      WRITE(IFLDB,*) '   IINLT(4,',N,') =',IINLT(4,N)
      WRITE(IFLDB,*) '   IINLT(5,',N,') =',IINLT(5,N)
      WRITE(IFLDB,*) '   IINLT(6,',N,') =',IINLT(6,N)
      WRITE(IFLDB,*) '   IINLT(7,',N,') =',IINLT(7,N)
      WRITE(IFLDB,*) '   IINLT(8,',N,') =',IINLT(8,N)
  200 CONTINUE
      WRITE(IFLDB,*) '   NOUTLT     = ',NOUTLT
      DO 210 N=1,NOUTLT
      WRITE(IFLDB,*) '   MOUTLT (',N,')  =',MOUTLT(N)
      WRITE(IFLDB,*) '   IOUTLT(1,',N,') =',IOUTLT(1,N)
      WRITE(IFLDB,*) '   IOUTLT(2,',N,') =',IOUTLT(2,N)
      WRITE(IFLDB,*) '   IOUTLT(3,',N,') =',IOUTLT(3,N)
  210 CONTINUE
      WRITE(IFLDB,*) '   NWALL      = ',NWALL
      DO 220 N=1,NWALL
      WRITE(IFLDB,*) '   MWALL(1,',N,') =',MWALL(1,N)
      WRITE(IFLDB,*) '   MWALL(2,',N,') =',MWALL(2,N)
      WRITE(IFLDB,*) '   MWALL(3,',N,') =',MWALL(3,N)
      WRITE(IFLDB,*) '   IWALL(1,',N,') =',IWALL(1,N)
      WRITE(IFLDB,*) '   IWALL(2,',N,') =',IWALL(2,N)
      WRITE(IFLDB,*) '   IWALL(3,',N,') =',IWALL(3,N)
      WRITE(IFLDB,*) '   IWALL(4,',N,') =',IWALL(4,N)
      WRITE(IFLDB,*) '   IWALL(5,',N,') =',IWALL(5,N)
  220 CONTINUE
      WRITE(IFLDB,*) '   MDWALV     = ',MDWALV
      WRITE(IFLDB,*) '   MDWALT     = ',MDWALT
      WRITE(IFLDB,*) '   IDWALT     = ',IDWALT
      WRITE(IFLDB,*) '   IDWALC     = ',IDWALC
      WRITE(IFLDB,*) '   NTIDE      = ',NTIDE
      WRITE(IFLDB,*) '   IPFLG      = ',IPFLG
      WRITE(IFLDB,*) '   ILGLWL     = ',ILGLWL
      WRITE(IFLDB,*) '   ILGLWP     = ',ILGLWP
      WRITE(IFLDB,*) '   NBOT       = ',NBOT
      WRITE(IFLDB,*) '   LVPNAB     = ',LVPNAB
      WRITE(IFLDB,*) '   IVPNAB     = ',IVPNAB
      WRITE(IFLDB,*) '   LNTANG     = ',LNTANG
      WRITE(IFLDB,*) '   LMODDEP    = ',LMODDEP
C
      WRITE(IFLDB,*) 'COMMON BLOCK: BOUNDR'
      WRITE(IFLDB,*) '   RSURF(1)   = ',RSURF(1)
      WRITE(IFLDB,*) '   RSURF(2)   = ',RSURF(2)
      WRITE(IFLDB,*) '   RSURF(3)   = ',RSURF(3)
      DO 230 N=1,NINLT
      WRITE(IFLDB,*) '   RINLT(1,',N,') =',RINLT(1,N)
      WRITE(IFLDB,*) '   RINLT(2,',N,') =',RINLT(2,N)
      WRITE(IFLDB,*) '   RINLT(3,',N,') =',RINLT(3,N)
      WRITE(IFLDB,*) '   RINLT(4,',N,') =',RINLT(4,N)
      WRITE(IFLDB,*) '   RINLT(5,',N,') =',RINLT(5,N)
      WRITE(IFLDB,*) '   RINLT(6,',N,') =',RINLT(6,N)
      WRITE(IFLDB,*) '   RINLT(7,',N,') =',RINLT(7,N)
      WRITE(IFLDB,*) '   RINLT(8,',N,') =',RINLT(8,N)
  230 CONTINUE
      DO 235 N=1,NOUTLT
      WRITE(IFLDB,*) '   ROUTLT(1,',N,') =',ROUTLT(1,N)
      WRITE(IFLDB,*) '   ROUTLT(2,',N,') =',ROUTLT(2,N)
  235 CONTINUE
      DO 236 N=1,4
      WRITE(IFLDB,*) '   NSOMER(',N,') =',NSOMER(N)
  236 CONTINUE
      DO 240 N=1,NWALL
      WRITE(IFLDB,*) '   RWALL(1,',N,') =',RWALL(1,N)
      WRITE(IFLDB,*) '   RWALL(2,',N,') =',RWALL(2,N)
      WRITE(IFLDB,*) '   RWALL(3,',N,') =',RWALL(3,N)
      WRITE(IFLDB,*) '   RWALL(4,',N,') =',RWALL(4,N)
      WRITE(IFLDB,*) '   RWALL(5,',N,') =',RWALL(5,N)
  240 CONTINUE
      WRITE(IFLDB,*) '   RDWALT     = ',RDWALT
      WRITE(IFLDB,*) '   RDWALC     = ',RDWALC
      DO 250 N=1,NTIDE
      WRITE(IFLDB,*) '   RTIDE(1,1,',N,')=',RTIDE(1,1,N)
      WRITE(IFLDB,*) '   RTIDE(2,1,',N,')=',RTIDE(2,1,N)
      WRITE(IFLDB,*) '   RTIDE(1,2,',N,')=',RTIDE(1,2,N)
      WRITE(IFLDB,*) '   RTIDE(2,2,',N,')=',RTIDE(2,2,N)
      WRITE(IFLDB,*) '   RTIDE(1,3,',N,')=',RTIDE(1,3,N)
      WRITE(IFLDB,*) '   RTIDE(2,3,',N,')=',RTIDE(2,3,N)
      WRITE(IFLDB,*) '   RTIDE(1,4,',N,')=',RTIDE(1,4,N)
      WRITE(IFLDB,*) '   RTIDE(2,4,',N,')=',RTIDE(2,4,N)
      WRITE(IFLDB,*) '   RTIDE(1,5,',N,')=',RTIDE(1,5,N)
      WRITE(IFLDB,*) '   RTIDE(2,5,',N,')=',RTIDE(2,5,N)
      WRITE(IFLDB,*) '   RTIDE(1,6,',N,')=',RTIDE(1,6,N)
      WRITE(IFLDB,*) '   RTIDE(2,6,',N,')=',RTIDE(2,6,N)
      WRITE(IFLDB,*) '   RTIDE(1,7,',N,')=',RTIDE(1,7,N)
      WRITE(IFLDB,*) '   RTIDE(2,7,',N,')=',RTIDE(2,7,N)
      WRITE(IFLDB,*) '   RTIDE(1,8,',N,')=',RTIDE(1,8,N)
      WRITE(IFLDB,*) '   RTIDE(2,8,',N,')=',RTIDE(2,8,N)
 250   CONTINUE
      WRITE(IFLDB,*) '   HTIDE0        =',HTIDE0
      DO 255 N=1,NTIDE
      WRITE(IFLDB,*) '   HTIDE(1,',N,')=',HTIDE(1,N)
      WRITE(IFLDB,*) '   HTIDE(2,',N,')=',HTIDE(2,N)
 255   CONTINUE
      DO 260 N=1,8
      WRITE(IFLDB,*) '   ROMEG(',N,')=',ROMEG(N)
 260  CONTINUE
      WRITE(IFLDB,*) '   AMP         =',AMP
      WRITE(IFLDB,*) '   TTT         =',TTT
      WRITE(IFLDB,*) '   ALL         =',ALL
      WRITE(IFLDB,*) '   HHH         =',HHH
      WRITE(IFLDB,*) '   AXX         =',AXX
C
      WRITE(IFLDB,*) 'COMMON BLOCK: CONNEC'
      WRITE(IFLDB,*) '   MAXPE  =',MAXPE
      WRITE(IFLDB,*) '   IDCON  =',IDCON(1:7)
      DO N=1,NSIZE
         WRITE(IFLDB,*) '   IPECON =',IPECON(1:8,N)
      ENDDO
      DO N=1,NSIZE
         WRITE(IFLDB,*) '   IDTABL =',IDTABL(1:2,N)
      ENDDO
      DO N=0,NSIZE-1
         WRITE(IFLDB,*) '   NUMPE  =',NUMPE(1:2,N)
      ENDDO
      DO N=1,NSIZE
         WRITE(IFLDB,*) '   NUMCOM  =',NUMCOM(1:5,N)
      ENDDO
      WRITE(IFLDB,*) '   NSIZEALL =',NSIZEALL
      WRITE(IFLDB,*) '   NSIZE  =',NSIZE
      WRITE(IFLDB,*) '   NRANK  =',NRANK
      DO N=1,NSIZE
         WRITE(IFLDB,*) '   NMFILE  =',trim(NMFILE(N))
      ENDDO
C
      WRITE(IFLDB,*) 'COMMON BLOCK: GLOBAL'
      WRITE(IFLDB,*) '   MXG    =',MXG
      WRITE(IFLDB,*) '   MYG    =',MYG
      WRITE(IFLDB,*) '   MZG    =',MZG
      WRITE(IFLDB,*) '   NPROC  =',NPROC
      WRITE(IFLDB,*) '   CHILDCOMM=',CHILDCOMM
      DO N=1,NSIZE
      WRITE(IFLDB,*) '   INDCOM =',INDCOM(1:6,N)
      ENDDO
      DO N=1,NSIZE
      WRITE(IFLDB,*) '   INDCM2 =',INDCM2(1:6,N)
      ENDDO
      WRITE(IFLDB,*) '   IAUTOD =',IAUTOD
      WRITE(IFLDB,*) '   MYPROC =',MYPROC
      WRITE(IFLDB,*) '   MYIS   =',MYIS
      WRITE(IFLDB,*) '   MYIE   =',MYIE
      WRITE(IFLDB,*) '   MYJS   =',MYJS
      WRITE(IFLDB,*) '   MYJE   =',MYJE
C
      WRITE(IFLDB,*) 'COMMON BLOCK: DOMAIN'
      WRITE(IFLDB,*) '   MX     =',MX
      WRITE(IFLDB,*) '   MY     =',MY
      WRITE(IFLDB,*) '   MZ     =',MZ
      WRITE(IFLDB,*) '   MXM    =',MXM
      WRITE(IFLDB,*) '   MYM    =',MYM
      WRITE(IFLDB,*) '   MZM    =',MZM
      WRITE(IFLDB,*) '   MXYZ   =',MXYZ
      WRITE(IFLDB,*) '   MXY    =',MXY
      WRITE(IFLDB,*) '   NXYZ   =',NXYZ
      WRITE(IFLDB,*) '   MLWALL =',MLWALL
      WRITE(IFLDB,*) '   MLWALL1=',MLWALL1
      WRITE(IFLDB,*) '   MLWALP =',MLWALP
      WRITE(IFLDB,*) '   MLWALB =',MLWALB
      WRITE(IFLDB,*) '   MLWALBX=',MLWALBX
      WRITE(IFLDB,*) '   MLOFL  =',MLOFL
      WRITE(IFLDB,*) '   MLOFLX =',MLOFLX
C
      WRITE(IFLDB,*) 'COMMON BLOCK: FILEC'
      WRITE(IFLDB,*) '   CLINE(  1: 33) =',CLINE(1:33)
      WRITE(IFLDB,*) '   CLINE( 34: 66) =',CLINE(34:66)
      WRITE(IFLDB,*) '   CLINE( 67: 99) =',CLINE(67:99)
      WRITE(IFLDB,*) '   CLINE(100:132) =',CLINE(100:132)
      WRITE(IFLDB,*) '   CNUL(  1: 33)  =',CNUL(1:33)
      WRITE(IFLDB,*) '   CNUL( 34: 66)  =',CNUL(34:66)
      WRITE(IFLDB,*) '   CNUL( 67: 99)  =',CNUL(67:99)
      WRITE(IFLDB,*) '   CNUL(100:132)  =',CNUL(100:132)
      WRITE(IFLDB,*) '   CFLNM          =',CFLNM(1:IFLNM)
C
      WRITE(IFLDB,*) 'COMMON BLOCK: FILEI'
      WRITE(IFLDB,*) '   IFLNM          =',IFLNM
C
      WRITE(IFLDB,*) 'COMMON BLOCK: GRID'
      DO 300 N=1,MXM
      WRITE(IFLDB,*) '   XGRID(',N,') =',XGRID(N)
  300 CONTINUE
      DO 310 N=1,MYM
      WRITE(IFLDB,*) '   YGRID(',N,') =',YGRID(N)
  310 CONTINUE
      DO 320 N=1,MZM
      WRITE(IFLDB,*) '   ZGRID(',N,') =',ZGRID(N)
  320 CONTINUE
      WRITE(IFLDB,*) '   HLMT         =',HLMT
C
      WRITE(IFLDB,*) 'COMMON BLOCK: INITLI'
      WRITE(IFLDB,*) '   NHINIT      =',NHINIT
      DO 400 N=1,NHINIT
      WRITE(IFLDB,*) '   IHINIT(1,',N,') =',IHINIT(1,N)
      WRITE(IFLDB,*) '   IHINIT(2,',N,') =',IHINIT(2,N)
      WRITE(IFLDB,*) '   IHINIT(3,',N,') =',IHINIT(3,N)
      WRITE(IFLDB,*) '   IHINIT(4,',N,') =',IHINIT(4,N)
  400 CONTINUE
C
      WRITE(IFLDB,*) 'COMMON BLOCK: INITLR'
      WRITE(IFLDB,*) '   UUINIT    =',UUINIT
      WRITE(IFLDB,*) '   VVINIT    =',VVINIT
      WRITE(IFLDB,*) '   WWINIT    =',WWINIT
      WRITE(IFLDB,*) '   AKINIT    =',AKINIT
      WRITE(IFLDB,*) '   EPINIT    =',EPINIT
      WRITE(IFLDB,*) '   TTINIT    =',TTINIT
      WRITE(IFLDB,*) '   CCINIT    =',CCINIT
      DO 410 N=1,NHINIT
      WRITE(IFLDB,*) '   HHINIT(',N,') =',HHINIT(N)
  410 CONTINUE
C
      WRITE(IFLDB,*) 'COMMON BLOCK: MATRXI'
      WRITE(IFLDB,*) '   ITRMTX =',ITRMTX
      WRITE(IFLDB,*) '   MAXMTX =',MAXMTX
      WRITE(IFLDB,*) '   LPRMTX =',LPRMTX
C
      WRITE(IFLDB,*) 'COMMON BLOCK: MATRXR'
      WRITE(IFLDB,*) '   EPSMTX =',EPSMTX
      WRITE(IFLDB,*) '   EPRMTX =',EPRMTX
      WRITE(IFLDB,*) '   RNRMTX =',RNRMTX
C
      WRITE(IFLDB,*) 'COMMON BLOCK: MODELI'
      WRITE(IFLDB,*) '   MLNS  =',MLNS
      WRITE(IFLDB,*) '   LSURF =',LSURF
      WRITE(IFLDB,*) '   LTYPH =',LTYPH
      WRITE(IFLDB,*) '   LTEMP =',LTEMP
      WRITE(IFLDB,*) '   LCONC =',LCONC
      WRITE(IFLDB,*) '   LDENS =',LDENS
      WRITE(IFLDB,*) '   LTURB =',LTURB
      WRITE(IFLDB,*) '   NFN   =',NFN
      WRITE(IFLDB,*) '   IGM2S =',IGM2S
      WRITE(IFLDB,*) '   IGM2B =',IGM2B
      WRITE(IFLDB,*) '   ISEAWL=',ISEAWL
      WRITE(IFLDB,*) '   IHONMA=',IHONMA
      WRITE(IFLDB,*) '   IAIDA =',IAIDA
      WRITE(IFLDB,*) '   IBKSTP=',IBKSTP
      WRITE(IFLDB,*) '   IMVERT=',IMVERT
      WRITE(IFLDB,*) '   LDISS =',LDISS
      WRITE(IFLDB,*) '   LSEDI =',LSEDI
      WRITE(IFLDB,*) '   LBRKW =',LBRKW
      WRITE(IFLDB,*) '   LAIR  =',LAIR
      WRITE(IFLDB,*) '   IFALLW=',IFALLW
      WRITE(IFLDB,*) '   JFALLW=',JFALLW
      WRITE(IFLDB,*) '   LDISP =',LDISP
      WRITE(IFLDB,*) '   LSTOCDS=',LSTOCDS
      DO 420 N=1,5
        WRITE(IFLDB,*) '   ISW(',N,') =',ISW(N)
  420 CONTINUE
      DO 425 N=1,NFN
        WRITE(IFLDB,*) '   IFNTBL(1:4,',N,') =',IFNTBL(1:4,N)
  425 CONTINUE
C
      WRITE(IFLDB,*) 'COMMON BLOCK: MODELR'
      WRITE(IFLDB,*) '   GRAV =',GRAV
      WRITE(IFLDB,*) '   GAMMAS=',GM2S
      WRITE(IFLDB,*) '   GAMMAB=',GM2B
      WRITE(IFLDB,*) '   PARAMF =',PARAMF
      WRITE(IFLDB,*) '   PARAMF2=',PARAMF2
      WRITE(IFLDB,*) '   PARAMV =',PARAMV
      WRITE(IFLDB,*) '   PARAMV2=',PARAMV2
      WRITE(IFLDB,*) '   PARAMK =',PARAMK
      WRITE(IFLDB,*) '   PARAMK2=',PARAMK2
      WRITE(IFLDB,*) '   PARAMT =',PARAMT
      WRITE(IFLDB,*) '   PARAMT2=',PARAMT2
      WRITE(IFLDB,*) '   PARAMC =',PARAMC
      WRITE(IFLDB,*) '   PARAMC2=',PARAMC2
      WRITE(IFLDB,*) '   PRT =',PRT
      WRITE(IFLDB,*) '   SCT =',SCT
      WRITE(IFLDB,*) '   CORI=',CORI
      WRITE(IFLDB,*) '   GXB =',GXB
      WRITE(IFLDB,*) '   GLH =',GLH
      WRITE(IFLDB,*) '   GZH =',GZH
      WRITE(IFLDB,*) '   CSMG=',CSMG
      WRITE(IFLDB,*) '   EPSH=',EPSH
      WRITE(IFLDB,*) '   EPST=',EPST
      WRITE(IFLDB,*) '   RIMP=',RIMP
      WRITE(IFLDB,*) '   HAIDA=',HAIDA
      WRITE(IFLDB,*) '   DKENNEDY=',DKENNEDY
      WRITE(IFLDB,*) '   DKENN1=',DKENN1
      WRITE(IFLDB,*) '   DKENN2=',DKENN2
      WRITE(IFLDB,*) '   DKENN3=',DKENN3
      WRITE(IFLDB,*) '   BETAIWA=',BETAIWA
      WRITE(IFLDB,*) '   SAFEBRKW=',SAFEBRKW
      WRITE(IFLDB,*) '   CFALLW0 =',CFALLW0
      WRITE(IFLDB,*) '   DFALLWN0=',DFALLWN0
      WRITE(IFLDB,*) '   DFALLWT0=',DFALLWT0
      WRITE(IFLDB,*) '   DISPBETA=',DISPBETA
      WRITE(IFLDB,*) '   DISPLIM =',DISPLIM
      WRITE(IFLDB,*) '   DISPBCLIM=',DISPBCLIM
      DO 430 N=1,NFN
        WRITE(IFLDB,*) '   FNVAL(',N,') =',FNVAL(N)
  430 CONTINUE
      DO 435 N=1,4
        WRITE(IFLDB,*) '   AABB(',N,') =',AABB(N)
  435 CONTINUE
C
      WRITE(IFLDB,*) 'COMMON BLOCK: TURBR'
      WRITE(IFLDB,*) '   AKAR =',AKAR
      WRITE(IFLDB,*) '   TAA  =',TAA
      WRITE(IFLDB,*) '   CMU  =',CMU
      WRITE(IFLDB,*) '   SGK  =',SGK
      WRITE(IFLDB,*) '   SGE  =',SGE
      WRITE(IFLDB,*) '   SGT  =',SGT
      WRITE(IFLDB,*) '   TC1  =',TC1
      WRITE(IFLDB,*) '   TC2  =',TC2
      WRITE(IFLDB,*) '   TC3  =',TC3
      WRITE(IFLDB,*) '   TCE  =',TCE
      WRITE(IFLDB,*) '   AKMIN=',AKMIN
      WRITE(IFLDB,*) '   EPMIN=',EPMIN
      WRITE(IFLDB,*) '   TVSMAX=',TVSMAX
      WRITE(IFLDB,*) '   TVSMIN=',TVSMIN
C
      WRITE(IFLDB,*) 'COMMON BLOCK: MYCNST'
      WRITE(IFLDB,*) '   RKAR =',RKAR
      WRITE(IFLDB,*) '   RA1  =',RA1
      WRITE(IFLDB,*) '   RA2  =',RA2
      WRITE(IFLDB,*) '   RB1  =',RB1
      WRITE(IFLDB,*) '   RB2  =',RB2
      WRITE(IFLDB,*) '   RC1  =',RC1
      WRITE(IFLDB,*) '   RCC  =',RCC
      WRITE(IFLDB,*) '   RE1  =',RE1
      WRITE(IFLDB,*) '   RE2  =',RE2
      WRITE(IFLDB,*) '   PARAMQ=',PARAMQ
      WRITE(IFLDB,*) '   PARAMQ2=',PARAMQ2
      WRITE(IFLDB,*) '   TVSVMX=',TVSVMX
C
      WRITE(IFLDB,*) 'COMMON BLOCK: TYPHOI'
      WRITE(IFLDB,*) '   IHNC1 =',IHNC1
      WRITE(IFLDB,*) '   IHNCM =',IHNCM
      WRITE(IFLDB,*) '   IHNDS =',IHNDS
      WRITE(IFLDB,*) '   NS1   =',NS1
      WRITE(IFLDB,*) '   NS2   =',NS2
      WRITE(IFLDB,*) '   NS3   =',NS3
      WRITE(IFLDB,*) '   NS4   =',NS4
      WRITE(IFLDB,*) '   NS5   =',NS5
      WRITE(IFLDB,*) '   NE1   =',NE1
      WRITE(IFLDB,*) '   NE2   =',NE2
      WRITE(IFLDB,*) '   NE3   =',NE3
      WRITE(IFLDB,*) '   NE4   =',NE4
      WRITE(IFLDB,*) '   NE5   =',NE5
      WRITE(IFLDB,*) '   NTYH  =',NTYH
C
      WRITE(IFLDB,*) 'COMMON BLOCK: TYPHOR'
      DO 440 N=1,NTYH
      WRITE(IFLDB,*) '   TX(',N,')=',TX(N)
      WRITE(IFLDB,*) '   TY(',N,')=',TY(N)
      WRITE(IFLDB,*) '   TP(',N,')=',TP(N)
      WRITE(IFLDB,*) '   TR(',N,')=',TR(N)
      WRITE(IFLDB,*) '   TU(',N,')=',TU(N)
      WRITE(IFLDB,*) '   TV(',N,')=',TV(N)
  440 CONTINUE
      WRITE(IFLDB,*) '   XLN      =',XLN
      WRITE(IFLDB,*) '   YLT      =',YLT
      WRITE(IFLDB,*) '   C1       =',C1
      WRITE(IFLDB,*) '   C2       =',C2
C
      WRITE(IFLDB,*) 'COMMON BLOCK: OBSTI'
      WRITE(IFLDB,*) '   LOBST =',LOBST
      WRITE(IFLDB,*) '   NOBSS =',NOBSS
      WRITE(IFLDB,*) '   NOBSP =',NOBSP
      WRITE(IFLDB,*) '   NPORS =',NPORS
      WRITE(IFLDB,*) '   NFRIC =',NFRIC
      WRITE(IFLDB,*) '   NSEA  =',NSEA
      WRITE(IFLDB,*) '   LFOBS =',LFOBS
      WRITE(IFLDB,*) '   NFOBS =',NFOBS
      DO 500 N=1,NOBSS
      WRITE(IFLDB,*) '   IOBSS(1,',N,') =',IOBSS(1,N)
      WRITE(IFLDB,*) '   IOBSS(2,',N,') =',IOBSS(2,N)
      WRITE(IFLDB,*) '   IOBSS(3,',N,') =',IOBSS(3,N)
      WRITE(IFLDB,*) '   IOBSS(4,',N,') =',IOBSS(4,N)
      WRITE(IFLDB,*) '   IOBSS(5,',N,') =',IOBSS(5,N)
      WRITE(IFLDB,*) '   IOBSS(6,',N,') =',IOBSS(6,N)
  500 CONTINUE
      DO 510 N=1,NOBSP
      WRITE(IFLDB,*) '   IOBSP(1,',N,') =',IOBSP(1,N)
      WRITE(IFLDB,*) '   IOBSP(2,',N,') =',IOBSP(2,N)
      WRITE(IFLDB,*) '   IOBSP(3,',N,') =',IOBSP(3,N)
      WRITE(IFLDB,*) '   IOBSP(4,',N,') =',IOBSP(4,N)
      WRITE(IFLDB,*) '   IOBSP(5,',N,') =',IOBSP(5,N)
      WRITE(IFLDB,*) '   IOBSP(6,',N,') =',IOBSP(6,N)
      WRITE(IFLDB,*) '   IOBSP(7,',N,') =',IOBSP(7,N)
  510 CONTINUE
      DO 520 N=1,NPORS
      WRITE(IFLDB,*) '   IPORS(1,',N,') =',IPORS(1,N)
      WRITE(IFLDB,*) '   IPORS(2,',N,') =',IPORS(2,N)
      WRITE(IFLDB,*) '   IPORS(3,',N,') =',IPORS(3,N)
      WRITE(IFLDB,*) '   IPORS(4,',N,') =',IPORS(4,N)
      WRITE(IFLDB,*) '   IPORS(5,',N,') =',IPORS(5,N)
      WRITE(IFLDB,*) '   IPORS(6,',N,') =',IPORS(6,N)
      WRITE(IFLDB,*) '   IPORS(7,',N,') =',IPORS(7,N)
  520 CONTINUE
      DO 530 N=1,NFRIC
      WRITE(IFLDB,*) '   IFRIC(1,',N,') =',IFRIC(1,N)
      WRITE(IFLDB,*) '   IFRIC(2,',N,') =',IFRIC(2,N)
      WRITE(IFLDB,*) '   IFRIC(3,',N,') =',IFRIC(3,N)
      WRITE(IFLDB,*) '   IFRIC(4,',N,') =',IFRIC(4,N)
      WRITE(IFLDB,*) '   IFRIC(5,',N,') =',IFRIC(5,N)
      WRITE(IFLDB,*) '   IFRIC(6,',N,') =',IFRIC(6,N)
  530 CONTINUE
      DO 540 N=1,NFRIC
      WRITE(IFLDB,*) '   ISEA(1,',N,') =',ISEA(1,N)
      WRITE(IFLDB,*) '   ISEA(2,',N,') =',ISEA(2,N)
  540 CONTINUE
C
      WRITE(IFLDB,*) 'COMMON BLOCK: OBSTR'
      DO 550 N=1,NPORS
      WRITE(IFLDB,*) '   RPORS(1,',N,') =',RPORS(1,N)
      WRITE(IFLDB,*) '   RPORS(2,',N,') =',RPORS(2,N)
      WRITE(IFLDB,*) '   RPORS(3,',N,') =',RPORS(3,N)
      WRITE(IFLDB,*) '   RPORS(4,',N,') =',RPORS(4,N)
      WRITE(IFLDB,*) '   RPORS(5,',N,') =',RPORS(5,N)
      WRITE(IFLDB,*) '   RPORS(6,',N,') =',RPORS(6,N)
      WRITE(IFLDB,*) '   RPORS(7,',N,') =',RPORS(7,N)
      WRITE(IFLDB,*) '   RPORS(8,',N,') =',RPORS(8,N)
      WRITE(IFLDB,*) '   RPORS(9,',N,') =',RPORS(9,N)
      WRITE(IFLDB,*) '   RPORS(10,',N,') =',RPORS(10,N)
  550 CONTINUE
      DO 560 N=1,NFRIC
      WRITE(IFLDB,*) '   RFRIC(',N,') =',RFRIC(N)
  560 CONTINUE
      DO 570 N=1,NFOBS
      WRITE(IFLDB,*) '   FPORS(1,',N,') =',FPORS(1,N)
      WRITE(IFLDB,*) '   FPORS(2,',N,') =',FPORS(2,N)
      WRITE(IFLDB,*) '   FPORS(3,',N,') =',FPORS(3,N)
      WRITE(IFLDB,*) '   FPORS(4,',N,') =',FPORS(4,N)
      WRITE(IFLDB,*) '   FPORS(5,',N,') =',FPORS(5,N)
      WRITE(IFLDB,*) '   FPORS(6,',N,') =',FPORS(6,N)
      WRITE(IFLDB,*) '   FPORS(7,',N,') =',FPORS(7,N)
      DO 575 M=3,5
        FPORS(M,N) = FPORS(M,N)*A2RAD
  575 CONTINUE
  570 CONTINUE
C
      WRITE(IFLDB,*) 'COMMON BLOCK: OUTPUC'
      WRITE(IFLDB,*) '   CLIST(1) =',CLIST(1)
      WRITE(IFLDB,*) '   CLIST(2) =',CLIST(2)
      WRITE(IFLDB,*) '   CLIST(3) =',CLIST(3)
      WRITE(IFLDB,*) '   CLIST(4) =',CLIST(4)
      WRITE(IFLDB,*) '   CLIST(5) =',CLIST(5)
      WRITE(IFLDB,*) '   CLIST(6) =',CLIST(6)
      WRITE(IFLDB,*) '   CLIST(7) =',CLIST(7)
      WRITE(IFLDB,*) '   CLIST(8) =',CLIST(8)
      WRITE(IFLDB,*) '   CLIST(9) =',CLIST(9)
      WRITE(IFLDB,*) '   CLIST(10) =',CLIST(10)
      WRITE(IFLDB,*) '   CLIST(11) =',CLIST(11)
      WRITE(IFLDB,*) '   CLIST(12) =',CLIST(12)
      WRITE(IFLDB,*) '   CLIST(13) =',CLIST(13)
      WRITE(IFLDB,*) '   CLIST(14) =',CLIST(14)
      WRITE(IFLDB,*) '   CLIST(15) =',CLIST(15)
      WRITE(IFLDB,*) '   CLIST(16) =',CLIST(16)
      WRITE(IFLDB,*) '   CLIST(17) =',CLIST(17)
      WRITE(IFLDB,*) '   CLIST(18) =',CLIST(18)
      WRITE(IFLDB,*) '   CLIST(19) =',CLIST(19)
      WRITE(IFLDB,*) '   CLIST(20) =',CLIST(20)
      WRITE(IFLDB,*) '   CLIST(21) =',CLIST(21)
      WRITE(IFLDB,*) '   CLIST(22) =',CLIST(22)
      WRITE(IFLDB,*) '   CLIST(23) =',CLIST(23)
      WRITE(IFLDB,*) '   CLIST(24) =',CLIST(24)
      WRITE(IFLDB,*) '   CLIST(25) =',CLIST(25)
      WRITE(IFLDB,*) '   CLIST(26) =',CLIST(26)
      WRITE(IFLDB,*) '   CLIST(27) =',CLIST(27)
      WRITE(IFLDB,*) '   CLIST(28) =',CLIST(28)
      WRITE(IFLDB,*) '   CLIST(29) =',CLIST(29)
      WRITE(IFLDB,*) '   CLIST(30) =',CLIST(30)
      WRITE(IFLDB,*) '   CGRPH(1) =',CGRPH(1)
      WRITE(IFLDB,*) '   CGRPH(2) =',CGRPH(2)
      WRITE(IFLDB,*) '   CGRPH(3) =',CGRPH(3)
      WRITE(IFLDB,*) '   CGRPH(4) =',CGRPH(4)
      WRITE(IFLDB,*) '   CGRPH(5) =',CGRPH(5)
      WRITE(IFLDB,*) '   CGRPH(6) =',CGRPH(6)
      WRITE(IFLDB,*) '   CGRPH(7) =',CGRPH(7)
      WRITE(IFLDB,*) '   CGRPH(8) =',CGRPH(8)
      WRITE(IFLDB,*) '   CGRPH(9) =',CGRPH(9)
      WRITE(IFLDB,*) '   CGRPH(10) =',CGRPH(10)
      WRITE(IFLDB,*) '   CGRPH(11) =',CGRPH(11)
      WRITE(IFLDB,*) '   CGRPH(12) =',CGRPH(12)
      WRITE(IFLDB,*) '   CGRPH(13) =',CGRPH(13)
      WRITE(IFLDB,*) '   CGRPH(14) =',CGRPH(14)
      WRITE(IFLDB,*) '   CGRPH(15) =',CGRPH(15)
      WRITE(IFLDB,*) '   CGRPH(16) =',CGRPH(16)
      WRITE(IFLDB,*) '   CGRPH(17) =',CGRPH(17)
      WRITE(IFLDB,*) '   CGRPH(18) =',CGRPH(18)
      WRITE(IFLDB,*) '   CGRPH(19) =',CGRPH(19)
      WRITE(IFLDB,*) '   CGRPH(20) =',CGRPH(20)
      WRITE(IFLDB,*) '   CGRPH(21) =',CGRPH(21)
      WRITE(IFLDB,*) '   CGRPH(22) =',CGRPH(22)
      WRITE(IFLDB,*) '   CGRPH(23) =',CGRPH(23)
      WRITE(IFLDB,*) '   CGRPH(24) =',CGRPH(24)
      WRITE(IFLDB,*) '   CGRPH(25) =',CGRPH(25)
      WRITE(IFLDB,*) '   CGRPH(26) =',CGRPH(26)
      WRITE(IFLDB,*) '   CGRPH(27) =',CGRPH(27)
      WRITE(IFLDB,*) '   CGRPH(28) =',CGRPH(28)
      WRITE(IFLDB,*) '   CGRPH(29) =',CGRPH(29)
      WRITE(IFLDB,*) '   CGRPH(30) =',CGRPH(30)
      WRITE(IFLDB,*) '   CHIST(1) =',CHIST(1)
      WRITE(IFLDB,*) '   CHIST(2) =',CHIST(2)
      WRITE(IFLDB,*) '   CHIST(3) =',CHIST(3)
      WRITE(IFLDB,*) '   CHIST(4) =',CHIST(4)
      WRITE(IFLDB,*) '   CHIST(5) =',CHIST(5)
      WRITE(IFLDB,*) '   CHIST(6) =',CHIST(6)
      WRITE(IFLDB,*) '   CHIST(7) =',CHIST(7)
      WRITE(IFLDB,*) '   CHIST(8) =',CHIST(8)
      WRITE(IFLDB,*) '   CHIST(9) =',CHIST(9)
      WRITE(IFLDB,*) '   CHIST(10) =',CHIST(10)
      WRITE(IFLDB,*) '   CHIST(11) =',CHIST(11)
      WRITE(IFLDB,*) '   CHIST(12) =',CHIST(12)
      WRITE(IFLDB,*) '   CHIST(13) =',CHIST(13)
      WRITE(IFLDB,*) '   CHIST(14) =',CHIST(14)
      WRITE(IFLDB,*) '   CHIST(15) =',CHIST(15)
      WRITE(IFLDB,*) '   CHIST(16) =',CHIST(16)
      WRITE(IFLDB,*) '   CHIST(17) =',CHIST(17)
      WRITE(IFLDB,*) '   CHIST(18) =',CHIST(18)
      WRITE(IFLDB,*) '   CHIST(19) =',CHIST(19)
      WRITE(IFLDB,*) '   CHIST(20) =',CHIST(20)
      WRITE(IFLDB,*) '   CHIST(21) =',CHIST(21)
      WRITE(IFLDB,*) '   CHIST(22) =',CHIST(22)
      WRITE(IFLDB,*) '   CHIST(23) =',CHIST(23)
      WRITE(IFLDB,*) '   CHIST(24) =',CHIST(24)
      WRITE(IFLDB,*) '   CHIST(25) =',CHIST(25)
      WRITE(IFLDB,*) '   CHIST(26) =',CHIST(26)
      WRITE(IFLDB,*) '   CHIST(27) =',CHIST(27)
      WRITE(IFLDB,*) '   CHIST(28) =',CHIST(28)
      WRITE(IFLDB,*) '   CHIST(29) =',CHIST(29)
      WRITE(IFLDB,*) '   CHIST(30) =',CHIST(30)
C
      WRITE(IFLDB,*) 'COMMON BLOCK: OUTPUI'
      WRITE(IFLDB,*) '   LREST    =',LREST
      WRITE(IFLDB,*) '   NREST    =',NREST
      WRITE(IFLDB,*) '   IREST0   =',IREST0
      DO 600 N=1,NREST
      WRITE(IFLDB,*) '   IREST(',N,') =',IREST(N)
  600 CONTINUE
      WRITE(IFLDB,*) '   LLIST    =',LLIST
      WRITE(IFLDB,*) '   NLIST    =',NLIST
      WRITE(IFLDB,*) '   ILIST0   =',ILIST0
      DO 610 N=1,NLIST
      WRITE(IFLDB,*) '   ILIST(N) =',ILIST(N)
  610 CONTINUE
      WRITE(IFLDB,*) '   MLIST       =',MLIST
      WRITE(IFLDB,*) '   LISTT       =',LISTT
      WRITE(IFLDB,*) '   NLSECT      =',NLSECT
      DO 620 N=1,NLSECT
      WRITE(IFLDB,*) '   ILSECT(1,',N,') =',ILSECT(1,N)
      WRITE(IFLDB,*) '   ILSECT(2,',N,') =',ILSECT(2,N)
  620 CONTINUE
      WRITE(IFLDB,*) '   LGRPH    =',LGRPH
      WRITE(IFLDB,*) '   NGRPH    =',NGRPH
      WRITE(IFLDB,*) '   IGRPH0   =',IGRPH0
      DO 630 N=1,NGRPH
      WRITE(IFLDB,*) '   IGRPH(',N,') =',IGRPH(N)
  630 CONTINUE
      WRITE(IFLDB,*) '   MGRPH       =',MGRPH
      WRITE(IFLDB,*) '   LHIST       =',LHIST
      WRITE(IFLDB,*) '   IHIST0      =',IHIST0
      WRITE(IFLDB,*) '   IHIST       =',IHIST
      WRITE(IFLDB,*) '   MHIST       =',MHIST
      WRITE(IFLDB,*) '   NHCELL      =',NHCELL
      WRITE(IFLDB,*) '   NHCELLSUM   =',NHCELLSUM
      DO 640 N=1,NHCELL
      WRITE(IFLDB,*) '   IHCELL(1,',N,') =',IHCELL(1,N)
      WRITE(IFLDB,*) '   IHCELL(2,',N,') =',IHCELL(2,N)
      WRITE(IFLDB,*) '   IHCELL(3,',N,') =',IHCELL(3,N)
      WRITE(IFLDB,*) '   LHCELL(  ',N,') =',LHCELL(N)
  640 CONTINUE
      WRITE(IFLDB,*) '   LENDF    =',LENDF
      WRITE(IFLDB,*) '   NENDF    =',NENDF
      WRITE(IFLDB,*) '   IENDF0   =',IENDF0
      DO 645 N=1,NENDF
      WRITE(IFLDB,*) '   IENDF(',N,') =',IENDF(N)
  645 CONTINUE
      WRITE(IFLDB,*) '   IALFAFLOW=',IALFAFLOW
      WRITE(IFLDB,*) '   KENSAMODE=',KENSAMODE
      WRITE(IFLDB,*) '   NFRAGL   =',NFRAGL
C
      WRITE(IFLDB,*) 'COMMON BLOCK: OUTPUR'
      DO 650 N=1,NREST
      WRITE(IFLDB,*) '   RREST(',N,') =',RREST(N)
  650 CONTINUE
      DO 660 N=1,NLIST
      WRITE(IFLDB,*) '   RLIST(',N,') =',RLIST(N)
  660 CONTINUE
      DO 670 N=1,NGRPH
      WRITE(IFLDB,*) '   RGRPH(',N,') =',RGRPH(N)
  670 CONTINUE
      RHIST0 = RSTART
      WRITE(IFLDB,*) '   RHIST0   =',RHIST0
      WRITE(IFLDB,*) '   RHIST    =',RHIST
      DO 675 N=1,3
      WRITE(IFLDB,*) '   RFILE(',N,') =',RFILE(N)
  675 CONTINUE
      DO 676 N=1,NENDF
      WRITE(IFLDB,*) '   RENDF(',N,') =',RENDF(N)
  676 CONTINUE
      WRITE(IFLDB,*) '   ETIME       =',ETIME
      DO 677 N=1,NFRAGL
      WRITE(IFLDB,*) '   DFRAGL(',N,') =',DFRAGL(N)
  677 CONTINUE
C
      WRITE(IFLDB,*) 'COMMON BLOCK: PROPTY'
      WRITE(IFLDB,*) '   RHO    =',RHO
      WRITE(IFLDB,*) '   RHOA   =',RHOA
      WRITE(IFLDB,*) '   ADRHO  =',ADRHO
      WRITE(IFLDB,*) '   AMUH   =',AMUH
      WRITE(IFLDB,*) '   AMUV   =',AMUV
      WRITE(IFLDB,*) '   CP     =',CP
      WRITE(IFLDB,*) '   CNDH   =',CNDH
      WRITE(IFLDB,*) '   CNDV   =',CNDV
      WRITE(IFLDB,*) '   ANUH   =',ANUH
      WRITE(IFLDB,*) '   ANUV   =',ANUV
      WRITE(IFLDB,*) '   ALPH   =',ALPH
      WRITE(IFLDB,*) '   ALPV   =',ALPV
      WRITE(IFLDB,*) '   DIFH   =',DIFH
      WRITE(IFLDB,*) '   DIFV   =',DIFV
C
      WRITE(IFLDB,*) 'COMMON BLOCK: TABLEI'
      WRITE(IFLDB,*) '   NTABLE =',NTABLE
      DO 700 N=1,NTABLE
      WRITE(IFLDB,*) '   ITABLE(',N,') =',ITABLE(N)
  700 CONTINUE
C
      WRITE(IFLDB,*) 'COMMON BLOCK: TABLER'
      DO 710 N=1,NTABLE
      WRITE(IFLDB,*) '   TTABLE(*,',N,'),VTABLE(*,',N,')='
      DO 720 M=1,ITABLE(N)
         WRITE(IFLDB,*) '   ',TTABLE(M,N),VTABLE(M,N)
  720 CONTINUE
  710 CONTINUE
      DO 730 N=1,NTABLE
      WRITE(IFLDB,*) '   TABLE(',N,') =',TABLE(N)
  730 CONTINUE
C
      WRITE(IFLDB,*) 'COMMON BLOCK: TIMEI'
      WRITE(IFLDB,*) '   ISTEP  =',ISTEP
      WRITE(IFLDB,*) '   MAXSTP =',MAXSTP
      WRITE(IFLDB,*) '   IDT    =',IDT
      WRITE(IFLDB,*) '   NITER  =',NITER
      WRITE(IFLDB,*) '   MXITER =',MXITER
      WRITE(IFLDB,*) '   LSTART =',LSTART
C
      WRITE(IFLDB,*) 'COMMON BLOCK: TIMER'
      WRITE(IFLDB,*) '   TIME   =',TIME
      WRITE(IFLDB,*) '   DT     =',DT
      WRITE(IFLDB,*) '   DTOLD  =',DTOLD
      WRITE(IFLDB,*) '   DTV    =',DTV
      WRITE(IFLDB,*) '   RSTART =',RSTART
      WRITE(IFLDB,*) '   REND   =',REND
      WRITE(IFLDB,*) '   DTCNST =',DTCNST
      WRITE(IFLDB,*) '   DTSAFE =',DTSAFE
      WRITE(IFLDB,*) '   DTMIN  =',DTMIN
      WRITE(IFLDB,*) '   DTMAX  =',DTMAX
      WRITE(IFLDB,*) '   VELMAX =',VELMAX
C 
      WRITE(IFLDB,*) 'COMMON BLOCK: CP_NESTBC_I'
      WRITE(IFLDB,*) '   NESTFL =',NESTFL
      WRITE(IFLDB,*) '   NXY    =',NXY
      DO 740 N=1,4 
      WRITE(IFLDB,*) '   NESNS(',N,') =',NESNS(N)
  740 CONTINUE  
      DO 750 N=1,4 
      WRITE(IFLDB,*) '   NESML(',N,') =',NESML(N)
  750 CONTINUE  
      WRITE(IFLDB,*) 'COMMON BLOCK: CP_NESTBC_R'
      WRITE(IFLDB,*) '   TIMVF  =',TIMVF
      WRITE(IFLDB,*) '   TIMVB  =',TIMVB
      WRITE(IFLDB,*) '   TIMHF  =',TIMHF
      WRITE(IFLDB,*) '   TIMHB  =',TIMHB
C
      WRITE(IFLDB,*) 'COMMON BLOCK: SEDIMENTI'
      WRITE(IFLDB,*) '   MWEXSD =',MWEXSD
      WRITE(IFLDB,*) '   MCONCSD=',MCONCSD
      WRITE(IFLDB,*) '   MDIFSD =',MDIFSD
      WRITE(IFLDB,*) '   MSETSD =',MSETSD
      WRITE(IFLDB,*) '   MUSTSD =',MUSTSD
      WRITE(IFLDB,*) '   MRGHSD =',MRGHSD
      WRITE(IFLDB,*) '   MSHLSD =',MSHLSD
      WRITE(IFLDB,*) '   IBEDINI=',IBEDINI
      WRITE(IFLDB,*) '   IBEDSTR=',IBEDSTR
      WRITE(IFLDB,*) '   MFDBCKSD=',MFDBCKSD
      WRITE(IFLDB,*) '   MOFFLNSD=',MOFFLNSD
      WRITE(IFLDB,*) '   MBDSLP  =',MBDSLP
C
      WRITE(IFLDB,*) 'COMMON BLOCK: SEDIMENTR'
      WRITE(IFLDB,*) '   ZLIMSD =',ZLIMSD
      WRITE(IFLDB,*) '   SSAND  =',SSAND
      WRITE(IFLDB,*) '   DSAND  =',DSAND
      WRITE(IFLDB,*) '   GVSAND =',GVSAND
      WRITE(IFLDB,*) '   DIFHSD =',DIFHSD
      WRITE(IFLDB,*) '   DIFVSD =',DIFVSD
      WRITE(IFLDB,*) '   SCTHSD =',SCTHSD
      WRITE(IFLDB,*) '   SCTVSD =',SCTVSD
      WRITE(IFLDB,*) '   AEXSD  =',AEXSD
      WRITE(IFLDB,*) '   CMAXSD =',CMAXSD
      WRITE(IFLDB,*) '   PARAMSD=',PARAMSD
      WRITE(IFLDB,*) '   WSEDI  =',WSEDI
      WRITE(IFLDB,*) '   PSIC   =',PSIC
      WRITE(IFLDB,*) '   BEDINI =',BEDINI
      WRITE(IFLDB,*) '   TSOFFLN=',TSOFFLN
      WRITE(IFLDB,*) '   TEOFFLN=',TEOFFLN
      WRITE(IFLDB,*) '   DTOFFLN=',DTOFFLN
      WRITE(IFLDB,*) '   PHIS   =',PHIS
      WRITE(IFLDB,*) '   KCMIN  =',KCMIN
C
      WRITE(IFLDB,*) 'COMMON BLOCK: DRIFT'
      WRITE(IFLDB,*) '   OFF_INTERVAL =',OFF_INTERVAL
      WRITE(IFLDB,*) '   OFF_START    =',OFF_START
      WRITE(IFLDB,*) '   OFF_NEXT     =',OFF_NEXT
      WRITE(IFLDB,*) '   NB_SD        =',NB_SD
      WRITE(IFLDB,*) '   NB_SD_MAIN   =',NB_SD_MAIN
      WRITE(IFLDB,*) '   NOCALDM      =',NOCALDM
C
      WRITE(IFLDB,*) 'COMMON BLOCK: VVMAX'
      WRITE(IFLDB,*) '   VVMAX  =',VVMAX
C
      WRITE(IFLDB,*) 'COMMON BLOCK: CADMAS'
      WRITE(IFLDB,*) '   IB_STOC   =',IB_STOC
      WRITE(IFLDB,*) '   NB_CADMAS =',NB_CADMAS
      WRITE(IFLDB,*) '   LB_CADMAS =',LB_CADMAS
      DO 810 N=1,NB_CADMAS
         WRITE(IFLDB,*) '   IB_CADMAS(',N,') =',IB_CADMAS(N)
  810 CONTINUE
      WRITE(IFLDB,*) '   NB_SC     =',NB_SC
      WRITE(IFLDB,*) '   NIST      =',NIST
      WRITE(IFLDB,*) '   NJST      =',NJST
      WRITE(IFLDB,*) '   NKST      =',NKST
      WRITE(IFLDB,*) '   IWCAD     =',IWCAD
      WRITE(IFLDB,*) '   IECAD     =',IECAD
      WRITE(IFLDB,*) '   JSCAD     =',JSCAD
      WRITE(IFLDB,*) '   JNCAD     =',JNCAD
      WRITE(IFLDB,*) '   KBCAD     =',KBCAD
      WRITE(IFLDB,*) '   KTCAD     =',KTCAD
      DO 820 N=1,NB_CADMAS
         WRITE(IFLDB,*) '   IICAD(1:6,',N,') =',IICAD(1:6,N)
         WRITE(IFLDB,*) '   JJCAD(1:6,',N,') =',JJCAD(1:6,N)
  820 CONTINUE
C
      WRITE(IFLDB,*) 'COMMON BLOCK: AIRI'
      WRITE(IFLDB,*) '   MZA    =',MZA
      WRITE(IFLDB,*) '   MZMA   =',MZMA
      WRITE(IFLDB,*) '   LTURBA =',LTURBA
      WRITE(IFLDB,*) '   IBCAIRWES =',IBCAIRWES
      WRITE(IFLDB,*) '   IBCAIREAS =',IBCAIREAS
      WRITE(IFLDB,*) '   IBCAIRSOU =',IBCAIRSOU
      WRITE(IFLDB,*) '   IBCAIRNOR =',IBCAIRNOR
      WRITE(IFLDB,*) '   IBCAIRTOP =',IBCAIRTOP
C
      WRITE(IFLDB,*) 'COMMON BLOCK: AIRR'
      WRITE(IFLDB,*) '   UBCAIRWES =',UBCAIRWES
      WRITE(IFLDB,*) '   UBCAIREAS =',UBCAIREAS
      WRITE(IFLDB,*) '   UBCAIRSOU =',UBCAIRSOU
      WRITE(IFLDB,*) '   UBCAIRNOR =',UBCAIRNOR
      WRITE(IFLDB,*) '   VBCAIRWES =',VBCAIRWES
      WRITE(IFLDB,*) '   VBCAIREAS =',VBCAIREAS
      WRITE(IFLDB,*) '   VBCAIRSOU =',VBCAIRSOU
      WRITE(IFLDB,*) '   VBCAIRNOR =',VBCAIRNOR
      WRITE(IFLDB,*) '   AKBCAIRWES=',AKBCAIRWES
      WRITE(IFLDB,*) '   AKBCAIREAS=',AKBCAIREAS
      WRITE(IFLDB,*) '   AKBCAIRSOU=',AKBCAIRSOU
      WRITE(IFLDB,*) '   AKBCAIRNOR=',AKBCAIRNOR
      WRITE(IFLDB,*) '   EPBCAIRWES=',EPBCAIRWES
      WRITE(IFLDB,*) '   EPBCAIREAS=',EPBCAIREAS
      WRITE(IFLDB,*) '   EPBCAIRSOU=',EPBCAIRSOU
      WRITE(IFLDB,*) '   EPBCAIRNOR=',EPBCAIRNOR
      WRITE(IFLDB,*) '   PBCAIR    =',PBCAIR
      WRITE(IFLDB,*) '   UINITAIR =',UINITAIR
      WRITE(IFLDB,*) '   VINITAIR =',VINITAIR
      WRITE(IFLDB,*) '   AKINITAIR=',AKINITAIR
      WRITE(IFLDB,*) '   EPINITAIR=',EPINITAIR
      WRITE(IFLDB,*) '   RHOAIR   =',RHOAIR
      WRITE(IFLDB,*) '   AMUAIR   =',AMUAIR
      WRITE(IFLDB,*) '   PARAMAIR =',PARAMAIR
      WRITE(IFLDB,*) '   PARAMAIR2=',PARAMAIR2
      WRITE(IFLDB,*) '   VVMAXAIR =',VVMAXAIR
      WRITE(IFLDB,*) '   GZHAIR   =',GZHAIR
      WRITE(IFLDB,*) '   TIMEABC1 =',TIMEABC1
      WRITE(IFLDB,*) '   TIMEABC2 =',TIMEABC2
      DO 830 N=1,MZMA
         WRITE(IFLDB,*) '   ZGRIDA(',N,')=',ZGRIDA(N)
  830 CONTINUE
C
      WRITE(IFLDB,*) 'COMMON BLOCK: AGENTI'
      WRITE(IFLDB,*) '   NB_SM =',NB_SM
      WRITE(IFLDB,*) '   IMMTYP=',IMMTYP
      WRITE(IFLDB,*) '   IMAMS =',IMAMS
      WRITE(IFLDB,*) '   IMAME =',IMAME
      WRITE(IFLDB,*) '   IMAMI =',IMAMI
      WRITE(IFLDB,*) '   RMAMS =',RMAMS
      WRITE(IFLDB,*) '   RMAME =',RMAME
      WRITE(IFLDB,*) '   RMAMI =',RMAMI
      WRITE(IFLDB,*) '   RMAMR =',RMAMR
C
      WRITE(IFLDB,*) 'COMMON BLOCK: DESTROY'
      DO 840 N=1,NDSTSZ
         WRITE(IFLDB,*) '   DSTLMT(',N,')=',DSTLMT(N)
  840 CONTINUE
C
C      CALL FLUSH(IFLDB)
C
      RETURN
      END
