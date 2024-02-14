      SUBROUTINE VF_A2DFLT()

CD=== 概要 ===========================================================

CDT   VF_A2DFLT:コモン変数へのデフォルト値の設定

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPI.h'
      INCLUDE 'VF_ACOMPR.h'
C     INCLUDE 'VF_ACPUTR.h'
      INCLUDE 'VF_ADBGI.h'
      INCLUDE 'VF_ADBGR.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_AFILER.h'
      INCLUDE 'VF_ANUMBI.h'
C     INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSI.h'
      INCLUDE 'VF_APHYSR.h'
      INCLUDE 'VF_ATIMEI.h'
      INCLUDE 'VF_ATIMER.h'
C----------------------------------------------------------2012.03 start
      INCLUDE 'VF_ASEABT.h'
C----------------------------------------------------------2012.03 end

C==== 実行 ===========================================================

CD    -- VF_A0PRM.hの初期化 --
C     PARAMETERであるから初期化の必要なし

CD    -- VF_ACOMPI.hの初期化 --
      ICGTYP=1
      ICGMAX=500
      ICGITR=0
      ISCMVP=0
      ISCMFF=0
      ISCMK =0
      ISCMT =0
      CALL VF_ZSETI1(ISCMC,0,MAXNC)
      IBSUW0=0

CD    -- VF_ACOMPR.hの初期化 --
      CGPARA=0.95D0
      CGEPSA=1.0D-15
      CGEPSR=1.0D-12
      CGDIV =1.0D+30
      CGBNRM=0.0D0
      CGXNRM=0.0D0
      FEPS  =1.0D-6
      FLOWER=FEPS
      FUPPER=1.0D0-FEPS
      FSUM  =0.0D0
      FCUT  =0.0D0
      PLOWER=1.0D-4
      SCMVP =1.0D0
      SCMK  =1.0D0
      SCMT  =1.0D0
      CALL VF_ZSETR1(SCMC,1.0D0,MAXNC)

CD    -- VF_ACPUTR.hの初期化 --
CD    タイマーの初期化時に初期化済み

CD    -- VF_ADBGI.hの初期化 --
      CALL VF_ZSETI1(IDBGF ,0,6)
      CALL VF_ZSETI1(IDBGTD,0,6)

CD    -- VF_ADBGR.hの初期化 --
      RDBGF=0.0D0
      CALL VF_ZSETR1(RDBGTD,0.0D0,3)

CD    -- VF_AFILEI.hの初期化 --
C     リストファイルだけはオープン済み
      IENFIL=IENFIL
      IINFIL=0
      IMTFIL=0
      IMTFIL2=0
      IREFIL=0
      IPRFIL=0
      ILPFIL=ILPFIL
      IGRFIL=0
      IRSFIL=0
      ITRFIL=0
      IMMFIL=0
      ILPTYP=0
      CALL VF_ZSETI1(ILPTRN,0,3)
      CALL VF_ZSETI1(ILPARA,1,3)
      CALL VF_ZSETI1(ILPON ,0,19)
      IGRTYP=0
      CALL VF_ZSETI1(IGRTRN,0,3)
      CALL VF_ZSETI1(IGRARA,1,6)
      IGRVOR=0
      IRSTYP=0
      CALL VF_ZSETI1(IRSTRN,0,3)
      IRETYP=-1
      ITRTYP=0
      CALL VF_ZSETI1(ITRTRN,0,3)
      ITRNUM=0
      CALL VF_ZSETI2(ITRPRM,0,MAXTR1,MAXTR)
      IMMTYP=0
      CALL VF_ZSETI1(IMMTRN,0,3)
      IPRNT=0
      IPRNB=0
      IPRNP=0
      CALL VF_ZSETI2(IPRARA,0,6,MAXPRB)
      IPRIT=-1
      MTBTYP=0
      MTBTT =0
      MTBZZ =0
      MTBNOW=0
      MTBTYP2=0
      MTBTT2 =0
      MTBZZ2 =0
      MTBNOW2=0

CD    -- VF_AFILER.hの初期化 --
      CALL VF_ZSETR1(RLPTRN,0.0D0,4)
      CALL VF_ZSETR1(RGRTRN,0.0D0,4)
      CALL VF_ZSETR1(RRSTRN,0.0D0,4)
      CALL VF_ZSETR1(RTRTRN,0.0D0,4)
      CALL VF_ZSETR1(RTRVAL,0.0D0,MAXTR)
      CALL VF_ZSETR1(RMMTRN,0.0D0,4)
      PRTOLD=0.0D0
      PRTNOW=0.0D0
      DMTBT0=0.0D0
      DMTBT02=0.0D0
      ETIME=1.0D30

CD    -- VF_ANUMBI.hの初期化 --
      NUMI =0
      NUMJ =0
      NUMK =0
      NUMB =0
      NUMS =0
      LEQK =0
      LEQT =0
      LEQC =0
      NUMIJ=0
      NUMBX=0

CD    -- VF_APARAI.hの初期化 --
CD    並列環境の初期化時に初期化済み

CD    -- VF_APHYSI.hの初期化 --
      CALL VF_ZSETI2(IBCTYP, 0,4,4)
      CALL VF_ZSETI1(IDAMP ,-1,4)
      IDROP=1
      CALL VF_ZSETI1(ISCTYP, 0,2)
      NPVCB=0
      CALL VF_ZSETI1(IPVCBC, 0,MAXPVC)
      IDRGN=0

CD    -- VF_APHYSR.hの初期化 --
      UINI  =0.0D0
      VINI  =0.0D0
      WINI  =0.0D0
      RHO0  =1.0D+3
      ANU0  =1.0D-6
      GRZ0  =9.8D0
      WVLVL =0.0D0
      AKMINK=ZERO
      AKMINE=ZERO
      AKINIK=ZERO
      AKINIE=ZERO
      AKCMU =0.09D0
      AKSGK =1.0D0
      AKSGE =1.3D0
      AKC1  =1.44D0
      AKC2  =1.92D0
      AKC3  =0.0D0
      AKK0  =0.4D0
      AKA0  =5.5D0
      AKPR  =1.0D0
      CALL VF_ZSETR1(AKSM,1.0D0,MAXNC)
      TINI =2.73D+2
      TCP0 =4.2D+3
      TCN0 =5.7D-1
      TDT0 =2.73D+2
      TDR0 =1.5D-1
      CALL VF_ZSETR1(CINI,0.0D0,MAXNC)
      CALL VF_ZSETR1(CDF0,0.0D0,MAXNC)
      CALL VF_ZSETR1(CDC0,0.0D0,MAXNC)
      CALL VF_ZSETR1(CDR0,0.0D0,MAXNC)
      CALL VF_ZSETR2(BCTYP,0.0D0,11,4)
      DAMP(1,1)=0.6D0
      DAMP(2,1)=0.6D0
      DAMP(3,1)=0.0D0
      DAMP(4,1)=0.0D0
      DAMP(1,2)=0.6D0
      DAMP(2,2)=0.6D0
      DAMP(3,2)=0.0D0
      DAMP(4,2)=0.0D0
      DAMP(1,3)=0.6D0
      DAMP(2,3)=0.6D0
      DAMP(3,3)=0.0D0
      DAMP(4,3)=0.0D0
      DAMP(1,4)=0.6D0
      DAMP(2,4)=0.6D0
      DAMP(3,4)=0.0D0
      DAMP(4,4)=0.0D0
      WBUB =0.2D0
      CALL VF_ZSETR1(SCTYP,0.0D0,8)
      PVCP0=0.0D0
      PVCGM=0.0D0
      CALL VF_ZSETR1(PVCDIV,0.0D0,MAXPVC)
      CALL VF_ZSETR1(PVCPES,0.0D0,MAXPVC)
      CALL VF_ZSETR1(PVCPFS,0.0D0,MAXPVC)
      CALL VF_ZSETR1(PVCVES,0.0D0,MAXPVC)
      CALL VF_ZSETR1(PVCVFS,0.0D0,MAXPVC)
      DRGYU=0.0D0
      CALL VF_ZSETR1(DRGDR,0.0D0,MAXDRG)
      CALL VF_ZSETR1(DRGAP,0.0D0,MAXDRG)
      CALL VF_ZSETR1(DRGBT,0.0D0,MAXDRG)
      VVMAX=30.0D0

CD    -- VF_ATIMEI.hの初期化 --
      NEND  =0
      NNOW  =0
      IDTTYP=1
      LOOPS =1

CD    -- VF_ATIMER.hの初期化 --
      TEND  =0.0D0
      TNOW  =0.0D0
      DTNOW =0.0D0
      DTCNST=1.0D-3
      DTINIT=1.0D-6
      DTMIN =ZERO
      DTMAX =1.0D0/ZERO
      DTSAFE=1.0D-1

C----------------------------------------------------------2012.03 start
      ISEABT = 0
C----------------------------------------------------------2012.03 end
C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
