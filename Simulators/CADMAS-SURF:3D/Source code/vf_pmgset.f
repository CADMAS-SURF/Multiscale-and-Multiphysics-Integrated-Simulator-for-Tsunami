C----------------------------------------------------------2011.04 start
C     SUBROUTINE VF_PMGSET(XX,YY,ZZ,NF)
cmod20160721(s)
c      SUBROUTINE VF_PMGSET(XX,YY,ZZ,GGV,GGX,GGY,GGZ,XPF,YPF,ZPF,NF)
      SUBROUTINE VF_PMGSET(XX,YY,ZZ,GGV,GGX,GGY,GGZ,XPF,YPF,ZPF,NF
     &                    ,XXWK,YYWK,ZZWK,dbuf,ibuf,level)
cmod20160721(e)
C----------------------------------------------------------2011.04 end

CD=== 概要 ===========================================================

CDT   VF_PMGSET:マルチグリッド環境の親子関係をチェックし設定する

C==== 宣言 ===========================================================

cadd20160727(s)
      use mod_apara,only: 
     $   MAKE_GGV_LIST,DELETE_GGV_LIST,SET_GGV_F,SET_GGV_UVW,
     $   MAKE_P2C_LIST,MAKE_C2P_LIST
cadd20160727(e)
C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : I/O : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : I/O : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : I/O : R*8 : z方向格子座標等
C----------------------------------------------------------2011.04 start
CD    GGV(@FOR-3D@)    : I/O : R*8 : 空隙率
CD    GGX(@FOR-3D@)    : I/O : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)    : I/O : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)    : I/O : R*8 : z方向面積透過率
CD    XPF(NUMI)        : O   : R*8 : 親の格子におけるx方向の補間係数
CD    YPF(NUMJ)        : O   : R*8 : 親の格子におけるy方向の補間係数
CD    ZPF(NUMK)        : O   : R*8 : 親の格子におけるz方向の補間係数
C----------------------------------------------------------2011.04 end
CD    NF(@FOR-3D@)     : I/O : I*4 : セルの状態を示すインデックス
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    IBUF(NUMBUF*MAXBUF) : OUT :I*4 : 並列用のバッファ
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
C----------------------------------------------------------2011.04 start
      DIMENSION GGV (NUMI,NUMJ,NUMK),GGX (NUMI,NUMJ,NUMK)
      DIMENSION GGY (NUMI,NUMJ,NUMK),GGZ (NUMI,NUMJ,NUMK)
      DIMENSION XPF(NUMI),YPF(NUMJ),ZPF(NUMK)
C----------------------------------------------------------2011.04 end
      DIMENSION NF(NUMI,NUMJ,NUMK)
cadd20160721(s)
      DIMENSION XXWK(NUMI0),YYWK(NUMJ0),ZZWK(NUMK)
      DIMENSION DBUF(NUMBUF*MAXBUF),IBUF(NUMBUF*MAXBUF)
      INTEGER,ALLOCATABLE:: JSW(:)
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: 
     $                                           GWRKF,GWRKU,GWRKV,GWRKW
      INTEGER NNF,NNU,NNV,NNW
cadd20160721(e)

CD    -- 局所変数 --
C----------------------------------------------------------2011.04 start
C     DIMENSION DDD(6),III(9)
      DIMENSION III(9)
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: XWRK,YWRK,ZWRK
cmod20160803(s)
      integer,allocatable:: nfwrk(:,:,:)
C----------------------------------------------------------2011.04 end
      allocate(nfwrk(NUMI,NUMJ,NUMK))
      nfwrk(:,:,:)=1
cmod20160803(e)

C==== 実行 ===========================================================

CD    -- 自分の解析範囲 --
      XMS=XX(1,MYIS  )
      XME=XX(1,MYIE+1)
      YMS=YY(1,MYJS  )
      YME=YY(1,MYJE+1)
      ZMS=ZZ(1,2     )
      ZME=ZZ(1,NUMK  )
cmod20160721(s)
      CALL VF_ZXMP_ALLMND(XMS,XMS1,IERR)
      CALL VF_ZXMP_ALLMND(YMS,YMS1,IERR)
      CALL VF_ZXMP_ALLMXD(XME,XME1,IERR)
      CALL VF_ZXMP_ALLMXD(YME,YME1,IERR)
      IF(MYRANK.NE.0) THEN
         XMS1=0.d0
         YMS1=0.d0
         XME1=0.d0
         YME1=0.d0
      ENDIF
      ALLOCATE(JSW(0:MGPROC-1),STAT=IERR)
      JSW(:)=0
cmod20160721(e)
c      write(100+mgrank,'(a13,i5,i5)')     ' numi ,numj =',numi,numj
c      write(100+mgrank,'(a13,i5,i5)')     ' numi0,numj0=',numi0,numj0
c      write(100+mgrank,'(a13,f6.1,f6.1)') ' xms  ,xme  =',xms,xme
c      write(100+mgrank,'(a13,f6.1,f6.1)') ' yms  ,yme  =',yms,yme
c      write(100+mgrank,'(a13,f6.1,f6.1)') ' xms1 ,xme1 =',xms1,xme1
c      write(100+mgrank,'(a13,f6.1,f6.1)') ' yms1 ,yme1 =',yms1,yme1
c      write(100+mgrank,*) ''

CD    -- 親子関係をチェックし設定する --
      MGPRNK=-1
      MGCNUM=0
      DO 140 IC=0,MGPROC-1
C       * 自分が子として親候補に情報を送り、結果をもらう
        IF (MGPARE(IC+1).NE.0) THEN
          IF (MGRANK.EQ.IC) THEN
C----------------------------------------------------------2011.04 start
C           DDD(1)=XMS
C           DDD(2)=XME
C           DDD(3)=YMS
C           DDD(4)=YME
C           DDD(5)=ZMS
C           DDD(6)=ZME
C----------------------------------------------------------2011.04 end
            III(1)=MYIE-MYIS+1
            III(2)=MYJE-MYJS+1
            III(3)=NUMK-2
            III(4)=1
            III(5)=1
            III(6)=0
            III(7)=1
            III(8)=1
            III(9)=0
            IF (MYMIS.EQ.1) III(4)=0
            IF (MYMJS.EQ.1) III(5)=0
            IF (MYMIE.EQ.1) III(7)=0
            IF (MYMJE.EQ.1) III(8)=0
C----------------------------------------------------------2011.04 start
            DO 10 I=MYIS,MYIE+1
              XPF(I) = XX(1,I)
 10         CONTINUE
            DO 11 J=MYJS,MYJE+1
              YPF(J) = YY(1,J)
 11         CONTINUE
            DO 12 K=2,NUMK
              ZPF(K) = ZZ(1,K)
 12         CONTINUE
C----------------------------------------------------------2011.04 end
C           * 親候補をさがして
            DO 100 IP=0,MGPROC-1
              IF (MGPARE(IC+1).EQ.MGAREA(IP+1)) THEN
C----------------------------------------------------------2011.04 start
C               CALL VF_ZXMG_ISENDD(DDD,6,IP,IREQ,IERR)
C               CALL VF_ZXMG_WAIT(IREQ,IERR)
C----------------------------------------------------------2011.04 end
                CALL VF_ZXMG_ISENDI(III,9,IP,IREQ,IERR)
                CALL VF_ZXMG_WAIT(IREQ,IERR)
C----------------------------------------------------------2011.04 start
                CALL VF_ZXMG_ISENDD(XPF(MYIS),III(1)+1,IP,IREQ,IERR)
                CALL VF_ZXMG_WAIT(IREQ,IERR)
                CALL VF_ZXMG_ISENDD(YPF(MYJS),III(2)+1,IP,IREQ,IERR)
                CALL VF_ZXMG_WAIT(IREQ,IERR)
                CALL VF_ZXMG_ISENDD(ZPF(2),III(3)+1,IP,IREQ,IERR)
                CALL VF_ZXMG_WAIT(IREQ,IERR)
C----------------------------------------------------------2011.04 end
cmod20160726(s)
c                CALL VF_ZXMG_IRECVI(ISW,1,IP,IREQ,IERR)
                CALL VF_ZXMG_IRECVI(JSW(IP),1,IP,IREQ,IERR)
                CALL VF_ZXMG_WAIT(IREQ,IERR)
c                IF (ISW.NE.0) THEN
                IF (JSW(IP).NE.0) THEN
cmod20160726(e)
                  IF (MGPRNK.GE.0)
     &              CALL VF_A2ERR('VF_PMGSET','SECOND TIMES (MGPRNK).')
                  MGPRNK=IP
                  CALL VF_ZXMG_IRECVI(MGPINF,9,IP,IREQ,IERR)
                  CALL VF_ZXMG_WAIT(IREQ,IERR)
C----------------------------------------------------------2011.04 start
                  CALL VF_ZXMG_IRECVD(XPF(MYIS),III(1)+1,IP,IREQ,IERR)
                  CALL VF_ZXMG_WAIT(IREQ,IERR)
                  CALL VF_ZXMG_IRECVD(YPF(MYJS),III(2)+1,IP,IREQ,IERR)
                  CALL VF_ZXMG_WAIT(IREQ,IERR)
                  CALL VF_ZXMG_IRECVD(ZPF(2),III(3)+1,IP,IREQ,IERR)
                  CALL VF_ZXMG_WAIT(IREQ,IERR)

cmod20160726(s)
c                  CALL VF_PMGGGT(GGV,GGX,GGY,GGZ,XPF,YPF,ZPF,NF,IP)
cmod20160726(e)
C----------------------------------------------------------2011.04 start
                ENDIF
              ENDIF
 100        CONTINUE
CDEBUG            IF (MGPRNK.LT.0)
CDEBUG     &              CALL VF_A2ERR('VF_PMGSET','NOT FOUND (MGPRNK).')
          ENDIF
C         * 自分が親の候補なら調べて、相手に結果を送る
          IF (MGPARE(IC+1).EQ.MGAREA(MGRANK+1)) THEN
C----------------------------------------------------------2011.04 start
C           CALL VF_ZXMG_IRECVD(DDD,6,IC,IREQ,IERR)
C           CALL VF_ZXMG_WAIT(IREQ,IERR)
C----------------------------------------------------------2011.04 end
c      write(100+mgrank,*) 'vf_pmgset:irecvi level=',level
            CALL VF_ZXMG_IRECVI(III,9,IC,IREQ,IERR)
            CALL VF_ZXMG_WAIT(IREQ,IERR)
C----------------------------------------------------------2011.04 start
            NXC = III(1)
            NYC = III(2)
            NZC = III(3)
            ALLOCATE(XWRK(0:NXC),YWRK(0:NYC),ZWRK(0:NZC))
c      write(100+mgrank,*) 'vf_pmgset:irecvd level=',level
            CALL VF_ZXMG_IRECVD(XWRK,NXC+1,IC,IREQ,IERR)
            CALL VF_ZXMG_WAIT(IREQ,IERR)
            CALL VF_ZXMG_IRECVD(YWRK,NYC+1,IC,IREQ,IERR)
            CALL VF_ZXMG_WAIT(IREQ,IERR)
            CALL VF_ZXMG_IRECVD(ZWRK,NZC+1,IC,IREQ,IERR)
            CALL VF_ZXMG_WAIT(IREQ,IERR)
C----------------------------------------------------------2011.04 end
C----------------------------------------------------------2011.04 start
C           XCS=DDD(1)
C           XCE=DDD(2)
C           YCS=DDD(3)
C           YCE=DDD(4)
C           ZCS=DDD(5)
C           ZCE=DDD(6)
            XCS=XWRK(0)
            XCE=XWRK(NXC)
            YCS=YWRK(0)
            YCE=YWRK(NYC)
            ZCS=ZWRK(0)
            ZCE=ZWRK(NZC)
C----------------------------------------------------------2011.04 end
            ISW=0
C           * 子が自分に含まれるなら、自分が親
cmod20160721(s)
            IF ( XMS1.LE.XCS .AND. XCE.LE.XME1 .AND. 
     &           YMS1.LE.YCS .AND. YCE.LE.YME1 .AND. 
     &           ZMS .LE.ZCS .AND. ZCE.LE.ZME ) THEN
CDEBUG            IF (XMS.LE.XCS .AND. XCE.LE.XME .AND. 
CDEBUG     &          YMS.LE.YCS .AND. YCE.LE.YME .AND. 
CDEBUG     &          ZMS.LE.ZCS .AND. ZCE.LE.ZME      ) THEN
cmod20160721(e)
              ISW=1
              MGCNUM=MGCNUM+1
              MGCRNK(  MGCNUM)=IC
              MGCINF(1,MGCNUM)=III(1)
              MGCINF(2,MGCNUM)=III(2)
              MGCINF(3,MGCNUM)=III(3)
              MGCINF(4,MGCNUM)=III(4)
              MGCINF(5,MGCNUM)=III(5)
              MGCINF(6,MGCNUM)=III(6)
              MGCINF(7,MGCNUM)=III(7)
              MGCINF(8,MGCNUM)=III(8)
              MGCINF(9,MGCNUM)=III(9)
C             * イコール判定で座標値を比較(CAKIYC@@)
C----------------------------------------------------------2011.04 start
C             IS=0
C             IE=0
C             DO 110 I=MYIS,MYIE+1
C               IF (XX(1,I).EQ.XCS) IS=I
C               IF (XX(1,I).EQ.XCE) IE=I-1
C110          CONTINUE
C             JS=0
C             JE=0
C             DO 120 J=MYJS,MYJE+1
C               IF (YY(1,J).EQ.YCS) JS=J
C               IF (YY(1,J).EQ.YCE) JE=J-1
C120          CONTINUE
C             KS=0
C             KE=0
C             DO 130 K=2,NUMK
C               IF (ZZ(1,K).EQ.ZCS) KS=K
C               IF (ZZ(1,K).EQ.ZCE) KE=K-1
C130          CONTINUE
cmod20160721(s)
c              CALL VF_PMGSTS(XWRK,XX,IS,IE,NXC,NUMI,MYIS,MYIE)
c              CALL VF_PMGSTS(YWRK,YY,JS,JE,NYC,NUMJ,MYJS,MYJE)
c              CALL VF_PMGSTS(ZWRK,ZZ,KS,KE,NZC,NUMK,2,NUMK-1) 
c      write(100+mgrank,*) 'vf_pmgset:pmgsts level=',level
              CALL VF_PMGSTS(XWRK,XXWK,IS,IE,NXC,NUMI0,2,NUMI0-1)
              CALL VF_PMGSTS(YWRK,YYWK,JS,JE,NYC,NUMJ0,2,NUMJ0-1)
              CALL VF_PMGSTS(ZWRK,ZZWK,KS,KE,NZC,NUMK,2,NUMK-1)
c             write(100+mgrank,*) 'is,ie,isize=',is,ie,IE-IS+1
c             write(100+mgrank,*) 'js,je,jsize=',js,je,JE-JS+1
c             write(100+mgrank,*) 'ks,ke,ksize=',ks,ke,KE-KS+1
cmod20160721(s)
C----------------------------------------------------------2011.04 end
              MGCPOS(1,MGCNUM)=IS
              MGCPOS(2,MGCNUM)=JS
              MGCPOS(3,MGCNUM)=KS
              MGCPOS(4,MGCNUM)=IE
              MGCPOS(5,MGCNUM)=JE
              MGCPOS(6,MGCNUM)=KE
              III(1)=IE-IS+1
              III(2)=JE-JS+1
              III(3)=KE-KS+1
              III(4)=0
              III(5)=0
              III(6)=0
              III(7)=0
              III(8)=0
              III(9)=0
              IF (MGCINF(4,MGCNUM).NE.0) III(4)=1
              IF (MGCINF(5,MGCNUM).NE.0) III(5)=1
              IF (MGCINF(7,MGCNUM).NE.0) III(7)=1
              IF (MGCINF(8,MGCNUM).NE.0) III(8)=1
cmod20160721(s)
c              IF (XMS.EQ.XCS) III(4)=1
c              IF (YMS.EQ.YCS) III(5)=1
              IF (XMS1.EQ.XCS) III(4)=1
              IF (YMS1.EQ.YCS) III(5)=1
              IF (ZMS.EQ.ZCS) III(6)=1
c              IF (XME.EQ.XCE) III(7)=1
c              IF (YME.EQ.YCE) III(8)=1
              IF (XME1.EQ.XCE) III(7)=1
              IF (YME1.EQ.YCE) III(8)=1
              IF (ZME.EQ.ZCE) III(9)=1
cmod20160721(e)
              MGCINF(4,MGCNUM)=III(4)
              MGCINF(5,MGCNUM)=III(5)
              MGCINF(6,MGCNUM)=III(6)
              MGCINF(7,MGCNUM)=III(7)
              MGCINF(8,MGCNUM)=III(8)
              MGCINF(9,MGCNUM)=III(9)
c             write(100+mgrank,*) 'iii(4:9)=',iii(4:9)
C             * 親側でセル数が２より小さい場合は両側境界のみ
              IF (III(1).LT.2) THEN
                IF (III(4).NE.1 .OR. III(7).NE.1)
     &              CALL VF_A2ERR('VF_PMGSET','INVALID VALUE(POS:X).')
              ENDIF
              IF (III(2).LT.2) THEN
                IF (III(5).NE.1 .OR. III(8).NE.1)
     &              CALL VF_A2ERR('VF_PMGSET','INVALID VALUE(POS:Y).')
              ENDIF
              IF (III(3).LT.2) THEN
                IF (III(6).NE.1 .OR. III(9).NE.1)
     &              CALL VF_A2ERR('VF_PMGSET','INVALID VALUE(POS:Z).')
              ENDIF
C             * 親境界に接するか２セル離れてなければならない
cmod20160721(s)
c              IF (III(4).NE.1 .AND. (IS-MYIS).LE.1)
              IF (III(4).NE.1 .AND. (IS-2   ).LE.1)
     &              CALL VF_A2ERR('VF_PMGSET','INVALID VALUE(POS:X-).')
c              IF (III(5).NE.1 .AND. (JS-MYJS).LE.1)
              IF (III(5).NE.1 .AND. (JS-2   ).LE.1)
     &              CALL VF_A2ERR('VF_PMGSET','INVALID VALUE(POS:Y-).')
              IF (III(6).NE.1 .AND. (KS-2   ).LE.1)
     &              CALL VF_A2ERR('VF_PMGSET','INVALID VALUE(POS:Z-).')
c              IF (III(7).NE.1 .AND. (MYIE-IE).LE.1)
              IF (III(7).NE.1 .AND. (NUMI0-IE).LE.2)
     &              CALL VF_A2ERR('VF_PMGSET','INVALID VALUE(POS:X+).')
c              IF (III(8).NE.1 .AND. (MYJE-JE).LE.1)
              IF (III(8).NE.1 .AND. (NUMJ0-JE).LE.2)
     &              CALL VF_A2ERR('VF_PMGSET','INVALID VALUE(POS:Y+).')
              IF (III(9).NE.1 .AND. (NUMK-KE).LE.2)
     &              CALL VF_A2ERR('VF_PMGSET','INVALID VALUE(POS:Z+).')
cmod20160721(e)
            ENDIF
c            write(100+mgrank,*) 'isw=',isw
            CALL VF_ZXMG_ISENDI(ISW,1,IC,IREQ,IERR)
            CALL VF_ZXMG_WAIT(IREQ,IERR)
            IF (ISW.NE.0) THEN
              CALL VF_ZXMG_ISENDI(III,9,IC,IREQ,IERR)
              CALL VF_ZXMG_WAIT(IREQ,IERR)
C----------------------------------------------------------2011.04 start
              CALL VF_ZXMG_ISENDD(XWRK,NXC+1,IC,IREQ,IERR)
              CALL VF_ZXMG_WAIT(IREQ,IERR)
              CALL VF_ZXMG_ISENDD(YWRK,NYC+1,IC,IREQ,IERR)
              CALL VF_ZXMG_WAIT(IREQ,IERR)
              CALL VF_ZXMG_ISENDD(ZWRK,NZC+1,IC,IREQ,IERR)
              CALL VF_ZXMG_WAIT(IREQ,IERR)

cmod20160726(s)
c              CALL VF_PMGGPT(GGV,GGX,GGY,GGZ,NF,IC,IS,JS,KS,IE,JE,KE)
cmod20160726(e)
C----------------------------------------------------------2011.04 end
            ENDIF
C----------------------------------------------------------2011.04 start
            DEALLOCATE(XWRK,YWRK,ZWRK)
C----------------------------------------------------------2011.04 end
          ENDIF
        ENDIF
 140  CONTINUE

c      write(100+mgrank,*) 'mgcnum = ',mgcnum
c      call VF_ZXMG_ABORT(IERR)
cadd20160726(s)
      CALL VF_ZXMP_BCASTI(ISW,1,0,IERR)
      CALL VF_ZXMP_BCASTI(MGCNUM,1,0,IERR)
      ISIZE=6*MAX(MGCNUM,1)
      CALL VF_ZXMP_BCASTI(MGCPOS,ISIZE,0,IERR)
      ISIZE=9*MAX(MGCNUM,1)
      CALL VF_ZXMP_BCASTI(MGCINF,ISIZE,0,IERR)
cadd20170512(s)
      ISIZE=MAX(MGCNUM,1)
      CALL VF_ZXMP_BCASTI(MGCRNK,ISIZE,0,IERR)
cadd20170512(e)
C
      IF( MGCNUM.GT.0 ) THEN
CDEBUGC ...... 子供の全体領域の範囲
CDEBUG         ISCA=HUGE(1)
CDEBUG         JSCA=HUGE(1)
CDEBUG         IECA=0
CDEBUG         JECA=0
CDEBUG         KSCA=2
CDEBUG         KECA=NUMK-1
CDEBUG         DO N=1,MGCNUM
CDEBUG            ISCA=MIN(ISCA,MGCPOS(1,N))
CDEBUG            JSCA=MIN(JSCA,MGCPOS(2,N))
CDEBUG            IECA=MAX(IECA,MGCPOS(4,N))
CDEBUG            JECA=MAX(JECA,MGCPOS(5,N))
CDEBUG         ENDDO
CDEBUGC        ローカルインデックスに変換
CDEBUG         ISC=ISCA-MYGIS+1
CDEBUG         JSC=JSCA-MYGJS+1
CDEBUG         KSC=KSCA
CDEBUG         IEC=IECA-MYGIS+1
CDEBUG         JEC=JECA-MYGJS+1
CDEBUG         KEC=KECA
CDEBUG         write(100+mgrank,*) 'isc=',isc
CDEBUG         write(100+mgrank,*) 'iec=',iec
CDEBUG         write(100+mgrank,*) 'jsc=',jsc
CDEBUG         write(100+mgrank,*) 'jec=',jec
CDEBUGC
CDEBUGC ...... 自分の領域の範囲(ローカルインデックス)
CDEBUG         ISG=MYIS
CDEBUG         IEG=MYIE
CDEBUG         JSG=MYJS
CDEBUG         JEG=MYJE
CDEBUG         KSG=2
CDEBUG         KEG=NUMK-1
CDEBUG         CALL SET_REGION(ISC,JSC,KSC,IEC,JEC,KEC,
CDEBUG     $                   ISG,JSG,KSG,IEG,JEG,KEG,MYRANK)
CDEBUGC
C ...... 自分の領域の範囲(グローバルインデックス)
         ISG=MYGIS+MYMIS
         JSG=MYGJS+MYMJS
         IEG=MYGIE-MYMIE
         JEG=MYGJE-MYMJE
         CALL MAKE_GGV_LIST(MGCPOS,MGCNUM,MYRANK,NPROCS,
     $                      ISG,JSG,IEG,JEG)
C
         if( myrank.eq.0 ) then
            NGWRKF = 0
            NGWRKU = 0
            NGWRKV = 0
            NGWRKW = 0
            DO N=1,MGCNUM
               IS=MGCPOS(1,N)
               JS=MGCPOS(2,N)
               KS=MGCPOS(3,N)
               IE=MGCPOS(4,N)
               JE=MGCPOS(5,N)
               KE=MGCPOS(6,N)
               NX = IE - IS + 1
               NY = JE - JS + 1
               NZ = KE - KS + 1
               NGWRKF = NGWRKF + (NX*NY - MAX((NX-2)*(NY-2),0))*NZ *2
               NGWRKU = NGWRKU + ((NX+1)*NY - MAX((NX-3)*(NY-2),0))*NZ
               NGWRKV = NGWRKV + (NX*(NY+1) - MAX((NX-2)*(NY-3),0))*NZ
               NGWRKW = NGWRKW + (NX*NY - MAX((NX-2)*(NY-2),0))*(NZ+1)
            ENDDO
         else
            NGWRKF=1
            NGWRKU=1
            NGWRKV=1
            NGWRKW=1
         endif
         ALLOCATE(GWRKF(NGWRKF),GWRKU(NGWRKU),
     $            GWRKV(NGWRKV),GWRKW(NGWRKW),STAT=IERR)
         call set_ggv_f(gwrkf,ngwrkf,ggv,nf,numi,numj,numk,
     $                  myrank,nprocs,MYGIS,MYGJS)
c         write(100+mgrank,*) 'gather u start'
c      call vf_zxmp_barri(ierr)
         call set_ggv_uvw(gwrku,ngwrku,ggx,numi,numj,numk,
     $                    1,myrank,nprocs,MYGIS,MYGJS)
c         write(100+mgrank,*) 'gather v start'
c      call vf_zxmp_barri(ierr)
         call set_ggv_uvw(gwrkv,ngwrkv,ggy,numi,numj,numk,
     $                    2,myrank,nprocs,MYGIS,MYGJS)
c         write(100+mgrank,*) 'gather w start'
c      call vf_zxmp_barri(ierr)
         call set_ggv_uvw(gwrkw,ngwrkw,ggz,numi,numj,numk,
     $                    3,myrank,nprocs,MYGIS,MYGJS)
         call delete_ggv_list(myrank,nprocs)
c     check
c         if( myrank==0 ) then
c            write(100+mgrank,*) 'min gwrkf=',minval(gwrkf)
c            write(100+mgrank,*) 'min gwrku=',minval(gwrku)
c            write(100+mgrank,*) 'min gwrkv=',minval(gwrkv)
c            write(100+mgrank,*) 'min gwrkw=',minval(gwrkw)
c         endif
      ENDIF
C
      M=0
      NNF=0
      NNU=0
      NNV=0
      NNW=0
C
      DO 150 IC=0,MGPROC-1
C       * 自分が子として親候補に情報を送り、結果をもらう
        IF (MGPARE(IC+1).NE.0) THEN
          IF (MGRANK.EQ.IC) THEN
C           * 親候補をさがして
            DO 160 IP=0,MGPROC-1
              IF (JSW(IP).NE.0) THEN
                 if( level.eq.1 )
     $           CALL VF_PMGGGT(GGV,GGX,GGY,GGZ,XPF,YPF,ZPF,NF,IP)
                 if( level.eq.2 )
     $           CALL VF_PMGGGT(GGV,GGX,GGY,GGZ,XPF,YPF,ZPF,nfwrk,IP)
              ENDIF
 160        CONTINUE
          ENDIF
C         * 自分が親の候補なら調べて、相手に結果を送る
          IF (MGPARE(IC+1).EQ.MGAREA(MGRANK+1)) THEN
            IF (ISW.NE.0.and.myrank.eq.0) THEN
               M=M+1
               IS=MGCPOS(1,M)
               JS=MGCPOS(2,M)
               KS=MGCPOS(3,M)
               IE=MGCPOS(4,M)
               JE=MGCPOS(5,M)
               KE=MGCPOS(6,M)
C
               CALL VF_PMGGPT(GWRKF,GWRKU,GWRKV,GWRKW,NNF,NNU,NNV,NNW,
     $               IC,IS,JS,KS,IE,JE,KE,NGWRKF,NGWRKU,NGWRKV,NGWRKW)
            ENDIF
          ENDIF
        ENDIF
 150  CONTINUE
c
      IF( MGCNUM.GT.0 ) THEN
         DEALLOCATE(GWRKF,GWRKU,GWRKV,GWRKW,STAT=IERR)
      ENDIF
cadd20160726(e)

CD    -- 子の領域を障害物セルにする(Z方向は完全に抜く) --
cmod20160803(s)
      if(level.eq.1)then
cmod20160803(e)
      DO 230 IC=1,MGCNUM
        IS=MGCPOS(1,IC)
        JS=MGCPOS(2,IC)
        IE=MGCPOS(4,IC)
        JE=MGCPOS(5,IC)
cadd20160729(s)
c ..... ローカルインデックスに変換
        IS=IS-MYGIS+1
        JS=JS-MYGJS+1
        IE=IE-MYGIS+1
        JE=JE-MYGJS+1
        ISG=MYIS
        IEG=MYIE
        JSG=MYJS
        JEG=MYJE
cadd20160729(e)
C
c ..... 接続する場合は、1層分内側から障害物にする
        IF (MGCINF(4,IC).EQ.0) IS=IS+1
        IF (MGCINF(5,IC).EQ.0) JS=JS+1
        IF (MGCINF(7,IC).EQ.0) IE=IE-1
        IF (MGCINF(8,IC).EQ.0) JE=JE-1
c        write(mgrank+100,*) 'nf set: is,ie=',is,ie
c        write(mgrank+100,*) 'nf set: js,je=',js,je
        DO 220 K=1,NUMK
          DO 210 J=JS,JE
            DO 200 I=IS,IE
cmod20160729(s)
              IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $            JSG.LE.J.AND.J.LE.JEG ) THEN
              NF(I,J,K)=-1
              ENDIF
cmod20160729(e)
 200        CONTINUE
 210      CONTINUE
 220    CONTINUE
 230  CONTINUE
cmod20160803(s)
      endif
c
      deallocate(nfwrk)
cmod20160803(e)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
cmod20160803(s)
      if(level.eq.2)then
         if( mgcnum.gt.0 ) then
            ISG=MYGIS+MYMIS
            JSG=MYGJS+MYMJS
            IEG=MYGIE-MYMIE
            JEG=MYGJE-MYMJE
C
            MGNV=9
            call make_p2c_list(MGCPOS,MGCINF,MGCNUM,MYRANK,NPROCS,
     $                         ISG,JSG,IEG,JEG,MGNV)
c
            MGNV=4
            call make_c2p_list(MGCPOS,MGCINF,MGCNUM,MYRANK,NPROCS,
     $                         ISG,JSG,IEG,JEG,MGNV)
         endif
      endif
cmod20160803(e)
c      write(100+mgrank,*) 'exit vf_pmgset. level=',level
cmod20170426(s)
c      write(100+mgrank,*) 'mgcnum=',mgprnk
      IF( MGPRNK.GE.0 ) THEN
         IF(LEVEL.EQ.1)THEN
            CALL VF_P3SRI2(NF,IBUF,0)
         ELSEIF(LEVEL.EQ.2)THEN
            CALL VF_P3SRD2(GGV,DBUF,0)
            CALL VF_P3SRD2(GGX,DBUF,1)
            CALL VF_P3SRD2(GGY,DBUF,2)
            CALL VF_P3SRD2(GGZ,DBUF,3)
         ENDIF
      ENDIF
cmod20170426(e)
CDEBUG      write(100+mgrank,*) 
CDEBUG      write(100+mgrank,*)'MGPROC            =',MGPROC            
CDEBUG      write(100+mgrank,*)'MGRANK            =',MGRANK            
CDEBUG      write(100+mgrank,*)'MGCOMM            =',MGCOMM            
CDEBUG      write(100+mgrank,*)'MGARAN            =',MGARAN            
CDEBUG      write(100+mgrank,*)'MGNAME(1:MGPROC)  =',MGNAME(1:MGPROC)  
CDEBUG      write(100+mgrank,*)'MGNLEN(1:MGPROC)  =',MGNLEN(1:MGPROC)  
CDEBUG      write(100+mgrank,*)'MGNPIN(1:MGPROC)  =',MGNPIN(1:MGPROC)  
CDEBUG      write(100+mgrank,*)'MGPARE(1:MGPROC)  =',MGPARE(1:MGPROC)  
CDEBUG      write(100+mgrank,*)'MGAREA(1:MGPROC)  =',MGAREA(1:MGPROC)  
CDEBUG      write(100+mgrank,*)'MGPRNK            =',MGPRNK            
CDEBUG      write(100+mgrank,*)'MGPINF(:)         =',MGPINF(:)         
CDEBUG      write(100+mgrank,*)'MGCNUM            =',MGCNUM            
CDEBUG      write(100+mgrank,*)'MGCRNK(1:MGPROC)  =',MGCRNK(1:MGPROC)  
CDEBUG      write(100+mgrank,*)'MGCINF(:,1:MGPROC)=',MGCINF(:,1:MGPROC)
CDEBUG      write(100+mgrank,*)'MGCPOS(:,1:MGPROC)=',MGCPOS(:,1:MGPROC)
CDEBUG      write(100+mgrank,*)'NPROCS            =',NPROCS            
CDEBUG      write(100+mgrank,*)'NUMNPI            =',NUMNPI            
CDEBUG      write(100+mgrank,*)'NUMNPJ            =',NUMNPJ            
CDEBUG      write(100+mgrank,*)'MYRANK            =',MYRANK            
CDEBUG      write(100+mgrank,*)'MYRI              =',MYRI              
CDEBUG      write(100+mgrank,*)'MYRJ              =',MYRJ              
CDEBUG      write(100+mgrank,*)'NUMI0             =',NUMI0             
CDEBUG      write(100+mgrank,*)'NUMJ0             =',NUMJ0             
CDEBUG      write(100+mgrank,*)'MYIS              =',MYIS              
CDEBUG      write(100+mgrank,*)'MYIE              =',MYIE              
CDEBUG      write(100+mgrank,*)'MYJS              =',MYJS              
CDEBUG      write(100+mgrank,*)'MYJE              =',MYJE              
CDEBUG      write(100+mgrank,*)'MYMIS             =',MYMIS             
CDEBUG      write(100+mgrank,*)'MYMIE             =',MYMIE             
CDEBUG      write(100+mgrank,*)'MYMJS             =',MYMJS             
CDEBUG      write(100+mgrank,*)'MYMJE             =',MYMJE             
CDEBUG      write(100+mgrank,*)'MYGIS             =',MYGIS             
CDEBUG      write(100+mgrank,*)'MYGIE             =',MYGIE             
CDEBUG      write(100+mgrank,*)'MYGJS             =',MYGJS             
CDEBUG      write(100+mgrank,*)'MYGJE             =',MYGJE             
CDEBUG      write(100+mgrank,*)'NUMBUF            =',NUMBUF            
CDEBUG      write(100+mgrank,*)'IPROCS(0:MAXNPI)  =',IPROCS(0:MAXNPI)  
CDEBUG      write(100+mgrank,*)'JPROCS(0:MAXNPJ)  =',JPROCS(0:MAXNPJ)  
      RETURN
      END
