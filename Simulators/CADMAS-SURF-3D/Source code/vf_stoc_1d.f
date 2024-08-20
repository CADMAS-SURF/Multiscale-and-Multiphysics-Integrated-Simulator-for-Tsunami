      SUBROUTINE VF_STOC_1D(VAL,DBUF,ISW)

CD=== 概要 ===========================================================

CDT   VF_STOC_1D:1次元データの1層分のデータを送信・受信する/実数

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACPUTR.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    VAL(@FOR-3D@)       : I/O : R*8 : 送信・受信するデータ
CD    DBUF(NUMBUF*MAXBUF) : OUT : R*8 : 並列用のバッファ
CD    ISW                 : IN  : I*4 : 定義位置(C:=0X:=1,Y:=2,Z:=3)
C VAL(1,*,*) : UまたはV
C VAL(2,*,*) : W
C VAL(*,*,1) : 南西側
C VAL(*,*,2) : 南東側
C VAL(*,*,3) : 北西側
C VAL(*,*,4) : 北東側
      DIMENSION VAL(2,NUMK,4)
      DIMENSION DBUF(NUMBUF*MAXBUF)

C==== 実行 ===========================================================

      CALL VF_A2CPUT(0,ICPUST,KCP9PL)
      IF (NPROCS.EQ.1) GOTO 9000

C     -- 送信・受信対象のランク --
      M1=MYRANK-1
      M2=MYRANK+1
      M3=MYRANK-NUMNPI
      M4=MYRANK+NUMNPI

C     -- 送信・受信面の範囲 --
      IA=MYIS
      IB=MYIE
      JA=MYJS
      JB=MYJE
      KA=2
      KB=NUMK-1
      IF (ISW.NE.1.AND.ISW.NE.2) THEN
         CALL VF_A2ERR('VF_P3SRD2','P.G ERROR.')
      ENDIF

      IF( ISW.EQ.2 ) THEN
C     -- x方向を送信・受信する --
      IF (MYMIS.GE.2) THEN
        L1=0
        L3=8*(KB-KA+1)
        L=L1
        DO 110 K=KA,KB
           L=L+1
           DBUF(L)=VAL(1,K,1)
           L=L+1
           DBUF(L)=VAL(2,K,1)
           L=L+1
           DBUF(L)=VAL(1,K,3)
           L=L+1
           DBUF(L)=VAL(2,K,3)
  110   CONTINUE
        N1=L-L1
        CALL VF_ZXMP_ISENDD(DBUF(L1+1),N1,M1,IREQ1,IERR)
        CALL VF_ZXMP_IRECVD(DBUF(L3+1),N1,M1,IREQ3,IERR)
      ENDIF

      IF (MYMIE.GE.2) THEN
        L2=4*(KB-KA+1)
        L4=12*(KB-KA+1)
        L=L2
        DO 210 K=KA,KB
           L=L+1
           DBUF(L)=VAL(1,K,2)
           L=L+1
           DBUF(L)=VAL(2,K,2)
           L=L+1
           DBUF(L)=VAL(1,K,4)
           L=L+1
           DBUF(L)=VAL(2,K,4)
  210   CONTINUE
        N2=L-L2
        CALL VF_ZXMP_ISENDD(DBUF(L2+1),N2,M2,IREQ2,IERR)
        CALL VF_ZXMP_IRECVD(DBUF(L4+1),N2,M2,IREQ4,IERR)
      ENDIF

      IF (MYMIS.GE.2) THEN
        CALL VF_ZXMG_WAIT(IREQ3,IERR)
        L3=8*(KB-KA+1)
        L=L3
        DO 310 K=KA,KB
           L=L+1
           VAL(1,K,1)=DBUF(L)
           L=L+1
           VAL(2,K,1)=DBUF(L)
           L=L+1
           VAL(1,K,3)=DBUF(L)
           L=L+1
           VAL(2,K,3)=DBUF(L)
  310   CONTINUE
      ENDIF

      IF (MYMIE.GE.2) THEN
        CALL VF_ZXMG_WAIT(IREQ4,IERR)
        L4=12*(KB-KA+1)
        L=L4
        DO 410 K=KA,KB
           L=L+1
           VAL(1,K,2)=DBUF(L)
           L=L+1
           VAL(2,K,2)=DBUF(L)
           L=L+1
           VAL(1,K,4)=DBUF(L)
           L=L+1
           VAL(2,K,4)=DBUF(L)
  410   CONTINUE
      ENDIF

      IF (MYMIS.GE.2) CALL VF_ZXMG_WAIT(IREQ1,IERR)
      IF (MYMIE.GE.2) CALL VF_ZXMG_WAIT(IREQ2,IERR)

      ENDIF
      IF( ISW.EQ.1 ) THEN

C     -- y方向を送信・受信する --
      IF (MYMJS.GE.2) THEN
        L1=0
        L3=8*(KB-KA+1)
        L=L1
        DO 510 K=KA,KB
           L=L+1
           DBUF(L)=VAL(1,K,1)
           L=L+1
           DBUF(L)=VAL(2,K,1)
           L=L+1
           DBUF(L)=VAL(1,K,2)
           L=L+1
           DBUF(L)=VAL(2,K,2)
  510   CONTINUE
        N1=L-L1
        CALL VF_ZXMP_ISENDD(DBUF(L1+1),N1,M3,IREQ1,IERR)
        CALL VF_ZXMP_IRECVD(DBUF(L3+1),N1,M3,IREQ3,IERR)
      ENDIF

      IF (MYMJE.GE.2) THEN
        L2=4*(KB-KA+1)
        L4=12*(KB-KA+1)
        L=L2
        DO 610 K=KA,KB
           L=L+1
           DBUF(L)=VAL(1,K,3)
           L=L+1
           DBUF(L)=VAL(2,K,3)
           L=L+1
           DBUF(L)=VAL(1,K,4)
           L=L+1
           DBUF(L)=VAL(2,K,4)
  610   CONTINUE
        N2=L-L2
        CALL VF_ZXMP_ISENDD(DBUF(L2+1),N2,M4,IREQ2,IERR)
        CALL VF_ZXMP_IRECVD(DBUF(L4+1),N2,M4,IREQ4,IERR)
      ENDIF

      IF (MYMJS.GE.2) THEN
        CALL VF_ZXMG_WAIT(IREQ3,IERR)
        L3=8*(KB-KA+1)
        L=L3
        DO 710 K=KA,KB
           L=L+1
           VAL(1,K,1)=DBUF(L)
           L=L+1
           VAL(2,K,1)=DBUF(L)
           L=L+1
           VAL(1,K,2)=DBUF(L)
           L=L+1
           VAL(2,K,2)=DBUF(L)
  710   CONTINUE
      ENDIF

      IF (MYMJE.GE.2) THEN
        CALL VF_ZXMG_WAIT(IREQ4,IERR)
        L4=12*(KB-KA+1)
        L=L4
        DO 810 K=KA,KB
           L=L+1
           VAL(1,K,3)=DBUF(L)
           L=L+1
           VAL(2,K,3)=DBUF(L)
           L=L+1
           VAL(1,K,4)=DBUF(L)
           L=L+1
           VAL(2,K,4)=DBUF(L)
  810   CONTINUE
      ENDIF

      IF (MYMJS.GE.2) CALL VF_ZXMG_WAIT(IREQ1,IERR)
      IF (MYMJE.GE.2) CALL VF_ZXMG_WAIT(IREQ2,IERR)

      ENDIF
C     -- 実行文の終了 --
 9000 CONTINUE
      CALL VF_A2CPUT(0,ICPUEN,KCP9PL)
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
