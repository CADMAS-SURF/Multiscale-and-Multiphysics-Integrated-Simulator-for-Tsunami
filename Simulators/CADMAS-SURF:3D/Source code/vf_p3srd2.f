      SUBROUTINE VF_P3SRD2(VAL,DBUF,ISW)

CD=== 概要 ===========================================================

CDT   VF_P3SRD2:3次元データの2層分のデータを送信・受信する/実数

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
      DIMENSION VAL(NUMI,NUMJ,NUMK)
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
      IF     (ISW.EQ.0) THEN
      ELSEIF (ISW.EQ.1) THEN
        IF (MYMIE.EQ.1) IB=IB+1
      ELSEIF (ISW.EQ.2) THEN
        IF (MYMJE.EQ.1) JB=JB+1
      ELSEIF (ISW.EQ.3) THEN
        KB=KB+1
      ELSE
        CALL VF_A2ERR('VF_P3SRD2','P.G ERROR.')
      ENDIF
      IF (MYMIS.GE.2) IA=IA-2
      IF (MYMIE.GE.2) IB=IB+2

C     -- x方向を送信・受信する --
      IF (MYMIS.GE.2) THEN
        I1=MYIS
        I2=MYIS+1
        L1=0*NUMBUF
        L3=4*NUMBUF
        L=L1
        DO 110 K=KA,KB
          DO 100 J=JA,JB
            L=L+1
            DBUF(L)=VAL(I1,J,K)
            L=L+1
            DBUF(L)=VAL(I2,J,K)
 100      CONTINUE
 110    CONTINUE
        N1=L-L1
        CALL VF_ZXMP_ISENDD(DBUF(L1+1),N1,M1,IREQ1,IERR)
        CALL VF_ZXMP_IRECVD(DBUF(L3+1),N1,M1,IREQ3,IERR)
      ENDIF
      IF (MYMIE.GE.2) THEN
        I1=MYIE-1
        I2=MYIE
        L2=2*NUMBUF
        L4=6*NUMBUF
        L=L2
        DO 210 K=KA,KB
          DO 200 J=JA,JB
            L=L+1
            DBUF(L)=VAL(I1,J,K)
            L=L+1
            DBUF(L)=VAL(I2,J,K)
 200      CONTINUE
 210    CONTINUE
        N2=L-L2
        CALL VF_ZXMP_ISENDD(DBUF(L2+1),N2,M2,IREQ2,IERR)
        CALL VF_ZXMP_IRECVD(DBUF(L4+1),N2,M2,IREQ4,IERR)
      ENDIF
      IF (MYMIS.GE.2) THEN
        CALL VF_ZXMP_WAIT(IREQ3,IERR)
        I1=1
        I2=2
        L3=4*NUMBUF
        L=L3
        DO 310 K=KA,KB
          DO 300 J=JA,JB
            L=L+1
            VAL(I1,J,K)=DBUF(L)
            L=L+1
            VAL(I2,J,K)=DBUF(L)
 300      CONTINUE
 310    CONTINUE
      ENDIF
      IF (MYMIE.GE.2) THEN
        CALL VF_ZXMP_WAIT(IREQ4,IERR)
        I1=NUMI-1
        I2=NUMI
        L4=6*NUMBUF
        L=L4
        DO 410 K=KA,KB
          DO 400 J=JA,JB
            L=L+1
            VAL(I1,J,K)=DBUF(L)
            L=L+1
            VAL(I2,J,K)=DBUF(L)
 400      CONTINUE
 410    CONTINUE
      ENDIF
      IF (MYMIS.GE.2) CALL VF_ZXMP_WAIT(IREQ1,IERR)
      IF (MYMIE.GE.2) CALL VF_ZXMP_WAIT(IREQ2,IERR)

C     -- y方向を送信・受信する --
      IF (MYMJS.GE.2) THEN
        J1=MYJS
        J2=MYJS+1
        L1=0*NUMBUF
        L3=4*NUMBUF
        L=L1
        DO 510 K=KA,KB
          DO 500 I=IA,IB
            L=L+1
            DBUF(L)=VAL(I,J1,K)
            L=L+1
            DBUF(L)=VAL(I,J2,K)
 500      CONTINUE
 510    CONTINUE
        N1=L-L1
        CALL VF_ZXMP_ISENDD(DBUF(L1+1),N1,M3,IREQ1,IERR)
        CALL VF_ZXMP_IRECVD(DBUF(L3+1),N1,M3,IREQ3,IERR)
      ENDIF
      IF (MYMJE.GE.2) THEN
        J1=MYJE-1
        J2=MYJE
        L2=2*NUMBUF
        L4=6*NUMBUF
        L=L2
        DO 610 K=KA,KB
          DO 600 I=IA,IB
            L=L+1
            DBUF(L)=VAL(I,J1,K)
            L=L+1
            DBUF(L)=VAL(I,J2,K)
 600      CONTINUE
 610    CONTINUE
        N2=L-L2
        CALL VF_ZXMP_ISENDD(DBUF(L2+1),N2,M4,IREQ2,IERR)
        CALL VF_ZXMP_IRECVD(DBUF(L4+1),N2,M4,IREQ4,IERR)
      ENDIF
      IF (MYMJS.GE.2) THEN
        CALL VF_ZXMP_WAIT(IREQ3,IERR)
        J1=1
        J2=2
        L3=4*NUMBUF
        L=L3
        DO 710 K=KA,KB
          DO 700 I=IA,IB
            L=L+1
            VAL(I,J1,K)=DBUF(L)
            L=L+1
            VAL(I,J2,K)=DBUF(L)
 700      CONTINUE
 710    CONTINUE
      ENDIF
      IF (MYMJE.GE.2) THEN
        CALL VF_ZXMP_WAIT(IREQ4,IERR)
        J1=NUMJ-1
        J2=NUMJ
        L4=6*NUMBUF
        L=L4
        DO 810 K=KA,KB
          DO 800 I=IA,IB
            L=L+1
            VAL(I,J1,K)=DBUF(L)
            L=L+1
            VAL(I,J2,K)=DBUF(L)
 800      CONTINUE
 810    CONTINUE
      ENDIF
      IF (MYMJS.GE.2) CALL VF_ZXMP_WAIT(IREQ1,IERR)
      IF (MYMJE.GE.2) CALL VF_ZXMP_WAIT(IREQ2,IERR)

C     -- 実行文の終了 --
 9000 CONTINUE
      CALL VF_A2CPUT(0,ICPUEN,KCP9PL)
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
