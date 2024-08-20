      SUBROUTINE VF_CSET2F(BCU,BCV,BCW,BCP,BCK,BCE,INDB,INDBK,INDBE)

CD=== 概要 ===========================================================

CDT   VF_CSETBC:気相と液相の境界条件の整合性を調整

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    BCU(NUMB,3)        : I/O : R*8 : x方向流速の境界値
CD    BCV(NUMB,3)        : I/O : R*8 : y方向流速の境界値
CD    BCW(NUMB,3)        : I/O : R*8 : z方向流速の境界値
CD    BCK(NUMB,3)        : I/O : R*8 : 乱流エネルギの境界値
CD    BCE(NUMB,3)        : I/O : R*8 : 乱流エネルギ散逸の境界値
CD    INDB(MAXB1,NUMB)   : I/O : I*4 : 境界面のインデックス
CD    INDBK(MAXBK1,NUMB) : I/O : I*4 : 乱流エネルギの境界条件
CD    INDBE(MAXBE1,NUMB) : I/O : I*4 : 乱流エネルギ散逸の境界条件
      DIMENSION BCU(NUMB,3),BCV(NUMB,3),BCW(NUMB,3),BCP(NUMB,3)
      DIMENSION BCK(NUMB,3),BCE(NUMB,3)
      DIMENSION INDB(MAXB1,NUMB),INDBK(MAXBK1,NUMB),INDBE(MAXBE1,NUMB)

C==== 実行 ===========================================================

      DO 100 L=1,NUMB

C     * 流速の境界条件 *
        IBVF=INDB(3,L)
        IBVG=INDB(5,L)
        IF (IBVG.EQ.0) THEN
          IF (IBVF.NE.5) THEN
            INDB(5,L)=IBVF
            BCU(L,3)=BCU(L,2)
            BCV(L,3)=BCV(L,2)
            BCW(L,3)=BCW(L,2)
            BCP(L,3)=BCP(L,2)
          ELSE
            INDB(5,L)=1
            BCU(L,3)=0.0D0
            BCV(L,3)=0.0D0
            BCW(L,3)=0.0D0
            BCP(L,3)=0.0D0
          ENDIF
        ENDIF

        IF (LEQK.NE.0) THEN
CD        * 乱流エネルギの境界条件 *
          IBKF=INDBK(1,L)
          IBKG=INDBK(2,L)
          IF (IBKG.EQ.0) THEN
            INDBK(2,L)=IBKF
            BCK(L,3)=BCK(L,2)
          ENDIF

CD        * 乱流エネルギ散逸の境界条件 *
          IBEF=INDBE(1,L)
          IBEG=INDBE(2,L)
          IF (IBEG.EQ.0) THEN
            INDBE(2,L)=IBEF
            BCE(L,3)=BCE(L,2)
          ENDIF
        ENDIF

 100  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
