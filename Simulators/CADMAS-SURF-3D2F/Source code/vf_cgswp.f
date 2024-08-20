      SUBROUTINE VF_CGSWP(ISW,BCU ,BCV ,BCW ,BCP ,BCF ,BCVI ,
     &                        BCU0,BCV0,BCW0,BCP0,BCF0,BCVI0)

CD=== 概要 ===========================================================

CDT   VF_CGSWP:移動障害物処理時に、境界面データの保存と複写を行う

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_AFILEI.h'

CD    -- 引数 --
CD    ISW               : IN  : I*4 : 動作フラグ
CD    BCU(NUMB,3)       : I/O : R*8 : x方向流速の境界値
CD    BCV(NUMB,3)       : I/O : R*8 : y方向流速の境界値
CD    BCW(NUMB,3)       : I/O : R*8 : z方向流速の境界値
CD    BCP(NUMB,3)       : I/O : R*8 : 圧力の境界値
CD    BCF(NUMB)         : I/O : R*8 : VOF関数Fの境界値
CD    BCVI(NUMB)        : I/O : R*8 : 流速の境界条件(壁面の粗さ)
CD    BCU0(NUMB,3)      : I/O : R*8 : x方向流速の境界値
CD    BCV0(NUMB,3)      : I/O : R*8 : y方向流速の境界値
CD    BCW0(NUMB,3)      : I/O : R*8 : z方向流速の境界値
CD    BCP0(NUMB,3)      : I/O : R*8 : 圧力の境界値
CD    BCF0(NUMB)        : I/O : R*8 : VOF関数Fの境界値
CD    BCVI0(NUMB)       : I/O : R*8 : 流速の境界条件(壁面の粗さ)
      DIMENSION BCU (NUMB ,3),BCV (NUMB ,3),BCW  (NUMB ,3)
      DIMENSION BCP (NUMB ,3),BCF (NUMB   ),BCVI (NUMB )
      DIMENSION BCU0(NUMB0,3),BCV0(NUMB0,3),BCW0 (NUMB0,3)
      DIMENSION BCP0(NUMB0,3),BCF0(NUMB0  ),BCVI0(NUMB0)

C==== 実行 ===========================================================

      IF (ISW.EQ.1) THEN
CD    -- 修正する前の境界面の各種データを保存 --
        DO 100 IB=1,NUMB0
          BCU0(IB,1)=BCU(IB,1)
          BCU0(IB,2)=BCU(IB,2)
          BCU0(IB,3)=BCU(IB,3)
          BCV0(IB,1)=BCV(IB,1)
          BCV0(IB,2)=BCV(IB,2)
          BCV0(IB,3)=BCV(IB,3)
          BCW0(IB,1)=BCW(IB,1)
          BCW0(IB,2)=BCW(IB,2)
          BCW0(IB,3)=BCW(IB,3)
          BCP0(IB,1)=BCP(IB,1)
          BCP0(IB,2)=BCP(IB,2)
          BCP0(IB,3)=BCP(IB,3)
          BCF0(IB)  =BCF(IB)
          BCVI0(IB) =BCVI(IB)
 100    CONTINUE

      ELSE
CD    -- 修正する前の境界面の各種データを保存 --
        DO 200 IB=1,NUMB0
          BCU(IB,1)=BCU0(IB,1)
          BCU(IB,2)=BCU0(IB,2)
          BCU(IB,3)=BCU0(IB,3)
          BCV(IB,1)=BCV0(IB,1)
          BCV(IB,2)=BCV0(IB,2)
          BCV(IB,3)=BCV0(IB,3)
          BCW(IB,1)=BCW0(IB,1)
          BCW(IB,2)=BCW0(IB,2)
          BCW(IB,3)=BCW0(IB,3)
          BCP(IB,1)=BCP0(IB,1)
          BCP(IB,2)=BCP0(IB,2)
          BCP(IB,3)=BCP0(IB,3)
          BCF(IB)  =BCF0(IB)
          BCVI(IB) =BCVI0(IB)
 200    CONTINUE
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
