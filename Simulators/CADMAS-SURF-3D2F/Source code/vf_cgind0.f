      SUBROUTINE VF_CGIND0(INDX,INDY,INDZ,INDB,INDX0,INDY0,INDZ0,INDB0)

CD=== 概要 ===========================================================

CDT   VF_CGIND0:空隙率を基に修正する前の面状態インデックスを保存する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_AFILEI.h'

CD    -- 引数 --
CD    INDX(@FOR-3D@)    : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)    : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)    : IN  : I*4 : z面の状態を示すインデックス
CD    INDB(MAXB1,NUMB)  : IN  : I*4 : 境界面のインデックス
CD    INDX0(@FOR-3D@)   : OUT : I*4 : x面の状態を示すインデックス(修正前)
CD    INDY0(@FOR-3D@)   : OUT : I*4 : y面の状態を示すインデックス(修正前)
CD    INDZ0(@FOR-3D@)   : OUT : I*4 : z面の状態を示すインデックス(修正前)
CD    INDB0(MAXB1,NUMB0): OUT : I*4 : 境界面のインデックス(修正前)
      DIMENSION INDX (NUMI,NUMJ,NUMK),INDY (NUMI,NUMJ,NUMK)
      DIMENSION INDZ (NUMI,NUMJ,NUMK),INDB (MAXB1,NUMB)
      DIMENSION INDX0(NUMI,NUMJ,NUMK),INDY0(NUMI,NUMJ,NUMK)
      DIMENSION INDZ0(NUMI,NUMJ,NUMK),INDB0(MAXB1,NUMB0)

C==== 実行 ===========================================================

CD    -- 修正する前の面の状態を示すインデクスを保存 --
      DO 120 K=1,NUMK
        DO 110 J=1,NUMJ
          DO 100 I=1,NUMI
            INDX0(I,J,K)=INDX(I,J,K)
            INDY0(I,J,K)=INDY(I,J,K)
            INDZ0(I,J,K)=INDZ(I,J,K)
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

CD    -- 修正する前の境界面のインデックスを保存 --
      DO 210 IB=1,NUMB0
        DO 200 I=1,MAXB1
          INDB0(I,IB)=INDB(I,IB)
 200    CONTINUE
 210  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
