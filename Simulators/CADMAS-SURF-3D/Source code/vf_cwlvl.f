      SUBROUTINE VF_CWLVL(ZZ,FF,VAL,NF,IIN,JIN)

CD=== 概要 ===========================================================

CDT   VF_CWLVL:水位変動を計算する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSR.h'

CD    -- 引数 --
CD    ZZ(MAXG1,NUMK) : IN  : R*8 : z方向格子座標等
CD    FF(@FOR-3D@)   : IN  : R*8 : VOF関数F
CD    VAL            : OUT : R*8 : 水面の高さ
CD    NF(@FOR-3D@)   : IN  : I*4 : セルの状態を示すインデックス
CD    IIN            : IN  : I*4 : x方向セル番号
CD    JIN            : IN  : I*4 : y方向セル番号
      DIMENSION ZZ(MAXG1,NUMK)
      DIMENSION FF(NUMI,NUMJ,NUMK)
      DIMENSION NF(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 並列時のシフト分等 --
      IP=IIN-(MYGIS-1)
      JP=JIN-(MYGJS-1)

      IF (MYIS.LE.IP .AND. IP.LE.MYIE .AND.
     &    MYJS.LE.JP .AND. JP.LE.MYJE      ) THEN

CD      -- VOF関数Fをz方向に加える --
        SF=0.0D0
        DO 100 K=2,NUMK-1
          IF (NF(IP,JP,K).EQ.-1) THEN
            SF=SF+ZZ(2,K)*1.0D0
          ELSE
            SF=SF+ZZ(2,K)*FF(IP,JP,K)
          ENDIF
 100    CONTINUE

CD      -- 初期水面の位置を基準とする --
        VAL=SF-(WVLVL-ZZ(1,2))

      ELSE

        VAL=0.0D0

      ENDIF

      W=VAL
      CALL VF_P1SUMD(W,VAL)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
