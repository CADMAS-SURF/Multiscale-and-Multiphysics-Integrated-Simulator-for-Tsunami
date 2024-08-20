      SUBROUTINE VF_OGBCNM(INDB,NBX,NBY,NBZ)

CD=== 概要 ===========================================================

CDT   VF_OGBCNM:図化用の境界の数を数える

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    INDB (MAXB1,NUMB) : IN  : I*4 : 境界面のインデックス
CD    NBX               : OUT : I*4 : x方向境界面の数
CD    NBY               : OUT : I*4 : y方向境界面の数
CD    NBZ               : OUT : I*4 : z方向境界面の数
      DIMENSION INDB(MAXB1,NUMB)

C==== 実行 ===========================================================

CD    -- 初期設定 --
      I1=MAX(IGRARA(1)-(MYGIS-1),MYIS)
      J1=MAX(IGRARA(2)-(MYGJS-1),MYJS)
      K1=IGRARA(3)
      I2=MIN(IGRARA(4)-(MYGIS-1),MYIE)
      J2=MIN(IGRARA(5)-(MYGJS-1),MYJE)
      K2=IGRARA(6)
      IX=I2+1
      JY=J2+1
      KZ=K2+1
      NBX=0
      NBY=0
      NBZ=0

CD    -- 数を数える --
      DO 100 L=1,NUMB
        IJK=INDB(1,L)
        NS =INDB(2,L)
        K  =(IJK-1)/(NUMI*NUMJ)+1
        IJK=IJK-NUMI*NUMJ*(K-1)
        J  =(IJK-1)/NUMI+1
        I  =IJK-NUMI*(J-1)
        IF     (NS.EQ.1 .OR. NS.EQ.2) THEN
          IF (I1.NE.I2) THEN
            IF (I1.LE.I .AND. I.LE.IX) THEN
              IF (J1.LE.J .AND. J.LE.J2) THEN
                IF (K1.LE.K .AND. K.LE.K2) THEN
                  NBX=NBX+1
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ELSEIF (NS.EQ.3 .OR. NS.EQ.4) THEN
          IF (J1.NE.J2) THEN
            IF (I1.LE.I .AND. I.LE.I2) THEN
              IF (J1.LE.J .AND. J.LE.JY) THEN
                IF (K1.LE.K .AND. K.LE.K2) THEN
                  NBY=NBY+1
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ELSE
          IF (K1.NE.K2) THEN
            IF (I1.LE.I .AND. I.LE.I2) THEN
              IF (J1.LE.J .AND. J.LE.J2) THEN
                IF (K1.LE.K .AND. K.LE.KZ) THEN
                  NBZ=NBZ+1
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
 100  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
