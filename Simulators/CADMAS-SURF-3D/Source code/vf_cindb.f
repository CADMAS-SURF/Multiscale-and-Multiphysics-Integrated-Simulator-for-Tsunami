      SUBROUTINE VF_CINDB(NF,INDX,INDY,INDZ,INDB)

CD=== 概要 ===========================================================

CDT   VF_CINDB:境界面のインデックスINDBを設定する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'

CD    -- 引数 --
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : IN  : I*4 : z面の状態を示すインデックス
CD    INDB(MAXB1,NUMB) : OUT : I*4 : 境界面のインデックス
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDB(MAXB1,NUMB)

C==== 実行 ===========================================================

CD    -- x面に関する設定 --
      DO 130 K=2,NUMK-1
        DO 120 J=2,NUMJ-1
          DO 110 I=2,NUMI
            N2=NF  (I,J,K)
            NB=INDX(I,J,K)
            IF (NB.GE.1) THEN
              INDB(1,NB)=I+NUMI*(J-1)+NUMI*NUMJ*(K-1)
              IF (N2.NE.-1) THEN
                INDB(2,NB)=1
              ELSE
                INDB(2,NB)=2
              ENDIF
              DO 100 L=3,MAXB1
                INDB(L,NB)=0
 100          CONTINUE
            ENDIF
 110      CONTINUE
 120    CONTINUE
 130  CONTINUE

CD    -- y面に関する設定 --
      DO 230 K=2,NUMK-1
        DO 220 J=2,NUMJ
          DO 210 I=2,NUMI-1
            N2=NF  (I,J,K)
            NB=INDY(I,J,K)
            IF (NB.GE.1) THEN
              INDB(1,NB)=I+NUMI*(J-1)+NUMI*NUMJ*(K-1)
              IF (N2.NE.-1) THEN
                INDB(2,NB)=3
              ELSE
                INDB(2,NB)=4
              ENDIF
              DO 200 L=3,MAXB1
                INDB(L,NB)=0
 200          CONTINUE
            ENDIF
 210      CONTINUE
 220    CONTINUE
 230  CONTINUE

CD    -- z面に関する設定 --
      DO 330 K=2,NUMK
        DO 320 J=2,NUMJ-1
          DO 310 I=2,NUMI-1
            N2=NF  (I,J,K)
            NB=INDZ(I,J,K)
            IF (NB.GE.1) THEN
              INDB(1,NB)=I+NUMI*(J-1)+NUMI*NUMJ*(K-1)
              IF (N2.NE.-1) THEN
                INDB(2,NB)=5
              ELSE
                INDB(2,NB)=6
              ENDIF
              DO 300 L=3,MAXB1
                INDB(L,NB)=0
 300          CONTINUE
            ENDIF
 310      CONTINUE
 320    CONTINUE
 330  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
