      SUBROUTINE VF_BWFFSF(ZZ,FF,DMTBTT,DMTBHH,DMTBTT2,DMTBHH2,DBUF,
     &                     NF,INDX,INDY,INDB)

CD=== 概要 ===========================================================

CDT   VF_BWFFSF:水位固定時の境界面に接するセルのVOF関数Fを設定する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSI.h'
      INCLUDE 'VF_APHYSR.h'
      INCLUDE 'VF_ATIMER.h'

CD    -- 引数 --
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    FF(@FOR-3D@)     : OUT : R*8 : VOF関数F
CD    DMTBTT(MTBTT)    : IN  : R*8 : マトリクスデータの無次元位相
CD    DMTBHH(MTBTT)    : IN  : R*8 : マトリクスデータの水位
CD    DMTBTT2(MTBTT2)  : IN  : R*8 : マトリクスデータ-2
CD    DMTBHH2(MTBTT2)  : IN  : R*8 : マトリクスデータ-2
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDB(MAXB1,NUMB) : IN  : I*4 : 境界面のインデックス
      DIMENSION ZZ(MAXG1,NUMK),FF(NUMI,NUMJ,NUMK)
      DIMENSION DMTBTT (MTBTT ),DMTBHH (MTBTT )
      DIMENSION DMTBTT2(MTBTT2),DMTBHH2(MTBTT2)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDB(MAXB1,NUMB)

C     -- 局所変数 --
      REAL*4 R4TN

C==== 実行 ===========================================================

CD    -- 並列時のシフト分 --
      IP=MYGIS-1
      JP=MYGJS-1

CD    -- 水位固定に接するセルのVOF関数Fを設定する --
      DO 500 JD=1,4

CD      -- 法線方向への造波境界 --
        IF     (IBCTYP(1,JD).EQ.1) THEN
          N =IBCTYP(2,JD)
          IF ((N.EQ.-3 .AND. MTBTYP .EQ.3) .OR.
     &        (N.EQ.-4 .AND. MTBTYP2.EQ.3)     ) THEN
            T =BCTYP (3,JD)
            T0=BCTYP (6,JD)
            A =BCTYP (8,JD)
C           * 造波境界のための無次元位相の計算
C             CADMAS-SURFとの結果が変わらないよう、倍精度は別に計算
            TN=T0-TNOW/T
            TN=TN-DBLE(INT(TN))
            IF (TN.LT.0.0D0) TN=TN+1.0D0
            R4TN=REAL(T0-TNOW/T)
            R4TN=R4TN-REAL(INT(R4TN))
            IF (R4TN.LT.0.0E0) R4TN=R4TN+1.0E0
C           * 増幅率の計算
            AW=1.0D0
            IF (A.GE.ZERO) THEN
              A=TNOW/T/A
              IF (A.LT.1.0D0) AW=0.5D0*SIN(PI*(A-0.5D0))+0.5D0
            ENDIF
C           * 期待する水位を計算する
            WVT=DBLE(R4TN)
            IF (N.EQ.-3) THEN
              CALL VF_CWMTB1 (WVT,WMT1,WMT2,WVZ,DMTBTT ,DMTBHH )
            ELSE
              CALL VF_CWMTB12(WVT,WMT1,WMT2,WVZ,DMTBTT2,DMTBHH2)
            ENDIF
            BCTYP(7,JD)=WVZ*AW
            WL=WVLVL+BCTYP(7,JD)
C           * x方向境界面に接するセル
            IF (JD.EQ.1 .OR. JD.EQ.2) THEN
              IF (JD.EQ.1) THEN
                IC=2
              ELSE
                IC=NUMI0-1
              ENDIF
              IF (MYGIS.LE.IC .AND. IC.LE.MYGIE) THEN
                DO 110 K=2,NUMK-1
                  IF     (ZZ(1,K  ).GT.WL) THEN
                    VAL=0.0D0
                  ELSEIF (ZZ(1,K+1).LT.WL) THEN
                    VAL=1.0D0
                  ELSE
                    VAL=(WL-ZZ(1,K))/(ZZ(1,K+1)-ZZ(1,K))
                  ENDIF
                  DO 100 J=IBCTYP(3,JD),IBCTYP(4,JD)
                    IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
                      IF (NF(IC-IP,J-JP,K).NE.-1) FF(IC-IP,J-JP,K)=VAL
                    ENDIF
 100              CONTINUE
 110            CONTINUE
              ENDIF
C           * y方向境界面に接するセル
            ELSE
              IF (JD.EQ.3) THEN
                JC=2
              ELSE
                JC=NUMJ0-1
              ENDIF
              IF (MYGJS.LE.JC .AND. JC.LE.MYGJE) THEN
                DO 210 K=2,NUMK-1
                  IF     (ZZ(1,K  ).GT.WL) THEN
                    VAL=0.0D0
                  ELSEIF (ZZ(1,K+1).LT.WL) THEN
                    VAL=1.0D0
                  ELSE
                    VAL=(WL-ZZ(1,K))/(ZZ(1,K+1)-ZZ(1,K))
                  ENDIF
                  DO 200 I=IBCTYP(3,JD),IBCTYP(4,JD)
                    IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
                      IF (NF(I-IP,JC-JP,K).NE.-1) FF(I-IP,JC-JP,K)=VAL
                    ENDIF
 200              CONTINUE
 210            CONTINUE
              ENDIF
            ENDIF
          ENDIF
        ENDIF
 500  CONTINUE

      CALL VF_P3SRD2(FF,DBUF,0)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
