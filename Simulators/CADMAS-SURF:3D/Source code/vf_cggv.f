      SUBROUTINE VF_CGGV(ISW,T0,TN,DT,GGV,GGV0,GGVOLD,GGVNOW,NF)

CD=== 概要 ===========================================================

CDT   VF_CGGV:時間依存型の空隙率を設定する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_AFILER.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    ISW            : IN  : I*4 : スイッチ
CD                                 = 0:GGVに設定する
CD                                 !=0:GGV0に設定する
CD    T0             : IN  : R*8 : 対象とする時刻
CD    TN             : IN  : R*8 : サブループの時刻
CD    DT             : IN  : R*8 : サブループの時間刻み幅
CD    GGV(@FOR-3D@)  : I/O : R*8 : 空隙率
CD    GGV0(@FOR-3D@) : OUT : R*8 : 空隙率
CD    GGVOLD(IPRNP)  : IN  : R*8 : 前の時刻ブロックの空隙率
CD    GGVNOW(IPRNP)  : IN  : R*8 : 現在の時刻ブロックの空隙率
CD    NF(@FOR-3D@)   : IN  : I*4 : セルの状態を示すインデックス
      DIMENSION GGV(NUMI,NUMJ,NUMK),GGV0(NUMI,NUMJ,NUMK)
      DIMENSION GGVOLD(IPRNP),GGVNOW(IPRNP)
      DIMENSION NF(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 並列時のシフト分 --
      IP=MYGIS-1
      JP=MYGJS-1

CD    -- 重みを計算する --
      IF (IPRNT.EQ.1) THEN
        AOLD=0.0D0
        ANOW=1.0D0
      ELSE
        IF     (T0.LE.PRTOLD) THEN
          AOLD=1.0D0
          ANOW=0.0D0
        ELSEIF (T0.GE.PRTNOW) THEN
          AOLD=0.0D0
          ANOW=1.0D0
        ELSE
          ANOW=(T0-PRTOLD)/(PRTNOW-PRTOLD)
          AOLD=1.0D0-ANOW
        ENDIF
      ENDIF

CD    -- GGVに設定する --
      IF (ISW.EQ.0) THEN
        L=0
        DO 130 IB=1,IPRNB
          DO 120 K=IPRARA(3,IB),IPRARA(6,IB)
            DO 110 J=IPRARA(2,IB),IPRARA(5,IB)
              DO 100 I=IPRARA(1,IB),IPRARA(4,IB)
                L=L+1
                IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
                  IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
                    IF (NF(I-IP,J-JP,K).NE.-1)
     &                   GGV(I-IP,J-JP,K)=GGVOLD(L)*AOLD+GGVNOW(L)*ANOW
                  ENDIF
                ENDIF
 100          CONTINUE
 110        CONTINUE
 120      CONTINUE
 130    CONTINUE

CD    -- GGV0に設定する --
      ELSE
C       * 時間依存部分以外は同じ
        DO 220 K=1,NUMK
          DO 210 J=1,NUMJ
            DO 200 I=1,NUMI
              GGV0(I,J,K)=GGV(I,J,K)
 200        CONTINUE
 210      CONTINUE
 220    CONTINUE
C       * 時間依存部分を計算
        W=DT/(T0-TN+DT)
        L=0
        DO 330 IB=1,IPRNB
          DO 320 K=IPRARA(3,IB),IPRARA(6,IB)
            DO 310 J=IPRARA(2,IB),IPRARA(5,IB)
              DO 300 I=IPRARA(1,IB),IPRARA(4,IB)
                L=L+1
                IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
                  IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
                    IF (NF(I-IP,J-JP,K).NE.-1) THEN
                      V0=GGVOLD(L)*AOLD+GGVNOW(L)*ANOW
                      GGV0(I-IP,J-JP,K)=GGV(I-IP,J-JP,K)
     &                                  +W*(V0-GGV(I-IP,J-JP,K))
                    ENDIF
                  ENDIF
                ENDIF
 300          CONTINUE
 310        CONTINUE
 320      CONTINUE
 330    CONTINUE

      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
