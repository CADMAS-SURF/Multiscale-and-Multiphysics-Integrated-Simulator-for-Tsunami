      SUBROUTINE VF_FNFINI(FF,BCF,NF,INDX,INDY,INDZ,INDS,IBUF)

CD=== 概要 ===========================================================

CDT   VF_FNFINI:NFを流体セル、気体セルおよび表面セルに分類する
CD      (1)表面セルは暫定的に7とする

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    FF(@FOR-3D@)     : IN  : R*8 : VOF関数F
CD    BCF(NUMB)        : IN  : R*8 : VOF関数Fの境界値
CD    NF(@FOR-3D@)     : I/O : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : IN  : I*4 : z面の状態を示すインデックス
CD    INDS(@FOR-1D@)   : OUT : I*4 : 表面セルのI,J,K座標
CD    IBUF(NUMBUF*MAXBUF) : OUT :I*4 : 並列用のバッファ
      DIMENSION FF  (NUMI,NUMJ,NUMK),BCF (NUMB)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDS(NUMI*NUMJ*NUMK),IBUF(NUMBUF*MAXBUF)

C==== 実行 ===========================================================

CD    -- 全ての計算セルを流体セルとする --
      DO 120 K=2,NUMK-1
        DO 110 J=1,NUMJ
          DO 100 I=1,NUMI
            IF (NF(I,J,K).NE.-1) NF(I,J,K)=0
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

CD    -- F=0ならば気体セルとする --
      DO 220 K=2,NUMK-1
        DO 210 J=MYJS,MYJE
          DO 200 I=MYIS,MYIE
            IF (NF(I,J,K).NE.-1) THEN
              IF (FF(I,J,K).LT.FLOWER) THEN
                IX1=INDX(I  ,J,K)
                IF (IX1.GE.1) THEN
                  FX1=BCF(IX1)
                ELSE
                  FX1=FF(I-1,J,K)
                ENDIF
                IX2=INDX(I+1,J,K)
                IF (IX2.GE.1) THEN
                  FX2=BCF(IX2)
                ELSE
                  FX2=FF(I+1,J,K)
                ENDIF
                IY1=INDY(I,J  ,K)
                IF (IY1.GE.1) THEN
                  FY1=BCF(IY1)
                ELSE
                  FY1=FF(I,J-1,K)
                ENDIF
                IY2=INDY(I,J+1,K)
                IF (IY2.GE.1) THEN
                  FY2=BCF(IY2)
                ELSE
                  FY2=FF(I,J+1,K)
                ENDIF
                IZ1=INDZ(I,J,K  )
                IF (IZ1.GE.1) THEN
                  FZ1=BCF(IZ1)
                ELSE
                  FZ1=FF(I,J,K-1)
                ENDIF
                IZ2=INDZ(I,J,K+1)
                IF (IZ2.GE.1) THEN
                  FZ2=BCF(IZ2)
                ELSE
                  FZ2=FF(I,J,K+1)
                ENDIF
                IF (.NOT.(FZ1.GT.FUPPER .AND. IZ1.EQ.0)) THEN
                  IF (.NOT.(FY1.GT.FUPPER .AND. IY1.EQ.0)) THEN
                    IF (.NOT.(FY2.GT.FUPPER .AND. IY2.EQ.0)) THEN
                      IF (.NOT.(FX1.GT.FUPPER .AND. IX1.EQ.0)) THEN
                        IF (.NOT.(FX2.GT.FUPPER .AND. IX2.EQ.0)) THEN
                          IF (.NOT.(FZ2.GT.FUPPER .AND. IZ2.EQ.0)) THEN
                            NF(I,J,K)=8
                          ENDIF
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
 200      CONTINUE
 210    CONTINUE
 220  CONTINUE
      CALL VF_P3SRI1(NF,IBUF,0)

CD    ** 中判定反復 **
      ITER=0
 1000 CONTINUE
        ITER=ITER+1

CD      -- 気体セルに隣接したセルを表面セルとする --
        NS=0
        DO 1120 K=2,NUMK-1
          DO 1110 J=MYJS,MYJE
            DO 1100 I=MYIS,MYIE
              IF (NF(I,J,K).NE.-1) THEN
                IF (NF(I,J,K).NE.8) THEN
                  IF (NF(I,J,K+1).EQ.8 .OR. NF(I,J+1,K).EQ.8 .OR.
     &                NF(I,J-1,K).EQ.8 .OR. NF(I+1,J,K).EQ.8 .OR.
     &                NF(I-1,J,K).EQ.8 .OR. NF(I,J,K-1).EQ.8     ) THEN
                    NS=NS+1
                    INDS(NS)=I+NUMI*(J-1)+NUMI*NUMJ*(K-1)
                    NF(I,J,K)=7
                  ENDIF
                ENDIF
              ENDIF
 1100       CONTINUE
 1110     CONTINUE
 1120   CONTINUE
        CALL VF_P3SRI1(NF,IBUF,0)

CD      -- 表面セルのうち流体に隣接していないセルを気体とする --
        NUMS=0
        DO 1200 L=1,NS
          I00=INDS(L)
          IJK=I00
          K  =(IJK-1)/(NUMI*NUMJ)+1
          IJK=IJK-NUMI*NUMJ*(K-1)
          J  =(IJK-1)/NUMI+1
          I  =IJK-NUMI*(J-1)
          IF (NF(I,J,K+1).NE.0 .AND. NF(I,J+1,K).NE.0 .AND.
     &        NF(I,J-1,K).NE.0 .AND. NF(I+1,J,K).NE.0 .AND.
     &        NF(I-1,J,K).NE.0 .AND. NF(I,J,K-1).NE.0      ) THEN
            NF(I,J,K)=8
          ELSE
            NUMS=NUMS+1
            INDS(NUMS)=I00
          ENDIF
 1200   CONTINUE
        CALL VF_P3SRI1(NF,IBUF,0)

CD      -- 表面セルのうち流体と気体に挟まれていないセルを検索する --
        NCH=0
        DO 1300 L=1,NUMS
          IJK=INDS(L)
          K  =(IJK-1)/(NUMI*NUMJ)+1
          IJK=IJK-NUMI*NUMJ*(K-1)
          J  =(IJK-1)/NUMI+1
          I  =IJK-NUMI*(J-1)
          IF (NF(I,J,K-1).NE.0 .OR. NF(I,J,K+1).NE.8) THEN
            IF (NF(I-1,J,K).NE.0 .OR. NF(I+1,J,K).NE.8) THEN
              IF (NF(I+1,J,K).NE.0 .OR. NF(I-1,J,K).NE.8) THEN
                IF (NF(I,J-1,K).NE.0 .OR. NF(I,J+1,K).NE.8) THEN
                  IF (NF(I,J+1,K).NE.0 .OR. NF(I,J-1,K).NE.8) THEN
                    IF (NF(I,J,K+1).NE.0 .OR. NF(I,J,K-1).NE.8) THEN
                      NF(I,J,K)=-7
                      NCH=NCH+1
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
 1300   CONTINUE
        IW=NCH
        CALL VF_P1SUMI(IW,NCH)
        CALL VF_P3SRI1(NF,IBUF,0)

CD      ** 気体セルに変更される表面セルがなくなるまで **
        IF (NCH.LE.0) GOTO 2000

CD      -- 検索されたセルを気体セルとする --
        DO 1400 L=1,NUMS
          IJK=INDS(L)
          K  =(IJK-1)/(NUMI*NUMJ)+1
          IJK=IJK-NUMI*NUMJ*(K-1)
          J  =(IJK-1)/NUMI+1
          I  =IJK-NUMI*(J-1)
          IF (NF(I,J,K).EQ.-7) NF(I,J,K)=8
 1400   CONTINUE
        CALL VF_P3SRI1(NF,IBUF,0)

CD    ** 反復終了 **
        GOTO 1000
 2000 CONTINUE

      CALL VF_P3SRI2(NF,IBUF,0)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
