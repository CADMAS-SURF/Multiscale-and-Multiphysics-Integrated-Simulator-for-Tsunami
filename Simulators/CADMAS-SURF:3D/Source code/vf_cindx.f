      SUBROUTINE VF_CINDX(NF,INDX,INDY,INDZ)

CD=== 概要 ===========================================================

CDT   VF_CINDX:面の状態を示すインデックスINDX,INDY,INDZを設定する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    NF(@FOR-3D@)   : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@) : OUT : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@) : OUT : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@) : OUT : I*4 : z面の状態を示すインデックス
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 境界面の数をゼロクリア --
      NUMB=0

CD    -- 全てを通常面とする --
      DO 120 K=1,NUMK
        DO 110 J=1,NUMJ
          DO 100 I=1,NUMI
            INDX(I,J,K)=0
            INDY(I,J,K)=0
            INDZ(I,J,K)=0
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

CD    -- INDXを設定 --
      DO 210 K=1,NUMK
        DO 200 J=1,NUMJ
          IF (MYGIS.EQ.1) INDX(1,J,K)=-1
 200    CONTINUE
 210  CONTINUE
      DO 240 K=1,NUMK
        DO 230 J=1,NUMJ
          DO 220 I=2,NUMI
            N1=NF(I-1,J,K)
            N2=NF(I  ,J,K)
            IF (N1.EQ.-1 .AND. N2.EQ.-1) INDX(I,J,K)=-1
 220      CONTINUE
 230    CONTINUE
 240  CONTINUE
      DO 270 K=2,NUMK-1
        DO 260 J=2,NUMJ-1
          DO 250 I=2,NUMI
            IF (INDX(I,J,K).NE.-1) THEN
              N1=NF(I-1,J,K)
              N2=NF(I  ,J,K)
              IF (N1.EQ.-1 .OR. N2.EQ.-1) THEN
                IF     (I.EQ.2    .AND. N2.EQ.-1) THEN
                ELSEIF (I.EQ.NUMI .AND. N1.EQ.-1) THEN
                ELSE
                  NUMB=NUMB+1
                  INDX(I,J,K)=NUMB
                ENDIF
              ENDIF
            ENDIF
 250      CONTINUE
 260    CONTINUE
 270  CONTINUE

CD    -- INDYを設定 --
      DO 310 K=1,NUMK
        DO 300 I=1,NUMI
          IF (MYGJS.EQ.1) INDY(I,1,K)=-1
 300    CONTINUE
 310  CONTINUE
      DO 340 K=1,NUMK
        DO 330 J=2,NUMJ
          DO 320 I=1,NUMI
            N1=NF(I,J-1,K)
            N2=NF(I,J  ,K)
            IF (N1.EQ.-1 .AND. N2.EQ.-1) INDY(I,J,K)=-1
 320      CONTINUE
 330    CONTINUE
 340  CONTINUE
      DO 370 K=2,NUMK-1
        DO 360 J=2,NUMJ
          DO 350 I=2,NUMI-1
            IF (INDY(I,J,K).NE.-1) THEN
              N1=NF(I,J-1,K)
              N2=NF(I,J  ,K)
              IF (N1.EQ.-1 .OR. N2.EQ.-1) THEN
                IF     (J.EQ.2    .AND. N2.EQ.-1) THEN
                ELSEIF (J.EQ.NUMJ .AND. N1.EQ.-1) THEN
                ELSE
                  NUMB=NUMB+1
                  INDY(I,J,K)=NUMB
                ENDIF
              ENDIF
            ENDIF
 350      CONTINUE
 360    CONTINUE
 370  CONTINUE

CD    -- INDZを設定 --
      DO 410 J=1,NUMJ
        DO 400 I=1,NUMI
          INDZ(I,J,1)=-1
 400    CONTINUE
 410  CONTINUE
      DO 440 K=2,NUMK
        DO 430 J=1,NUMJ
          DO 420 I=1,NUMI
            N1=NF(I,J,K-1)
            N2=NF(I,J,K  )
            IF (N1.EQ.-1 .AND. N2.EQ.-1) INDZ(I,J,K)=-1
 420      CONTINUE
 430    CONTINUE
 440  CONTINUE
      DO 470 K=2,NUMK
        DO 460 J=2,NUMJ-1
          DO 450 I=2,NUMI-1
            IF (INDZ(I,J,K).NE.-1) THEN
              N1=NF(I,J,K-1)
              N2=NF(I,J,K  )
              IF (N1.EQ.-1 .OR. N2.EQ.-1) THEN
                NUMB=NUMB+1
                INDZ(I,J,K)=NUMB
              ENDIF
            ENDIF
 450      CONTINUE
 460    CONTINUE
 470  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      NUMBX=MAX(NUMB,1)
      RETURN
      END
