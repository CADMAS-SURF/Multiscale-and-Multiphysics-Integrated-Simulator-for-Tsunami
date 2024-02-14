      SUBROUTINE VF_OL3DNF(NF)

CD=== 概要 ===========================================================

CDT   VF_OL2DNF:NFをリストファイルに出力
CD      (1)障害物は「*」と出力する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    NF(@FOR-3D@) : IN : I*4 : セルの状態を示すインデックス
      DIMENSION NF(NUMI,NUMJ,NUMK)

CD    -- 局所変数 --
CD    MAXELE            : PRM : 1行に出力する要素数
      PARAMETER (MAXELE=100)

C==== 実行 ===========================================================

CD    -- 出力断面の番号を設定する --
      IF (ILPARA(2).EQ.1) THEN
        M1=1
        M2=0
      ELSE
        M1=ILPARA(2)
        M2=M1
      ENDIF

CD    -- XY断面を出力する --
      IF     (ILPARA(1).EQ.1) THEN
        IF (M2.EQ.0) M2=NUMK
        N1=NUMI
        N2=NUMJ
        I1P=MYGIS-1
        I2P=MYGJS-1
        DO 120 M=M1,M2
          DO 110 L=1,(N1-1)/MAXELE+1
            I1S=MAXELE*(L-1)+1
            I1E=MIN(MAXELE*L,N1)
            WRITE(ILPFIL,9510) 'K=',M-1,'I=',I1S-1+I1P
            WRITE(ILPFIL,9520) '   J/I',(MOD(I1-1+I1P,10),I1=I1S,I1E)
            DO 100 I2=N2,1,-1
              WRITE(ILPFIL,9530) I2-1+I2P,(NF(I1,I2,M),I1=I1S,I1E)
 100        CONTINUE
 110      CONTINUE
 120    CONTINUE

CD    -- XZ断面を出力する --
      ELSEIF (ILPARA(1).EQ.2) THEN
        IF (M2.EQ.0) M2=NUMJ0
        MS=MYGJS
        ME=MYGJE
        N1=NUMI
        N2=NUMK
        I1P=MYGIS-1
        DO 220 M=M1,M2
          IF (MS.LE.M .AND. M.LE.ME) THEN
            DO 210 L=1,(N1-1)/MAXELE+1
              I1S=MAXELE*(L-1)+1
              I1E=MIN(MAXELE*L,N1)
              WRITE(ILPFIL,9510) 'J=',M-1,'I=',I1S-1+I1P
              WRITE(ILPFIL,9520) '   K/I',(MOD(I1-1+I1P,10),I1=I1S,I1E)
              DO 200 I2=N2,1,-1
                WRITE(ILPFIL,9530) I2-1,(NF(I1,M-MS+1,I2),I1=I1S,I1E)
 200          CONTINUE
 210        CONTINUE
          ENDIF
 220    CONTINUE

CD    -- YZ断面を出力する --
      ELSEIF (ILPARA(1).EQ.3) THEN
        IF (M2.EQ.0) M2=NUMI0
        MS=MYGIS
        ME=MYGIE
        N1=NUMJ
        N2=NUMK
        I1P=MYGJS-1
        DO 320 M=M1,M2
          IF (MS.LE.M .AND. M.LE.ME) THEN
            DO 310 L=1,(N1-1)/MAXELE+1
              I1S=MAXELE*(L-1)+1
              I1E=MIN(MAXELE*L,N1)
              WRITE(ILPFIL,9510) 'I=',M-1,'J=',I1S-1+I1P
              WRITE(ILPFIL,9520) '   K/J',(MOD(I1-1+I1P,10),I1=I1S,I1E)
              DO 300 I2=N2,1,-1
                WRITE(ILPFIL,9530) I2-1,(NF(M-MS+1,I1,I2),I1=I1S,I1E)
 300          CONTINUE
 310        CONTINUE
          ENDIF
 320    CONTINUE

CD    -- YX断面を出力する --
      ELSEIF (ILPARA(1).EQ.4) THEN
        IF (M2.EQ.0) M2=NUMK
        N1=NUMJ
        N2=NUMI
        I1P=MYGJS-1
        I2P=MYGIS-1
        DO 420 M=M1,M2
          DO 410 L=1,(N1-1)/MAXELE+1
            I1S=MAXELE*(L-1)+1
            I1E=MIN(MAXELE*L,N1)
            WRITE(ILPFIL,9510) 'K=',M-1,'J=',I1S-1+I1P
            WRITE(ILPFIL,9520) '   I/J',(MOD(I1-1+I1P,10),I1=I1S,I1E)
            DO 400 I2=N2,1,-1
              WRITE(ILPFIL,9530) I2-1+I2P,(NF(I2,I1,M),I1=I1S,I1E)
 400        CONTINUE
 410      CONTINUE
 420    CONTINUE

CD    -- ZX断面を出力する --
      ELSEIF (ILPARA(1).EQ.5) THEN
        IF (M2.EQ.0) M2=NUMJ0
        MS=MYGJS
        ME=MYGJE
        N1=NUMK
        N2=NUMI
        I2P=MYGIS-1
        DO 520 M=M1,M2
          IF (MS.LE.M .AND. M.LE.ME) THEN
            DO 510 L=1,(N1-1)/MAXELE+1
              I1S=MAXELE*(L-1)+1
              I1E=MIN(MAXELE*L,N1)
              WRITE(ILPFIL,9510) 'J=',M-1,'K=',I1S-1
              WRITE(ILPFIL,9520) '   I/K',(MOD(I1-1,10),I1=I1S,I1E)
              DO 500 I2=N2,1,-1
                WRITE(ILPFIL,9530)
     &                       I2-1+I2P,(NF(I2,M-MS+1,I1),I1=I1S,I1E)
 500          CONTINUE
 510        CONTINUE
          ENDIF
 520    CONTINUE

CD    -- ZY断面を出力する --
      ELSEIF (ILPARA(1).EQ.6) THEN
        IF (M2.EQ.0) M2=NUMI0
        MS=MYGIS
        ME=MYGIE
        N1=NUMK
        N2=NUMJ
        I2P=MYGJS-1
        DO 620 M=M1,M2
          IF (MS.LE.M .AND. M.LE.ME) THEN
            DO 610 L=1,(N1-1)/MAXELE+1
              I1S=MAXELE*(L-1)+1
              I1E=MIN(MAXELE*L,N1)
              WRITE(ILPFIL,9510) 'I=',M-1,'K=',I1S-1
              WRITE(ILPFIL,9520) '   J/K',(MOD(I1-1,10),I1=I1S,I1E)
              DO 600 I2=N2,1,-1
                WRITE(ILPFIL,9530)
     &                       I2-1+I2P,(NF(M-MS+1,I2,I1),I1=I1S,I1E)
 600          CONTINUE
 610        CONTINUE
          ENDIF
 620    CONTINUE
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT(/' ',A2,I6,' : ',A2,I6)
 9520 FORMAT( ' ',A6,' ',200(I1:))
 9530 FORMAT( ' ',I6,' ',200(I1:))

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
