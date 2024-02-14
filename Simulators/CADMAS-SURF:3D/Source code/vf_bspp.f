      SUBROUTINE VF_BSPP(XX,YY,ZZ,PP,FF,DBUF,NF)

CD=== 概要 ===========================================================

CDT   VF_BSPP:気体セルの圧力をゼロとし、表面セルの圧力を設定する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSR.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)    : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)    : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)    : IN  : R*8 : z方向格子座標等
CD    PP(@FOR-3D@)      : I/O : R*8 : 圧力
CD    FF(@FOR-3D@)      : IN  : R*8 : VOF関数F
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    NF(@FOR-3D@)      : IN  : I*4 : セルの状態を示すインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION PP  (NUMI,NUMJ,NUMK),FF  (NUMI,NUMJ,NUMK)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION NF  (NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

      IF (PVCP0.LT.ZERO) THEN

CD      -- 自由表面の圧力 --
        PPS=0.0D0

CD      -- 圧力を設定する(空気圧の計算無し) --
        DO 120 K=2,NUMK-1
          DO 110 J=MYJS,MYJE
            DO 100 I=MYIS,MYIE
              IF     (NF(I,J,K).EQ.8) THEN
                PP(I,J,K)=0.0D0
              ELSEIF (NF(I,J,K).EQ.5) THEN
                PSI=ZZ(3,K  )/(0.5D0*ZZ(2,K-1)+FF(I,J,K)*ZZ(2,K))
                PP(I,J,K)=(1.0D0-PSI)*PP(I,J,K-1)+PSI*PPS
              ELSEIF (NF(I,J,K).EQ.6) THEN
                PSI=ZZ(3,K+1)/(0.5D0*ZZ(2,K+1)+FF(I,J,K)*ZZ(2,K))
                PP(I,J,K)=(1.0D0-PSI)*PP(I,J,K+1)+PSI*PPS
              ELSEIF (NF(I,J,K).EQ.3) THEN
                PSI=YY(3,J  )/(0.5D0*YY(2,J-1)+FF(I,J,K)*YY(2,J))
                PP(I,J,K)=(1.0D0-PSI)*PP(I,J-1,K)+PSI*PPS
              ELSEIF (NF(I,J,K).EQ.4) THEN
                PSI=YY(3,J+1)/(0.5D0*YY(2,J+1)+FF(I,J,K)*YY(2,J))
                PP(I,J,K)=(1.0D0-PSI)*PP(I,J+1,K)+PSI*PPS
              ELSEIF (NF(I,J,K).EQ.1) THEN
                PSI=XX(3,I  )/(0.5D0*XX(2,I-1)+FF(I,J,K)*XX(2,I))
                PP(I,J,K)=(1.0D0-PSI)*PP(I-1,J,K)+PSI*PPS
              ELSEIF (NF(I,J,K).EQ.2) THEN
                PSI=XX(3,I+1)/(0.5D0*XX(2,I+1)+FF(I,J,K)*XX(2,I))
                PP(I,J,K)=(1.0D0-PSI)*PP(I+1,J,K)+PSI*PPS
              ENDIF
 100        CONTINUE
 110      CONTINUE
 120    CONTINUE

      ELSE

CD      -- 圧力を設定する(空気圧の計算有り) --
        DO 220 K=2,NUMK-1
          DO 210 J=MYJS,MYJE
            DO 200 I=MYIS,MYIE
              IF     (NF(I,J,K).EQ.8) THEN
                PP(I,J,K)=PP(I,J,K)
              ELSEIF (NF(I,J,K).EQ.5) THEN
                PPS=PP(I,J,K+1)
                PSI=ZZ(3,K  )/(0.5D0*ZZ(2,K-1)+FF(I,J,K)*ZZ(2,K))
                PP(I,J,K)=(1.0D0-PSI)*PP(I,J,K-1)+PSI*PPS
              ELSEIF (NF(I,J,K).EQ.6) THEN
                PPS=PP(I,J,K-1)
                PSI=ZZ(3,K+1)/(0.5D0*ZZ(2,K+1)+FF(I,J,K)*ZZ(2,K))
                PP(I,J,K)=(1.0D0-PSI)*PP(I,J,K+1)+PSI*PPS
              ELSEIF (NF(I,J,K).EQ.3) THEN
                PPS=PP(I,J+1,K)
                PSI=YY(3,J  )/(0.5D0*YY(2,J-1)+FF(I,J,K)*YY(2,J))
                PP(I,J,K)=(1.0D0-PSI)*PP(I,J-1,K)+PSI*PPS
              ELSEIF (NF(I,J,K).EQ.4) THEN
                PPS=PP(I,J-1,K)
                PSI=YY(3,J+1)/(0.5D0*YY(2,J+1)+FF(I,J,K)*YY(2,J))
                PP(I,J,K)=(1.0D0-PSI)*PP(I,J+1,K)+PSI*PPS
              ELSEIF (NF(I,J,K).EQ.1) THEN
                PPS=PP(I+1,J,K)
                PSI=XX(3,I  )/(0.5D0*XX(2,I-1)+FF(I,J,K)*XX(2,I))
                PP(I,J,K)=(1.0D0-PSI)*PP(I-1,J,K)+PSI*PPS
              ELSEIF (NF(I,J,K).EQ.2) THEN
                PPS=PP(I-1,J,K)
                PSI=XX(3,I+1)/(0.5D0*XX(2,I+1)+FF(I,J,K)*XX(2,I))
                PP(I,J,K)=(1.0D0-PSI)*PP(I+1,J,K)+PSI*PPS
              ENDIF
 200        CONTINUE
 210      CONTINUE
 220    CONTINUE

      ENDIF

      CALL VF_P3SRD2(PP,DBUF,0)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
