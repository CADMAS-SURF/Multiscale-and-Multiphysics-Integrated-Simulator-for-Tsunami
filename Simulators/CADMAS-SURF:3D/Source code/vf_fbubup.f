      SUBROUTINE VF_FBUBUP(ZZ,FF,GGV,TBUB,DBUF,NF)

CD=== 概要 ===========================================================

CDT   VF_FBUBUP:TimerDoor法による気泡の上昇

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSR.h'
      INCLUDE 'VF_ATIMER.h'

CD    -- 引数 --
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    FF(@FOR-3D@)     : I/O : R*8 : VOF関数F
CD    GGV(@FOR-3D@)    : IN  : R*8 : 空隙率
CD    TBUB(NUMK)       : I/O : R*8 : 気泡上昇処理を最後に行った時間
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    NF(@FOR-3D@)     : IN : I*4 : セルの状態を示すインデックス
      DIMENSION ZZ(MAXG1,NUMK),FF(NUMI,NUMJ,NUMK)
      DIMENSION GGV(NUMI,NUMJ,NUMK),TBUB(NUMK)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION NF(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 気泡セルの抽出と気泡の上昇 --
      DO 120 K=NUMK-2,2,-1
        DTBUB=ZZ(2,K)/WBUB
        IF (TNOW-TBUB(K).GT.DTBUB) THEN
          K1=K+1
          DO 110 J=MYJS,MYJE
            DO 100 I=MYIS,MYIE
              IF (FF(I,J,K).LT.FUPPER .AND. NF(I,J,K ).EQ. 0
     &                                .AND. NF(I,J,K1).NE.-1) THEN
                V0=ZZ(2,K )*GGV(I,J,K )
                V1=ZZ(2,K1)*GGV(I,J,K1)
                FMOVE=MIN((1.0D0-FF(I,J,K))*V0,FF(I,J,K1)*V1)
                FF(I,J,K1)=FF(I,J,K1)-FMOVE/V1
                FF(I,J,K )=FF(I,J,K )+FMOVE/V0
              ENDIF
 100        CONTINUE
 110      CONTINUE
          TBUB(K)=TBUB(K)+DTBUB
        ENDIF
 120  CONTINUE

      CALL VF_P3SRD2(FF,DBUF,0)
C     TBUB(K)はプロセス単位で良い

C     * デバッグ出力
C     DO 220 K=2,NUMK-1
C       DO 210 J=MYJS,MYJE
C         DO 200 I=MYIS,MYIE
C           IF (FF(I,J,K).LT.FUPPER .AND. NF(I,J,K).EQ.0) THEN
C             WRITE(*,*) 'FBUBUP',MYRANK,TNOW,
C    &                   0.5D0*(ZZ(1,K)+ZZ(1,K+1)),FF(I,J,K)
C           ENDIF
C200      CONTINUE
C210    CONTINUE
C220  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
