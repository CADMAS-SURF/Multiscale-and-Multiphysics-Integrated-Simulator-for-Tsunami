      SUBROUTINE VF_BSSS(SC,DBUF,NF)

CD=== 概要 ===========================================================

CDT   VF_BSSS:気体セルをゼロとし、表面セルのスカラ量を設定する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    SC(@FOR-3D@) : I/O : R*8 : スカラ量
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    NF(@FOR-3D@) : IN  : I*4 : セルの状態を示すインデックス
      DIMENSION SC(NUMI,NUMJ,NUMK)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION NF(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- スカラー量を設定する --
      DO 120 K=2,NUMK-1
        DO 110 J=MYJS,MYJE
          DO 100 I=MYIS,MYIE
CD          * 気体セルはゼロ
            IF     (NF(I,J,K).EQ.8) THEN
              SC(I,J,K)=0.0D0
CD          * 表面セルは近傍の流体セルの算術平均
            ELSEIF (NF(I,J,K).GE.1) THEN
              N=0
              S=0.0D0
              IF (NF(I-1,J,K).EQ.0) THEN
                N=N+1
                S=S+SC(I-1,J,K)
              ENDIF
              IF (NF(I+1,J,K).EQ.0) THEN
                N=N+1
                S=S+SC(I+1,J,K)
              ENDIF
              IF (NF(I,J-1,K).EQ.0) THEN
                N=N+1
                S=S+SC(I,J-1,K)
              ENDIF
              IF (NF(I,J+1,K).EQ.0) THEN
                N=N+1
                S=S+SC(I,J+1,K)
              ENDIF
              IF (NF(I,J,K-1).EQ.0) THEN
                N=N+1
                S=S+SC(I,J,K-1)
              ENDIF
              IF (NF(I,J,K+1).EQ.0) THEN
                N=N+1
                S=S+SC(I,J,K+1)
              ENDIF
              SC(I,J,K)=S/DBLE(N)
            ENDIF
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

      CALL VF_P3SRD2(SC,DBUF,0)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
