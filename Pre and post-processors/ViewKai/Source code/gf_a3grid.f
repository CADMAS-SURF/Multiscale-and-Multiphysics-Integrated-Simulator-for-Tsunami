      SUBROUTINE GF_A3GRID(N,XX,YY,ZZ,VAL)

CD=== 概要 ===========================================================

CDT   GF_A3GRID:格子座標等の読込と出力

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'GF_CONV.h'

CD    -- 引数 --
CD    N         : IN  : I*4 : 自分のランク+1
CD    XX(NUMI0) : OUT : R*8 : x方向格子座標
CD    YY(NUMJ0) : OUT : R*8 : y方向格子座標
CD    ZZ(NUMK0) : OUT : R*8 : z方向格子座標
CD    VAL(@@@@) : OUT : R*8 : 物理量
      DIMENSION XX(NUMI0),YY(NUMJ0),ZZ(NUMK0)
      DIMENSION VAL(NUMI0,NUMJ0,NUMK0)

CD    -- 局所変数 --
      CHARACTER*13 FILENM

C==== 実行 ===========================================================

CD    -- 入力ファイルのファイル名 --
      IFL=IFLIN+N-1
      WRITE(FILENM,'(I5.5)') N-1
      FILENM='data.grp'//FILENM

CD    -- 格子座標 --
      LP=MYGIS(N)-1
      READ (IFL  ,ERR=9010) (XX(LP+I),I=1,NUMI(N))
      IF (N.EQ.NPROCS) THEN
        WRITE(IFLOU,ERR=9020) (XX(I),I=1,NUMI0)
      ENDIF
      LP=MYGJS(N)-1
      READ (IFL  ,ERR=9010) (YY(LP+J),J=1,NUMJ(N))
      IF (N.EQ.NPROCS) THEN
        WRITE(IFLOU,ERR=9020) (YY(J),J=1,NUMJ0)
      ENDIF
      LP=0
      READ (IFL  ,ERR=9010) (ZZ(LP+K),K=1,NUMK0  )
      IF (N.EQ.NPROCS) THEN
        WRITE(IFLOU,ERR=9020) (ZZ(K),K=1,NUMK0)
      ENDIF

CD    -- 境界のインデックス --
      CALL GF_BCI(N)
C@    XXXXXXXXXXXXXX

CD    -- 空隙率 --
      IF (ISWLG.EQ.0) THEN
        CALL GF_VALD(N,VAL,0)
        IF (N.EQ.NPROCS) THEN
          WRITE(IFLOU,ERR=9020)
     &           (((VAL(I,J,K),I=IG1,IG2),J=JG1,JG2),K=KG1,KG2)
        ENDIF
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== エラー処理 =====================================================

 9010 CONTINUE
      WRITE(*,*) 'GF_A3GRID : READ ERROR (',FILENM,').'
      STOP

 9020 CONTINUE
      WRITE(*,*) 'GF_A3GRID : WRITE ERROR (data.grp).'
      STOP

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
