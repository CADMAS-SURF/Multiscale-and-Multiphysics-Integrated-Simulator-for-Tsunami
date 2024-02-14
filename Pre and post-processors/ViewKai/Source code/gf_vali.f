      SUBROUTINE GF_VALI(N,IVAL)

CD=== 概要 ===========================================================

CDT   GF_VALI:NF等の読込/整数

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'GF_CONV.h'

CD    -- 引数 --
CD    N          : IN  : I*4 : 自分のランク+1
CD    IVAL(@@@@) : I/O : R*8 : NF等
      DIMENSION IVAL(NUMI0,NUMJ0,NUMK0)

CD    -- 局所変数 --
      CHARACTER*13 FILENM

C==== 実行 ===========================================================

CD    -- 入力ファイルのファイル名 --
      IFL=IFLIN+N-1
      WRITE(FILENM,'(I5.5)') N-1
      FILENM='data.grp'//FILENM

CD    -- データの範囲 --
      I1=MAX(IG1,MYIS(N)+(MYGIS(N)-1)-1)
      J1=MAX(JG1,MYJS(N)+(MYGJS(N)-1)-1)
      K1=KG1
      I2=MIN(IG2,MYIE(N)+(MYGIS(N)-1)-1)
      J2=MIN(JG2,MYJE(N)+(MYGJS(N)-1)-1)
      K2=KG2

CD    -- 物理量 --
      READ(IFL,ERR=9010) (((IVAL(I,J,K),I=I1,I2),J=J1,J2),K=K1,K2)

C     -- 実行文の終了 --
      GOTO 9999

C==== エラー処理 =====================================================

 9010 CONTINUE
      WRITE(*,*) 'GF_VALI : READ ERROR (',FILENM,').'
      STOP

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
