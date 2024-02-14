      SUBROUTINE GF_VALD(N,VAL,ISW)

CD=== 概要 ===========================================================

CDT   GF_VALD:物理量の読込/実数

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'GF_CONV.h'

CD    -- 引数 --
CD    N         : IN  : I*4 : 自分のランク+1
CD    VAL(@@@@) : I/O : R*8 : 物理量
CD    ISW       : IN  : R*8 : 定義位置
      DIMENSION VAL(NUMI0,NUMJ0,NUMK0)

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
      IF     (ISW.EQ.0) THEN
        READ(IFL,ERR=9010) (((VAL(I,J,K),I=I1,I2),J=J1,J2),K=K1,K2)
      ELSEIF (ISW.EQ.1) THEN
        READ(IFL,ERR=9010) (((VAL(I,J,K),I=I1,I2+1),J=J1,J2),K=K1,K2)
      ELSEIF (ISW.EQ.2) THEN
        READ(IFL,ERR=9010) (((VAL(I,J,K),I=I1,I2),J=J1,J2+1),K=K1,K2)
      ELSE
        READ(IFL,ERR=9010) (((VAL(I,J,K),I=I1,I2),J=J1,J2),K=K1,K2+1)
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== エラー処理 =====================================================

 9010 CONTINUE
      WRITE(*,*) 'GF_VALD : READ ERROR (',FILENM,').'
      STOP

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
