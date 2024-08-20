      SUBROUTINE DB_INI(XX,YY,ZZ,GV,IGRFIL,MX,MY,MZ,MYRANK)
C
CD=== 概要 ===========================================================
C
CDT   DB_INI:図化ファイルに格子数等を出力する
C
C==== 宣言 ===========================================================
C
      IMPLICIT NONE
C
ccc      INCLUDE 'CONNEC.h'
ccc      INCLUDE 'DOMAIN.h'
ccc      INCLUDE 'MODELI.h'
C
CD    -- 引数 --
CD    XX   (8,MX) : IN  : R*8 : x方向格子座標等
CD    YY   (8,MY) : IN  : R*8 : y方向格子座標等
CD    ZZ   (8,MZ) : IN  : R*8 : z方向格子座標等
CD    GV(MX,MY,MZ): IN  : R*8 : 空隙率
C
      REAL(8),INTENT(INOUT)::XX(6,MX),YY(6,MY),ZZ(6,MZ)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::IGRFIL
C
      INTEGER::I,I1,I2,IPRNT,IVR001,IVR002,J,J1,J2,K,K1,K2
      INTEGER::L1,L2,LF,LG,LK,LN,LP,LS,LT,LV,NB,NBX,NBY,NBZ
      INTEGER::MX,MY,MZ,MXM,MYM,MZM

      INTEGER::MYRANK
      CHARACTER*5 TEXTP

C
CD    -- 局所変数 --
C
      MXM = MX -1
      MYM = MY -1
      MZM = MZ -1
C
C==== 実行 ===========================================================
C
      NB=0
C
CD    -- 図化ファイルのオープンとメッセージの出力 --
      WRITE(TEXTP,'(I5.5)') MYRANK

      OPEN(IGRFIL,ERR=9010,FILE='data.grp_cad_'//TEXTP,
     &       STATUS='NEW',FORM='UNFORMATTED' )
C
      WRITE(*,9510) MYRANK
C
CD    -- バージョンを出力 --
      IVR001 = 1
      IVR002 = 0
      WRITE(IGRFIL,ERR=9020) IVR001,IVR002
CDEBUG      WRITE(IGRFIL,*,ERR=9020) IVR001
CDEBUG      WRITE(IGRFIL,*,ERR=9020) IVR002

CD    -- 解析領域を出力 --
      WRITE(IGRFIL,ERR=9020) MXM-1,MYM-1,MZM-1
CDEBUG      WRITE(IGRFIL,*,ERR=9020) MXM-1
CDEBUG      WRITE(IGRFIL,*,ERR=9020) MYM-1
CDEBUG      WRITE(IGRFIL,*,ERR=9020) MZM-1
      WRITE(IGRFIL,ERR=9020) XX(1,  2),YY(1,  2),ZZ(1,  2)
CDEBUG      WRITE(IGRFIL,*,ERR=9020) XX(1,  1)
CDEBUG      WRITE(IGRFIL,*,ERR=9020) YY(1,  1)
CDEBUG      WRITE(IGRFIL,*,ERR=9020) ZZ(1,  1)
      WRITE(IGRFIL,ERR=9020) XX(1,MX),YY(1,MY),ZZ(1,MZ)
CDEBUG      WRITE(IGRFIL,*,ERR=9020) XX(1,MXM)
CDEBUG      WRITE(IGRFIL,*,ERR=9020) YY(1,MYM)
CDEBUG      WRITE(IGRFIL,*,ERR=9020) ZZ(1,MZM)

CD    -- 出力領域を出力 --
      I1=1
      J1=1
      K1=1
      I2=MXM-1
      J2=MYM-1
      K2=MZM-1
      WRITE(IGRFIL,ERR=9020) I1,J1,K1,I2,J2,K2
CDEBUG      WRITE(IGRFIL,*,ERR=9020) I1
CDEBUG      WRITE(IGRFIL,*,ERR=9020) J1
CDEBUG      WRITE(IGRFIL,*,ERR=9020) K1
CDEBUG      WRITE(IGRFIL,*,ERR=9020) I2
CDEBUG      WRITE(IGRFIL,*,ERR=9020) J2
CDEBUG      WRITE(IGRFIL,*,ERR=9020) K2
      NBX=0
      NBY=0
      NBZ=0
      WRITE(IGRFIL,ERR=9020) NBX,NBY,NBZ
CDEBUG      WRITE(IGRFIL,*,ERR=9020) NBX
CDEBUG      WRITE(IGRFIL,*,ERR=9020) NBY
CDEBUG      WRITE(IGRFIL,*,ERR=9020) NBZ

CD    -- 時間毎に出力する物理量のフラグを出力 --
      LN=1
      LV=1
      LP=0
      LF=1
      LK=0
      LT=0
      IPRNT=0
ccc      IF (LTEMP.NE.0) LT=1
      LS=0
ccc      IF (LCONC.GT.0) LS=1
      LG=0
ccc      IF (IPRNT.GT.1) LG=1
      L1=0
      L2=0
      WRITE(IGRFIL,ERR=9020) LN,LV,LP,LF,LK,LT,LS,LG,L1,L2
CDEBUG      WRITE(IGRFIL,*,ERR=9020) LN
CDEBUG      WRITE(IGRFIL,*,ERR=9020) LV
CDEBUG      WRITE(IGRFIL,*,ERR=9020) LP
CDEBUG      WRITE(IGRFIL,*,ERR=9020) LF
CDEBUG      WRITE(IGRFIL,*,ERR=9020) LK
CDEBUG      WRITE(IGRFIL,*,ERR=9020) LT
CDEBUG      WRITE(IGRFIL,*,ERR=9020) LS
CDEBUG      WRITE(IGRFIL,*,ERR=9020) LG
CDEBUG      WRITE(IGRFIL,*,ERR=9020) L1
CDEBUG      WRITE(IGRFIL,*,ERR=9020) L2
      IPRNT=0
CD    -- 格子座標を出力 --
      WRITE(IGRFIL,ERR=9020) (XX(1,I),I=2,MX)
CDEBUG      DO 800 I=1,MXM
CDEBUG      WRITE(IGRFIL,*,ERR=9020) XX(1,I)
CDEBUG  800 CONTINUE
      WRITE(IGRFIL,ERR=9020) (YY(1,J),J=2,MY)
CDEBUG      DO 810 J=1,MYM
CDEBUG      WRITE(IGRFIL,*,ERR=9020) YY(1,J)
CDEBUG  810 CONTINUE
      WRITE(IGRFIL,ERR=9020) (ZZ(1,K),K=2,MZ)
CDEBUG      DO 820 K=1,MZM
CDEBUG      WRITE(IGRFIL,*,ERR=9020) ZZ(1,K)
CDEBUG  820 CONTINUE
CD    -- 空隙率を出力 --
      IF (IPRNT.LE.1) THEN
        WRITE(IGRFIL,ERR=9020) (((GV(I,J,K),I=2,MXM),J=2,MYM),K=2,MZM)
CDEBUG      DO 830 K=2,MZM
CDEBUG      DO 832 J=2,MYM
CDEBUG      DO 834 I=2,MXM
CDEBUG        WRITE(IGRFIL,*,ERR=9020) GV(I,J,K)
CDEBUG  834 CONTINUE
CDEBUG  832 CONTINUE
CDEBUG  830 CONTINUE
      ENDIF
C
C     -- 実行文の終了 --
 9000 CONTINUE
      GOTO 9999
C
C==== ファイル関連エラー処理 =========================================
C
 9010 CONTINUE
      WRITE(*,*) 'CAN NOT OPEN (data.grp).'
      GOTO 9999
      STOP
C
 9020 CONTINUE
      WRITE(*,*) 'DB_INI WRITE ERROR (data.grp).'
      GOTO 9999
      STOP
C
C==== フォーマット文 =================================================
C
 9510 FORMAT( ' ','>> FILE-GRP : OUT : INITIAL',I5)
C
C==== 終了 ===========================================================
C
 9999 CONTINUE
      RETURN
      END
