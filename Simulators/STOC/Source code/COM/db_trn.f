      SUBROUTINE DB_TRN(UU,VV,WW,PP,FF,TT,CC,GV,
     &                  NF,dzbed,INDP,KF,ILPFIL,IGRFIL)
C
CD=== 概要 ===========================================================
C
CDT   DB_TRN:解析結果を図化ファイルに出力する
C
C==== 宣言 ===========================================================
C
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'TIMER.h'
C
CD    -- 引数 --
CD    UU(MX,MY,MZ)     : IN  : R*8 : x方向流速
CD    VV(MX,MY,MZ)     : IN  : R*8 : y方向流速
CD    WW(MX,MY,MZ)     : IN  : R*8 : z方向流速
CD    PP(MX,MY,MZ)     : IN  : R*8 : 圧力
CD    FF(MX,MY,MZ)     : IN  : R*8 : VOF関数F
CD    TT(MX,MY,MZ)     : IN  : R*8 : 温度
CD    CC(MX,MY,MZ)     : IN : R*8 : 濃度
CD    INDP(MX,MY,MZ)   : IN  : I*4 : z面の状態を示すインデックス
CD    NF(MX,MY,MZ)     : IN  : I*4 : セルの状態を示すインデックス
CD    KF(MX,MY)        : IN  : I*4 : 自由表面の位置を示すインデックス
C
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::WW(MX,MY,MZ),PP(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::FF(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TT(MX,MY,MZ),CC(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::DZBED(MX,MY)
      INTEGER,INTENT(INOUT)::NF(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),KF(MX,MY)
      INTEGER,INTENT(INOUT)::ILPFIL,IGRFIL
C
      INTEGER::I,I1,I2,J,J1,J2,K,K1,K2,NB
C
C==== 実行 ===========================================================

CD    -- メッセージの出力 --
      WRITE(ILPFIL,9510) ISTEP,TIME

CD    -- 定数の設定 --
      I1=1
      J1=1
      K1=1
      I2=MXM-1
      J2=MYM-1
      K2=MZM-1
      NB=0

CD    -- 計算情報を出力 --
      WRITE(IGRFIL,ERR=9010) ISTEP,TIME
CDEBUG      WRITE(IGRFIL,*,ERR=9010) ISTEP
CDEBUG      WRITE(IGRFIL,*,ERR=9010) TIME
C 
      DO 100 J=2,MYM
      DO 110 I=2,MXM
      DO 120 K=2,MZM
        NF(I,J,K)=-1
        IF(INDP(I,J,K).EQ.1) THEN
          IF(K.GT.KF(I,J)) THEN
            NF(I,J,K)=8
          ELSE IF(K.EQ.KF(I,J)) THEN
            NF(I,J,K)=5
          ELSE
            NF(I,J,K)=0
          END IF
        END IF
  120 CONTINUE
  110 CONTINUE
  100 CONTINUE
C
CD    -- セルの状態を示すインデックスを出力 --
      WRITE(IGRFIL,ERR=9010) (((NF(I,J,K),I=2,MXM),J=2,MYM),K=2,MZM)
CDEBUG      DO 800 K=2,MZM
CDEBUG      DO 802 J=2,MYM
CDEBUG      DO 804 I=2,MXM
CDEBUG      WRITE(IGRFIL,*,ERR=9010) NF(I,J,K)
CDEBUG  804 CONTINUE
CDEBUG  802 CONTINUE
CDEBUG  800 CONTINUE
CD    -- 流速を出力 --
      WRITE(IGRFIL,ERR=9010)
     $   (((REAL(UU(I,J,K)),I=1,MXM),J=2,MYM),K=2,MZM)
CDEBUG      DO 810 K=2,MZM
CDEBUG      DO 812 J=2,MYM
CDEBUG      DO 814 I=1,MXM
CDEBUG      WRITE(IGRFIL,*,ERR=9010) UU(I,J,K)
CDEBUG  814 CONTINUE
CDEBUG  812 CONTINUE
CDEBUG  810 CONTINUE
      WRITE(IGRFIL,ERR=9010)
     $   (((REAL(VV(I,J,K)),I=2,MXM),J=1,MYM),K=2,MZM)
CDEBUG      DO 820 K=2,MZM
CDEBUG      DO 822 J=1,MYM
CDEBUG      DO 824 I=2,MXM
CDEBUG      WRITE(IGRFIL,*,ERR=9010) VV(I,J,K)
CDEBUG  824 CONTINUE
CDEBUG  822 CONTINUE
CDEBUG  820 CONTINUE
      WRITE(IGRFIL,ERR=9010)
     $   (((REAL(WW(I,J,K)),I=2,MXM),J=2,MYM),K=1,MZM)
CDEBUG      DO 830 K=1,MZM
CDEBUG      DO 832 J=2,MYM
CDEBUG      DO 834 I=2,MXM
CDEBUG      WRITE(IGRFIL,*,ERR=9010) WW(I,J,K)
CDEBUG  834 CONTINUE
CDEBUG  832 CONTINUE
CDEBUG  830 CONTINUE
CD    -- 圧力を出力 --
!!!      WRITE(IGRFIL,ERR=9010) (((PP(I,J,K),I=2,MXM),J=2,MYM),K=2,MZM)
CDEBUG      DO 840 K=2,MZM
CDEBUG      DO 842 J=2,MYM
CDEBUG      DO 844 I=2,MXM
CDEBUG      WRITE(IGRFIL,*,ERR=9010) PP(I,J,K)
CDEBUG  844 CONTINUE
CDEBUG  842 CONTINUE
CDEBUG  840 CONTINUE
CD    -- VOF関数Fを出力 --
      WRITE(IGRFIL,ERR=9010) (((FF(I,J,K),I=2,MXM),J=2,MYM),K=2,MZM)
CDEBUG      DO 850 K=2,MZM
CDEBUG      DO 852 J=2,MYM
CDEBUG      DO 854 I=2,MXM
CDEBUG      WRITE(IGRFIL,*,ERR=9010) FF(I,J,K)
CDEBUG  854 CONTINUE
CDEBUG  852 CONTINUE
CDEBUG  850 CONTINUE
CD    -- 地形変化量を出力 --
      IF(LSEDI.EQ.1) THEN
      WRITE(IGRFIL,ERR=9010) (((GV(I,J,K),I=2,MXM),J=2,MYM),K=2,MZM)
      WRITE(IGRFIL,ERR=9010) ((DZBED(I,J),I=2,MXM),J=2,MYM)
      ENDIF
CD    -- 温度を出力 --
      IF (LTEMP.NE.0) THEN
        WRITE(IGRFIL,ERR=9010)
     $      (((REAL(TT(I,J,K)),I=2,MXM),J=2,MYM),K=2,MZM)
CDEBUG        DO 860 K=2,MZM
CDEBUG        DO 862 J=2,MYM
CDEBUG        DO 864 I=2,MXM
CDEBUG        WRITE(IGRFIL,*,ERR=9010) TT(I,J,K)
CDEBUG  864   CONTINUE
CDEBUG  862   CONTINUE
CDEBUG  860   CONTINUE
      ENDIF
CD    -- 濃度を出力 --
      IF (LCONC.NE.0) THEN
        WRITE(IGRFIL,ERR=9010)
     $      (((REAL(CC(I,J,K)),I=2,MXM),J=2,MYM),K=2,MZM)
CDEBUG        DO 870 K=2,MZM
CDEBUG        DO 872 J=2,MYM
CDEBUG        DO 874 I=2,MXM
CDEBUG        WRITE(IGRFIL,*,ERR=9010) CC(I,J,K)
CDEBUG  874   CONTINUE
CDEBUG  872   CONTINUE
CDEBUG  870   CONTINUE
      END IF

C     -- 実行文の終了 --
 9000 CONTINUE
      GOTO 9999

C==== ファイル関連エラー処理 =========================================

 9010 CONTINUE
      WRITE(*,*) 'DB_TRN WRITE ERROR (data.grp).'
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT( ' ','>> FILE-GRP : OUT : STEP=',I7,' : TIME= ',1PE12.5)

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
