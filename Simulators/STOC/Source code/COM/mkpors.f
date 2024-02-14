      SUBROUTINE MKPORS(GV,GX,GY,GZ,FRIC,AMNG,INDP,INDU,INDV,INDW,
     $                  ICHILD,IWES,IEAS,JSOU,JNOR,KBOT,KTOP)
C======================================================================
C     ポロシティを設定する
C     マニングの粗度係数を設定する
C======================================================================
      IMPLICIT NONE
C
C@@   PARAMETER ( GMIN = 0.1D0 )
      REAL(8),PARAMETER::GMIN=0.0D-4
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
      INCLUDE 'OBSTI.h'
      INCLUDE 'OBSTR.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
C
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GX(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GY(MX,MY,MZ),GZ(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::FRIC(MX,MY,MZ),AMNG(MX,MY)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),INDU(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDV(MX,MY,MZ),INDW(MX,MY,MZ)
      INTEGER,INTENT(IN):: ICHILD,IWES,IEAS,JSOU,JNOR,KBOT,KTOP
C
      INTEGER::I,IE,IERR,IKK1,IS,J,JE,JS,K,KE,KS,N,IRTN
      LOGICAL:: LNEST
C
C
C ... 配列を1.0で初期化
      CALL ZERCLR(GV,MXYZ,1.0D0)
      CALL ZERCLR(GX,MXYZ,1.0D0)
      CALL ZERCLR(GY,MXYZ,1.0D0)
      CALL ZERCLR(GZ,MXYZ,1.0D0)
C
      CALL ZERCLR(FRIC,MXYZ,0.0D0)
      CALL ZERCLR(AMNG,MXY ,0.0D0)
C
C ... 障害物データファイルを読み込む
      IF( LOBST.EQ.1 ) THEN
         WRITE(LP,*) 'READING POROSITY DATA'
         IKK1 = MZM/2
C 20110907 for Hitachi FORTRAN
         READ(IFLST,ERR=910) ! 読み飛ばし NX1,NY1,NZ1
         READ(IFLST,ERR=910) ! 読み飛ばし INDC
C 20110907 for Hitachi FORTRAN
         IF(IAUTOD.EQ.0)THEN
            READ(IFLST,ERR=910) (((GV(I,J,K),I=2,MXM),J=2,MYM),K=2,MZM)
            READ(IFLST,ERR=920) (((GX(I,J,K),I=1,MXM),J=2,MYM),K=2,MZM)
            READ(IFLST,ERR=930) (((GY(I,J,K),I=2,MXM),J=1,MYM),K=2,MZM)
            READ(IFLST,ERR=940) (((GZ(I,J,K),I=2,MXM),J=2,MYM),K=1,MZM)
C
         ELSE
            CALL RDSUB3RB(GV,IFLST,0,IRTN)
            IF(IRTN.EQ.1) GOTO 910
            CALL RDSUB3RB(GX,IFLST,1,IRTN)
            IF(IRTN.EQ.1) GOTO 920
            CALL RDSUB3RB(GY,IFLST,2,IRTN)
            IF(IRTN.EQ.1) GOTO 930
            CALL RDSUB3RB(GZ,IFLST,3,IRTN)
            IF(IRTN.EQ.1) GOTO 940
         ENDIF
C
         DO 80 K=2,MZM
         DO 80 J=2,MYM
         DO 80 I=2,MXM
           IF(GX(I,J,K).EQ.0.0D0) GX(I,J,K)=1.0D0
           IF(GY(I,J,K).EQ.0.0D0) GY(I,J,K)=1.0D0
   80    CONTINUE
C
C324     CLOSE(IFLST)
C@@@@@@@@@@@@@@@@@@
         DO 90 K=2,MZM
         DO 90 J=2,MYM
           IF (NSOMER(2).GT.0) GV(2    ,J,K)=GV(3    ,J,K)
           IF (NSOMER(3).GT.0) GV(MXM  ,J,K)=GV(MXM-1,J,K)
 90      CONTINUE
         DO 91 K=2,MZM
         DO 91 I=2,MXM
           IF (NSOMER(1).GT.0) GV(I,2    ,K)=GV(I,3    ,K)
           IF (NSOMER(4).GT.0) GV(I,MYM  ,K)=GV(I,MYM-1,K)
 91      CONTINUE
         DO 92 K=2,MZM
         DO 92 J=2,MYM
           IF (NSOMER(2).GT.0) GX(1    ,J,K)=GV(3    ,J,K)
           IF (NSOMER(2).GT.0) GX(2    ,J,K)=GV(3    ,J,K)
           IF (NSOMER(3).GT.0) GX(MXM-1,J,K)=GV(MXM-1,J,K)
           IF (NSOMER(3).GT.0) GX(MXM  ,J,K)=GV(MXM-1,J,K)
 92      CONTINUE
         DO 93 K=2,MZM
         DO 93 I=1,MXM
           IF (NSOMER(1).GT.0) GX(I,2    ,K)=GX(I,3    ,K)
           IF (NSOMER(4).GT.0) GX(I,MYM  ,K)=GX(I,MYM-1,K)
 93      CONTINUE
         DO 94 K=2,MZM
         DO 94 I=2,MXM
           IF (NSOMER(1).GT.0) GY(I,1    ,K)=GV(I,3    ,K)
           IF (NSOMER(1).GT.0) GY(I,2    ,K)=GV(I,3    ,K)
           IF (NSOMER(4).GT.0) GY(I,MYM-1,K)=GV(I,MYM-1,K)
           IF (NSOMER(4).GT.0) GY(I,MYM  ,K)=GV(I,MYM-1,K)
 94      CONTINUE
         DO 95 K=2,MZM
         DO 95 J=1,MYM
           IF (NSOMER(2).GT.0) GY(2    ,J,K)=GY(3    ,J,K)
           IF (NSOMER(3).GT.0) GY(MXM  ,J,K)=GY(MXM-1,J,K)
 95      CONTINUE
         DO 96 K=1,MZM
         DO 96 J=2,MYM
           IF (NSOMER(2).GT.0) GZ(2    ,J,K)=GZ(3    ,J,K)
           IF (NSOMER(3).GT.0) GZ(MXM  ,J,K)=GZ(MXM-1,J,K)
 96      CONTINUE
         DO 97 K=1,MZM
         DO 97 I=2,MXM
           IF (NSOMER(1).GT.0) GZ(I,2    ,K)=GZ(I,3    ,K)
           IF (NSOMER(4).GT.0) GZ(I,MYM  ,K)=GZ(I,MYM-1,K)
 97      CONTINUE
C@@@@@@@@@@@@@@@@@@
      END IF
C
C ... 解析条件入力データファイル内で指定した障害物データを設定する
C
      DO 100 N=1,NPORS
         IS = IPORS(1,N)
         IE = IPORS(2,N)
         JS = IPORS(3,N)
         JE = IPORS(4,N)
         KS = IPORS(5,N)
         KE = IPORS(6,N)
         IF(IPORS(7,N).NE.0) GO TO 100
C
         IF(IS.LT.2.OR.IE.GT.MXM.OR.
     &      JS.LT.2.OR.JE.GT.MYM.OR.
     &      KS.LT.2.OR.KE.GT.MZM) THEN
           WRITE(LP,60) (IPORS(I,N),I=1,6)
   60      FORMAT('VARIABLE=POROUS (IS,IE,JS,JE,KS,KE)=',6I5)
           CALL ERRMS2('MKPORS',7050)
           GO TO 100     
         END IF 
C
         DO 110 K=KS,KE
         DO 110 J=JS,JE
         DO 110 I=IS,IE
            GV(I,J,K) = RPORS(1,N)
  110    CONTINUE
C
         DO 120 K=KS,KE
         DO 120 J=JS,JE
            GX(IS-1,J,K) = RPORS(2,N)
            DO 125 I=IS,IE-1
               GX(I,J,K) = RPORS(3,N)
  125       CONTINUE
            GX(IE,J,K)   = RPORS(4,N)
  120    CONTINUE
C
         DO 130 K=KS,KE
         DO 130 I=IS,IE
            GY(I,JS-1,K) = RPORS(5,N)
            DO 135 J=JS,JE-1
               GY(I,J,K) = RPORS(6,N)
  135       CONTINUE
            GY(I,JE,K)   = RPORS(7,N)
  130    CONTINUE
C
         DO 140 J=JS,JE
         DO 140 I=IS,IE
            GZ(I,J,KS-1) = RPORS(8,N)
            DO 145 K=KS,KE-1
               GZ(I,J,K) = RPORS(9,N)
  145       CONTINUE
            GZ(I,J,KE)   = RPORS(10,N)
  140    CONTINUE
  100 CONTINUE
C
C ... 浮上型防波堤データとして入力したポーラス値はGV,GZのみ設定する
C     -ポーラス値を地形データと合成する(INDP(I,J,KS-1).EQ.0:着底)-
C
      IF(LFOBS.NE.0) THEN
      DO 105 N=1,NPORS
         IS = IPORS(1,N)
         IE = IPORS(2,N)
         JS = IPORS(3,N)
         JE = IPORS(4,N)
         KS = IPORS(5,N)
         KE = IPORS(6,N)
         IF(IPORS(7,N).EQ.0) GO TO 105
C
         IF(IS.LT.2.OR.IE.GT.MXM.OR.
     &      JS.LT.2.OR.JE.GT.MYM.OR.
     &      KS.LT.2.OR.KE.GT.MZM) THEN
           WRITE(LP,60) (IPORS(I,N),I=1,6)
           CALL ERRMS2('MKPORS',7051)
           GO TO 105     
         END IF 
C
         DO 115 K=KS,KE
         DO 115 J=JS,JE
         DO 115 I=IS,IE
            IF( INDP(I,J,K-1).EQ.1) THEN
              GV(I,J,K) = GV(I,J,K)+RPORS(1,N)-1.0D0
            ELSE
              GV(I,J,K) = GV(I,J,K)*RPORS(1,N)
            END IF
  115    CONTINUE
C
         DO 126 J=JS,JE
         DO 126 I=IS,IE
            IF( INDP(I,J,KS-1).EQ.1) THEN
               GZ(I,J,KS-1) = RPORS(8,N)
            ELSE
               GZ(I,J,KS-1) = 1.0D0
            END IF
            DO 136 K=KS,KE-1
               GZ(I,J,K) = GZ(I,J,K)+RPORS(9,N)-1.0D0
  136       CONTINUE
            IF( INDP(I,J,KE+1).EQ.1) THEN
               GZ(I,J,KE) = GZ(I,J,KE)+RPORS(10,N)-1.0D0
            ELSE 
               GZ(I,J,KE) = 1.0D0
            END IF
  126    CONTINUE
  105 CONTINUE
      END IF
C
C ... 解析条件入力データファイル内で指定した抵抗データを設定する
      DO 150 N=1,NFRIC
         IS = IFRIC(1,N)
         IE = IFRIC(2,N)
         JS = IFRIC(3,N)
         JE = IFRIC(4,N)
         KS = IFRIC(5,N)
         KE = IFRIC(6,N)
C
         DO 160 K=KS,KE
         DO 160 J=JS,JE
         DO 160 I=IS,IE
            FRIC(I,J,K) = RFRIC(N)
  160    CONTINUE
  150 CONTINUE
C
         DO 170 N=1,NFN
C
            IS = IFNTBL(1,N)
            IE = IFNTBL(2,N)
            JS = IFNTBL(3,N)
            JE = IFNTBL(4,N)
            IF( IS.EQ.0 ) THEN
               IS = 2
               IE = MXM
               JS = 2
               JE = MYM
            END IF
C
            DO 180 J=JS,JE
            DO 185 I=IS,IE
              AMNG(I,J) = FNVAL(N)
  185       CONTINUE
  180       CONTINUE
  170    CONTINUE
C
C ... ポロシティのチェック
      IERR = 0
      DO 200 K=1,MZ
      DO 200 J=1,MY
      DO 200 I=1,MX
C ...... 流体セル
         IF( INDP(I,J,K).GT.0 ) THEN
            IF( GV(I,J,K).LT.GMIN ) THEN
               CALL ERRMSG('MKPORS',7052)
               WRITE(LP,*) 'POROUS VALUE IS TOO SMALL'
               WRITE(LP,*) ' GV   =',GV(I,J,K)
               WRITE(LP,*) ' I,J,K=',I,J,K
               IERR = 1
            END IF
         ELSE
C ......... 構造物セルのポロシティは1.0としておく
            IF( GV(I,J,K).NE.1.0D0 ) THEN
               LNEST=(I.GE.IWES+NESML(2)+1).AND.(I.LE.IEAS-NESML(3)-1)
     $          .AND.(J.GE.JSOU+NESML(1)+1).AND.(J.LE.JNOR-NESML(4)-1)
     $          .AND.(K.GE.KBOT).AND.(K.LE.KTOP)
               LNEST=.NOT.LNEST
C
               IF(I.NE.1.AND.I.NE.MX.AND.J.NE.1.AND.J.NE.MY.AND.
     $            K.NE.1.AND.K.NE.MZ.AND.LNEST) THEN
C                 GV=0 は ignore
                  IF( GV(I,J,K).NE.0.0D0 ) THEN
                  CALL ERRMS2('MKPORS',7053)
                  WRITE(LP,*) 'POROUS VALUE IS SET IN OBSTACLE'
                  WRITE(LP,*) ' GV   =',GV(I,J,K)
                  WRITE(LP,*) ' I,J,K=',I,J,K
                  ENDIF
               ENDIF
C               IERR = 0
               GV(I,J,K)  =1.0D0
CDEBUG               IF(I.NE.1) GX(I-1,J,K)=1.0D0
CDEBUG               GX(I  ,J,K)=1.0D0
CDEBUG               IF(J.NE.1) GY(I,J-1,K)=1.0D0
CDEBUG               GY(I  ,J,K)=1.0D0
CDEBUG               IF(K.NE.1) GZ(I,J,K-1)=1.0D0
CDEBUG               GZ(I  ,J,K)=1.0D0
            END IF
         END IF
C 壁面の透過率は1にしておく。
         IF( INDU(I,J,K).LE.-2 ) GX(I,J,K)=1.0D0
         IF( INDV(I,J,K).LE.-2 ) GY(I,J,K)=1.0D0
         IF( INDW(I,J,K).LE.-2 ) GZ(I,J,K)=1.0D0
  200 CONTINUE
C
C ... 海底セルのポロシティをコピーする
      DO 300 K=1,MZ
C
      DO 310 J=1,MY
        IF( GX(1,J,K).NE.1.0D0.AND.GX(1,J,K).EQ.GV(2,J,K) ) THEN
          GV(1,J,K) = GV(2,J,K)
        END IF
        IF( GX(MXM,J,K).NE.1.0D0.AND.GX(MXM,J,K).EQ.GV(MXM,J,K) ) THEN
          GV(MX,J,K) = GV(MXM,J,K)
        END IF
  310 CONTINUE
C
      DO 320 I=1,MX
        IF( GY(I,1,K).NE.1.0D0.AND.GY(I,1,K).EQ.GV(I,2,K) ) THEN
          GV(I,1,K) = GV(I,2,K)
        END IF
        IF( GY(I,MYM,K).NE.1.0D0.AND.GY(I,MYM,K).EQ.GV(I,MYM,K) ) THEN
          GV(I,MY,K) = GV(I,MYM,K)
        END IF
  320 CONTINUE
  300 CONTINUE
C
      IF( IERR.EQ.1 ) THEN
         CALL ABORT1('')
      ENDIF
C
      RETURN
C
C ... ファイル読み込みエラー
  910 CONTINUE
      CALL ERRMSG('MKPORS',7054)
      WRITE(LP,*) 'FILE READ ERROR: STRUCTURE FILE'
      WRITE(LP,*) 'VARIABLE=GV'
      WRITE(LP,*) 'FILE NUMBER=',IFLST
      CALL ABORT1('')
C
  920 CONTINUE
      CALL ERRMSG('MKPORS',7055)
      WRITE(LP,*) 'FILE READ ERROR: STRUCTURE FILE'
      WRITE(LP,*) 'VARIABLE=GX'
      WRITE(LP,*) 'FILE NUMBER=',IFLST
      CALL ABORT1('')
C
  930 CONTINUE
      CALL ERRMSG('MKPORS',7056)
      WRITE(LP,*) 'FILE READ ERROR: STRUCTURE FILE'
      WRITE(LP,*) 'VARIABLE=GY'
      WRITE(LP,*) 'FILE NUMBER=',IFLST
      CALL ABORT1('')
C
  940 CONTINUE
      CALL ERRMSG('MKPORS',7057)
      WRITE(LP,*) 'FILE READ ERROR: STRUCTURE FILE'
      WRITE(LP,*) 'VARIABLE=GZ'
      WRITE(LP,*) 'FILE NUMBER=',IFLST
      CALL ABORT1('')
      END
C     メモ
C
C     ポロシティのチェックを面透過率についても行う。
