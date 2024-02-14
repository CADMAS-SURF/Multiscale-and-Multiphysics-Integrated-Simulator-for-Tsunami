      SUBROUTINE MKDPRS(GV,GX,GY,GZ,GV0,GX0,GY0,GZ0,GVD,GXD,GYD,GZD,
     $                  CMD,CDD,COE1D,COE2D,INDP,INDU,INDV,INDW,
     $                  ICHILD,IWES,IEAS,JSOU,JNOR,KBOT,KTOP)
C======================================================================
C     透過性構造物用ポロシティを設定する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'OBSTI.h'
      INCLUDE 'OBSTR.h'
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'FILEI.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
C
C ... VISC: 水の動粘性係数(m2/s)
      REAL(8),PARAMETER:: VISC=1.0D-6
C
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GX(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GY(MX,MY,MZ),GZ(MX,MY,MZ)
      REAL(8),INTENT(OUT)::GV0(MX,MY,MZ),GX0(MX,MY,MZ)
      REAL(8),INTENT(OUT)::GY0(MX,MY,MZ),GZ0(MX,MY,MZ)
      REAL(8),INTENT(OUT)::GVD(MX,MY,MZ),GXD(MX,MY,MZ)
      REAL(8),INTENT(OUT)::GYD(MX,MY,MZ),GZD(MX,MY,MZ)
      REAL(8),INTENT(OUT)::CMD(MX,MY,MZ),CDD(MX,MY,MZ)
      REAL(8),INTENT(OUT)::COE1D(MX,MY,MZ),COE2D(MX,MY,MZ)
      INTEGER,INTENT(IN)::INDP(MX,MY,MZ),INDU(MX,MY,MZ)
      INTEGER,INTENT(IN)::INDV(MX,MY,MZ),INDW(MX,MY,MZ)
      INTEGER,INTENT(IN):: ICHILD,IWES,IEAS,JSOU,JNOR,KBOT,KTOP
C
      REAL(8),PARAMETER::GMIN=0.0D-4
      REAL(8)::XCM,XCD,XALPHA,XBETA,XDIAM
      REAL(8)::GV1,GX1,GY1,GZ1
      REAL(8),ALLOCATABLE::WORK(:,:,:)
      INTEGER::I,J,K,N,IS,IE,JS,JE,KS,KE,IERR,IRTN
      LOGICAL:: LNEST
C
C
      ALLOCATE(WORK(MX,MY,MZ),STAT=IERR)
      IF(IERR.NE.0) THEN
         CALL ERRMSG('MKDPRS',6930)
         WRITE(LP,*) 'CANNOT ALLOCATE WORK'
         CALL ABORT1('')
      ENDIF
C
      GV0(:,:,:)=GV(:,:,:)
      GX0(:,:,:)=GX(:,:,:)
      GY0(:,:,:)=GY(:,:,:)
      GZ0(:,:,:)=GZ(:,:,:)
C
C ... 透過性構造物用ポロシティ
      GVD(:,:,:)=1.0D0
      GXD(:,:,:)=1.0D0
      GYD(:,:,:)=1.0D0
      GZD(:,:,:)=1.0D0
C
C ... 係数
      CMD(:,:,:)=0.0D0
      CDD(:,:,:)=0.0D0
      COE1D(:,:,:)=0.0D0
      COE2D(:,:,:)=0.0D0
C
C ... 障害物データファイルを読み込む
      IF( LDPRS.EQ.1 ) THEN
         CFLNM(IFLNM-3:IFLNM) = '.dpr'
         OPEN(IFLDP,FILE=CFLNM(1:IFLNM),FORM='UNFORMATTED',STATUS='OLD')
         IF(IAUTOD.EQ.0)THEN         
            READ(IFLDP,ERR=910) (((GVD(I,J,K),I=2,MXM),J=2,MYM),K=2,MZM)
            READ(IFLDP,ERR=920) (((GXD(I,J,K),I=1,MXM),J=2,MYM),K=2,MZM)
            READ(IFLDP,ERR=930) (((GYD(I,J,K),I=2,MXM),J=1,MYM),K=2,MZM)
            READ(IFLDP,ERR=930) (((GZD(I,J,K),I=2,MXM),J=2,MYM),K=1,MZM)
            READ(IFLDP,ERR=940) (((CMD(I,J,K),I=2,MXM),J=2,MYM),K=2,MZM)
            READ(IFLDP,ERR=940) (((CDD(I,J,K),I=2,MXM),J=2,MYM),K=2,MZM)
           READ(IFLDP,ERR=940)(((COE1D(I,J,K),I=2,MXM),J=2,MYM),K=2,MZM)
           READ(IFLDP,ERR=940)(((COE2D(I,J,K),I=2,MXM),J=2,MYM),K=2,MZM)
           READ(IFLDP,ERR=940) (((WORK(I,J,K),I=2,MXM),J=2,MYM),K=2,MZM)
         ELSE
            CALL RDSUB3RB(GVD,IFLDP,0,IRTN)
            IF(IRTN.EQ.1) GOTO 910
            CALL RDSUB3RB(GXD,IFLDP,1,IRTN)
            IF(IRTN.EQ.1) GOTO 920
            CALL RDSUB3RB(GYD,IFLDP,2,IRTN)
            IF(IRTN.EQ.1) GOTO 930
            CALL RDSUB3RB(GZD,IFLDP,3,IRTN)
            IF(IRTN.EQ.1) GOTO 930
            CALL RDSUB3RB(CMD,IFLDP,0,IRTN)
            IF(IRTN.EQ.1) GOTO 940
            CALL RDSUB3RB(CDD,IFLDP,0,IRTN)
            IF(IRTN.EQ.1) GOTO 940
            CALL RDSUB3RB(COE1D,IFLDP,0,IRTN)
            IF(IRTN.EQ.1) GOTO 940
            CALL RDSUB3RB(COE2D,IFLDP,0,IRTN)
            IF(IRTN.EQ.1) GOTO 940
            CALL RDSUB3RB(WORK,IFLDP,0,IRTN)
            IF(IRTN.EQ.1) GOTO 940
         ENDIF
         CLOSE(IFLDP)
C
         DO 300 K=2,MZM
         DO 300 J=2,MYM
         DO 300 I=2,MXM
            GV(I,J,K) = GV0(I,J,K)*GVD(I,J,k)
            IF( GVD(I,J,K).EQ.1.0D0 ) THEN
               CMD(I,J,K)=0.0D0
               CDD(I,J,K)=0.0D0
               COE1D(I,J,K)=0.0D0
               COE2D(I,J,K)=0.0D0
            ELSE
               COE1D(I,J,K)=COE1D(I,J,K)*(1.0D0-GVD(I,J,K))**3.0D0
     $                     /GVD(I,J,K)**2.0D0*VISC
     $                     /MAX(WORK(I,J,K),1.0D-3)**2.0D0
               COE2D(I,J,K)=COE2D(I,J,K)*(1.0D0-GVD(I,J,K))
     $                     /GVD(I,J,K)**3.0D0
     $                     /MAX(WORK(I,J,K),1.0D-3)
            ENDIF
  300    CONTINUE
C
         DO 310 K=2,MZM
         DO 310 J=2,MYM
         DO 310 I=1,MXM
            GX(I,J,K) = GX0(I,J,K)*GXD(I,J,K)
  310    CONTINUE
C
         DO 320 K=2,MZM
         DO 320 J=1,MYM
         DO 320 I=2,MXM
            GY(I,J,K) = GY0(I,J,K)*GYD(I,J,K)
  320    CONTINUE
C
         DO 330 K=1,MZM
         DO 330 J=2,MYM
         DO 330 I=2,MXM
            GZ(I,J,K) = GZ0(I,J,K)*GZD(I,J,K)
  330    CONTINUE
C
      ELSE
      DO 100 N=1,NPORS
         IS = IPORS(1,N)
         IE = IPORS(2,N)
         JS = IPORS(3,N)
         JE = IPORS(4,N)
         KS = IPORS(5,N)
         KE = IPORS(6,N)
         IF(IPORS(7,N).NE.2) GO TO 100
C
         XCM= RPORS(11,N)
         XCD= RPORS(12,N)
         XALPHA= RPORS(13,N)
         XBETA = RPORS(14,N)
         XDIAM = RPORS(15,N)
C
         DO 110 K=KS,KE
         DO 110 J=JS,JE
         DO 110 I=IS,IE
            GVD(I,J,K)= RPORS(1,N)
            GV(I,J,K) = GV0(I,J,K)*GVD(I,J,k)
            CMD(I,J,K)=XCM
            CDD(I,J,K)=XCD
            COE1D(I,J,K)=XALPHA*(1.0D0-GVD(I,J,K))**3.0D0
     $                  /GVD(I,J,K)**2.0D0*VISC
     $                  /XDIAM**2.0D0
            COE2D(I,J,K)=XBETA*(1.0D0-GVD(I,J,K))
     $                  /GVD(I,J,K)**3.0D0
     $                  /XDIAM
  110    CONTINUE
C
         DO 120 K=KS,KE
         DO 120 J=JS,JE
         DO 120 I=IS-1,IE
            GXD(I,J,K)= RPORS(3,N)
            IF(I.EQ.IS-1) GXD(I,J,K)= RPORS(2,N)
            IF(I.EQ.IE  ) GXD(I,J,K)= RPORS(4,N)
            GX(I,J,K) = GX0(I,J,K)*GXD(I,J,K)
  120    CONTINUE
C
         DO 130 K=KS,KE
         DO 130 J=JS-1,JE
         DO 130 I=IS,IE
            GYD(I,J,K)= RPORS(6,N)
            IF(J.EQ.JS-1) GYD(I,J,K)= RPORS(5,N)
            IF(J.EQ.JE  ) GYD(I,J,K)= RPORS(7,N)
            GY(I,J,K) = GY0(I,J,K)*GYD(I,J,K)
  130    CONTINUE
C
         DO 140 K=KS-1,KE
         DO 140 J=JS,JE
         DO 140 I=IS,IE
            GZD(I,J,K)= RPORS(9,N)
            IF(K.EQ.KS-1) GZD(I,J,K)= RPORS(8,N)
            IF(K.EQ.KE  ) GZD(I,J,K)= RPORS(10,N)
            GZ(I,J,K) = GZ0(I,J,K)*GZD(I,J,K)
  140    CONTINUE
  100 CONTINUE
C
      ENDIF
C
C
C ... ポロシティのチェック
      IERR = 0
      DO 200 K=1,MZ
      DO 200 J=1,MY
      DO 200 I=1,MX
C ...... 流体セル
         IF( INDP(I,J,K).GT.0 ) THEN
            IF( GV(I,J,K).LT.GMIN ) THEN
               CALL ERRMSG('MKDPRS',6931)
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
     $            K.NE.1.AND.K.NE.MZ.AND.LNEST ) THEN
                  CALL ERRMS2('MKDPRS',6932)
                  WRITE(LP,*) 'POROUS VALUE IS SET IN OBSTACLE'
                  WRITE(LP,*) ' GV   =',GV(I,J,K)
                  WRITE(LP,*) ' I,J,K=',I,J,K
               ENDIF
C               IERR = 0
               GV(I,J,K)  =1.0D0
               GVD(I,J,K) =1.0D0
   11          GV0(I,J,K) =1.0D0
CDEBUG               IF(I.NE.1) THEN
CDEBUG                  GX(I-1,J,K)=1.0D0
CDEBUG                  GXCM(I-1,J,K)=1.0D0
CDEBUG                  GX0(I-1,J,K)=1.0D0
CDEBUG               ENDIF
CDEBUG               GX(I  ,J,K)=1.0D0
CDEBUG               GXCM(I  ,J,K)=1.0D0
CDEBUG               GX0(I  ,J,K)=1.0D0
CDEBUG               IF(J.NE.1) THEN
CDEBUG                  GY(I,J-1,K)=1.0D0
CDEBUG                  GYCM(I,J-1,K)=1.0D0
CDEBUG                  GY0(I,J-1,K)=1.0D0
CDEBUG               ENDIF
CDEBUG               GY(I  ,J,K)=1.0D0
CDEBUG               GYCM(I  ,J,K)=1.0D0
CDEBUG               GY0(I  ,J,K)=1.0D0
CDEBUG               IF(K.NE.1) THEN
CDEBUG                  GZ(I,J,K-1)=1.0D0
CDEBUG                  GZCM(I,J,K-1)=1.0D0
CDEBUG                  GZ0(I,J,K-1)=1.0D0
CDEBUG               ENDIF
CDEBUG               GZ(I  ,J,K)=1.0D0
CDEBUG               GZCM(I  ,J,K)=1.0D0
CDEBUG               GZ0(I  ,J,K)=1.0D0
            END IF
         END IF
C 壁面の透過率は1にしておく。
         IF( INDU(I,J,K).LE.-2 ) THEN
            GX(I,J,K)=1.0D0
            GX0(I,J,K)=1.0D0
            GXD(I,J,K)=1.0D0
         ENDIF
         IF( INDV(I,J,K).LE.-2 ) THEN
            GY(I,J,K)=1.0D0
            GY0(I,J,K)=1.0D0
            GYD(I,J,K)=1.0D0
         ENDIF
         IF( INDW(I,J,K).LE.-2 ) THEN
            GZ(I,J,K)=1.0D0
            GZ0(I,J,K)=1.0D0
            GZD(I,J,K)=1.0D0
         ENDIF
  200 CONTINUE
C
      IF( IERR.EQ.1 ) THEN
         CALL ABORT1('')
      ENDIF
C
      DEALLOCATE(WORK)
      RETURN
C
C ... ファイル読み込みエラー
  910 CONTINUE
      CALL ERRMSG('MKDPRS',6933)
      WRITE(LP,*) 'FILE READ ERROR: D-POROUS FILE'
      WRITE(LP,*) 'VARIABLE=GV'
      WRITE(LP,*) 'FILE NUMBER=',IFLDP
      CALL ABORT1('')
C
  920 CONTINUE
      CALL ERRMSG('MKDPRS',6934)
      WRITE(LP,*) 'FILE READ ERROR: D-POROUS FILE'
      WRITE(LP,*) 'VARIABLE=GX'
      WRITE(LP,*) 'FILE NUMBER=',IFLDP
      CALL ABORT1('')
C
  930 CONTINUE
      CALL ERRMSG('MKDPRS',6935)
      WRITE(LP,*) 'FILE READ ERROR: D-POROUS FILE'
      WRITE(LP,*) 'VARIABLE=GY'
      WRITE(LP,*) 'FILE NUMBER=',IFLDP
      CALL ABORT1('')
C
  940 CONTINUE
      CALL ERRMSG('MKDPRS',6936)
      WRITE(LP,*) 'FILE READ ERROR: D-POROUS FILE'
      WRITE(LP,*) 'VARIABLE=GZ'
      WRITE(LP,*) 'FILE NUMBER=',IFLDP
      CALL ABORT1('')
      END
