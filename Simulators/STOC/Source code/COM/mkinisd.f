      SUBROUTINE MKINISD(CSEDI,CSEDIN,CSDAVE,SHLSD,USSD,WEXSD,
     $                  EXSDE,EXSDD,ZBED,ZBEDN,ZBED0,QBX,QBY,DZBUF,AMNG)
C======================================================================
C     土砂移動データの初期化およびパラメータの設定
C       GRAV ：重力加速度(m/s2)
C       SSAND：砂の水中比重(m2/s)
C       DSAND：砂の粒径(m)
C       SDNU ：流体の動粘性係数
C     ①WSEDI：沈降速度(m/s)
C     ②PSIC ：限界シールズ数
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
      INCLUDE 'FILEI.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'SEDIMENT.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
C
      REAL(8),INTENT(OUT)::CSEDI(MX,MY,MZ),CSEDIN(MX,MY,MZ)
      REAL(8),INTENT(OUT)::CSDAVE(MX,MY)
      REAL(8),INTENT(OUT)::WEXSD(MX,MY)
      REAL(8),INTENT(OUT)::SHLSD(MX,MY),USSD(MX,MY)
      REAL(8),INTENT(OUT)::EXSDE(MX,MY),EXSDD(MX,MY)
      REAL(8),INTENT(OUT)::ZBED(MX,MY),ZBEDN(MX,MY),ZBED0(MX,MY)
      REAL(8),INTENT(OUT)::QBX(MX,MY),QBY(MX,MY),DZBUF(MX,MY)
      REAL(8),INTENT(IN) ::AMNG(MX,MY)
C
      REAL(8)::DN,S,A
      REAL(8)::RS,SS
      REAL(8),PARAMETER::EPS=1.0D-10
      INTEGER::I,J
      INTEGER::IFLAG
      INTEGER::NX1,NY1,IRTN
C
C======================================================================
C     土砂移動データを初期化する
C======================================================================
      CSEDI  = 0.0D0
      CSEDIN = 0.0D0
      CSDAVE = 0.0D0
      WEXSD  = 0.0D0
      SHLSD  = 0.0D0
      USSD   = 0.0D0
      EXSDE  = 0.0D0
      EXSDD  = 0.0D0
      QBX    = 0.0D0
      QBY    = 0.0D0
      DZBUF  = 0.0D0
C
C======================================================================
C     初期掃流砂厚さを設定する
C======================================================================
C ... 初期掃流砂厚さを一様に設定
      IF(IBEDINI.EQ.0)THEN
         IF(BEDINI.LT.0.0D0)THEN
            CALL ERRMSG('MKINISD',7010)
            WRITE(LP,*) 'BEDINI SHOULD BE >=0'
            WRITE(LP,*) '  BEDINI=',BEDINI
            CALL ABORT1('')
         ENDIF
         ZBED0=BEDINI
C ... 初期掃流砂厚さを入力ファイルから設定
      ELSEIF(IBEDINI.EQ.1)THEN
         CFLNM(IFLNM-3:IFLNM) = '.zbd'
         OPEN(IFLZB,FILE=CFLNM(1:IFLNM),STATUS='OLD',FORM='FORMATTED',
     $        ERR=900)
         WRITE(LP,*) 'READING ZBED DATA'
         READ(IFLZB,*,ERR=910) NX1,NY1
         IF(IAUTOD.EQ.0)THEN
         IF( NX1.NE.MX-2 .OR. NY1.NE.MY-2 )THEN
            CALL ERRMSG('MKINISD',7011)
            WRITE(LP,*) 'MESH NUMBER IS DIFFERENT FROM INPUT DATA'
            WRITE(LP,*) 'NX,NY=',NX1,NY1,' IN ZBED FILE'
            WRITE(LP,*) '     =',MX-2,MY-2,' IN GRID DATA'
            CALL ABORT1('')
         ENDIF
         ELSE
         IF( NX1.NE.MXG-2 .OR. NY1.NE.MYG-2 )THEN
            CALL ERRMSG('MKINISD',7012)
            WRITE(LP,*) 'MESH NUMBER IS DIFFERENT FROM INPUT DATA'
            WRITE(LP,*) 'NX,NY=',NX1,NY1,' IN ZBED FILE'
            WRITE(LP,*) '     =',MXG-2,MYG-2,' IN GRID DATA'
            CALL ABORT1('')
         ENDIF
         ENDIF
         IF(IAUTOD.EQ.0)THEN
            DO J=MYM,2,-1
               READ(IFLZB,*,ERR=910) (ZBED0(I,J),I=2,MXM)
            ENDDO
         ELSE
            CALL RDSUB2RAZ(ZBED0,IFLZB,0,IRTN)
            IF(IRTN.EQ.1) GOTO 910
         ENDIF
         DO J=2,MYM
         DO I=2,MXM
            IF(ZBED0(I,J).LT.0.0D0)THEN
               CALL ERRMSG('MKINISD',7013)
               WRITE(LP,*) 'ZBED0 SHOULD BE >=0'
               WRITE(LP,*) '  ZBED0=',ZBED0(I,J)
               WRITE(LP,*) '  I,J  =',I,J
               CALL ABORT1('')
            ENDIF
         ENDDO
         ENDDO
         CLOSE(IFLZB)
C ...... 境界での掃流砂厚さ勾配がゼロになるように仮想セルの初期値を設定
         DO I=2,MXM
            ZBED0(I, 1)=ZBED0(I,  2)
            ZBED0(I,MY)=ZBED0(I,MYM)
         ENDDO
         DO J=2,MYM
            ZBED0( 1,J)=ZBED0(  2,J)
            ZBED0(MX,J)=ZBED0(MXM,J)
         ENDDO
      ENDIF
      ZBED   = ZBED0
      ZBEDN  = ZBED0
C
C======================================================================
C     入力フラグの組み合わせを確認する
C======================================================================
      IF(MWEXSD.EQ.0 .AND. MCONCSD.NE.0)THEN
         CALL ERRMS2('MKINISD',7014)
         WRITE(LP,*) '  INPUT %SEDIMENT BLOCK'
         WRITE(LP,*) '  MODEL-SEDI-CONC SHOULD BE AVERAGE'
         WRITE(LP,*) '  WHEN MODEL-EXCHANGE = TAKAHASHI'
      ELSEIF(MWEXSD.EQ.1 .AND. MCONCSD.EQ.0)THEN
         CALL ERRMS2('MKINISD',7015)
         WRITE(LP,*) '  INPUT %SEDIMENT BLOCK'
         WRITE(LP,*) '  MODEL-SEDI-CONC SHOULD BE BOTTOM OR FUJII'
         WRITE(LP,*) '  WHEN MODEL-EXCHANGE = IKENO'
      ENDIF
C
      IF(MFDBCKSD.EQ.1 .AND. MOFFLNSD.EQ.1)THEN
         CALL ERRMSG('MKINISD',7016)
         WRITE(LP,*) 'ERROR: INPUT %SEDIMENT BLOCK'
         WRITE(LP,*) '  FEEDBACK SHOULD BE OFF WHEN OFFLINE CALC'
         CALL ABORT1('')
      ENDIF
C
      IF(MWEXSD.EQ.0 .AND. MBDSLP.EQ.1)THEN
         CALL ERRMSG('MKINISD',7017)
         WRITE(LP,*) 'ERROR: INPUT %SEDIMENT BLOCK'
         WRITE(LP,*) '  MODEL-EXCHANGE SHOULD BE IKENO'
         WRITE(LP,*) '  WHEN MODEL-BED-SLOPE = ON'
         CALL ABORT1('')
      ENDIF
C
C======================================================================
C     土砂移動計算でマニングの粗度係数を用いる場合の入力確認
C======================================================================
      IF(MRGHSD.EQ.3 .OR. MUSTSD.EQ.1)THEN
         IFLAG=0
         DO 100 J=2,MYM
         DO 100 I=2,MXM
            IF(AMNG(I,J).GT.EPS) IFLAG=1
  100    CONTINUE
         IF(IFLAG.EQ.0)THEN
            CALL ERRMSG('MKINISD',7018)
            WRITE(LP,*) 'MANNING ROUGHNESS COEFFICIENT SHOULD BE INPUT'
            CALL ABORT1('')
         ENDIF
      ENDIF
C
C======================================================================
C     浮遊砂の乱流拡散係数を修正する
C======================================================================
      IF(MDIFSD.EQ.1)THEN
         DIFHSD=ANUH/SCTHSD
         DIFVSD=ANUV/SCTVSD
      ENDIF
C
C======================================================================
C     ①浮遊砂の沈降速度WSEDIを設定する
C       MSETSD=0：Jimenez and Madsen,2003
C       MSETSD=1：Rubey,1993
C       MSETSD=2：Ahrens,2000
C       MSETSD=3：Soulsby,1998
C======================================================================
      IF(MSETSD.EQ.0)THEN
         DN=DSAND/0.9D0
         S=DN/(4.0D0*SDNU)*SQRT(SSAND*ABS(GRAV)*DN)
         WSEDI=SQRT(SSAND*ABS(GRAV)*DN)/(0.954D0+5.12d0/S)
      ELSEIF(MSETSD.EQ.1)THEN
         A=SSAND*ABS(GRAV)*DSAND**3/SDNU**2
         WSEDI=(SQRT(2.0D0/3.0D0+36.0D0/A)-SQRT(36.0D0/A))
     $         *SQRT(SSAND*ABS(GRAV)*DSAND)
      ELSEIF(MSETSD.EQ.2)THEN
         A=SSAND*ABS(GRAV)*DSAND**3/SDNU**2
         WSEDI=SDNU/DSAND
     $         *(SQRT(13.03D0+1.18D0*A**0.654D0)-3.61D0)**1.53D0
      ELSEIF(MSETSD.EQ.3)THEN
         A=SSAND*ABS(GRAV)*DSAND**3/SDNU**2
         WSEDI=SDNU/DSAND*(SQRT(10.36D0**2+1.049D0*A)-10.36D0)
      ENDIF
C
C======================================================================
C     ②限界シールズ数PSICを設定する
C       MSHLSD=0：岩垣公式
C       MSHLSD=1：Soulsby and Whitehouse,1997
C======================================================================
      IF(MSHLSD.EQ.0)THEN
         RS=SQRT(SSAND*ABS(GRAV)*DSAND**3)/SDNU
         IF(RS.GE.671.0D0)THEN
            PSIC=0.05D0
         ELSEIF(RS.GE.162.7D0)THEN
            PSIC=0.00849D0*RS**(3.0D0/11.0D0)
         ELSEIF(RS.GE.54.2D0)THEN
            PSIC=0.034D0
         ELSEIF(RS.GE.2.14D0)THEN
            PSIC=0.195D0*RS**(-7.0D0/16.0D0)
         ELSE
            PSIC=0.14D0
         ENDIF
      ELSEIF(MSHLSD.EQ.1)THEN
         SS=DSAND/(4.0D0*SDNU)*SQRT(SSAND*ABS(GRAV)*DSAND)
         IF(SS.LT.1.53D0)THEN
            PSIC=0.085D0*SS**(-2.0D0/7.0D0)
         ELSE
            PSIC=0.095D0*SS**(-2.0D0/3.0D0)
     $          +0.056D0*(1.0D0-EXP(-SS**0.75D0/20.0D0))
         ENDIF
      ENDIF
C
      RETURN
C
C ... ファイルオープンエラー
  900 CONTINUE
      CALL ERRMSG('MKINISD',7019)
      WRITE(LP,*) 'FILE OPEN ERROR: ZBED FILE'
      WRITE(LP,*) 'FILE NUMBER=',IFLZB
      CALL ABORT1('')
C
C ... ファイル読み込みエラー
  910 CONTINUE
      CALL ERRMSG('MKINISD',7020)
      WRITE(LP,*) 'FILE READ ERROR: ZBED FILE'
      WRITE(LP,*) 'FILE NUMBER=',IFLZB
      CALL ABORT1('')
C
      END
