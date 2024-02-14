      SUBROUTINE MKIND1(INDP,INDU,INDV,INDW,INDC,GX,GY,NFL)
C======================================================================
C     インデックス INDP, INDU, INDV, INDW の値を設定する
C
C     (1) 障害物データファイルを読み込む
C     (2) 解析条件入力データファイル内で指定した
C         障害物データファイルを設定する
C     (3) 流速固定境界を設定
C     (4) 自由流入出境界(0)を設定
C     (5) 計算点のインデックスに壁面の方向を設定
C     (6) 壁面境界の数をカウントする
C     (7) GX=0.0,GY=0.0のデータをPLATE-I,Jデータとみなす
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'AREA.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'FILEI.h'
      INCLUDE 'OBSTI.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'CADMAS.h'
      INCLUDE 'GLOBAL.h'
      INCLUDE 'MODELI.h'
C
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),INDU(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDV(MX,MY,MZ),INDW(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDC(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GX(MX,MY,MZ),GY(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::NFL
C
      INTEGER::IDB=0
C
      INTEGER::I,IDIR,IE,IS,J,JE,JS,K,KE,KS,IRTN
      INTEGER::M,M1,M1WALL,M1WALP,M2,M2WALL,M2WALP,N,NX1,NY1,NZ1
C
      IF(NFL.EQ.2) GO TO 80
C
C ... 全て障害物セルとして初期化
      CALL ZERCLI(INDP,MXYZ,0)
      CALL ZERCLI(INDU,MXYZ,-4)
      CALL ZERCLI(INDV,MXYZ,-4)
      CALL ZERCLI(INDW,MXYZ,-4)
C
C ... 作業用配列INDCを全て流体セルとして初期化
      CALL ZERCLI(INDC,MXYZ,1)
C
      CALL ZERCLR(GX,MXYZ,1.0D0)
      CALL ZERCLR(GY,MXYZ,1.0D0)
C
C
C----------------------------------------------------------------------
C     (1) 障害物データファイルを読み込む
C----------------------------------------------------------------------
C
      IF( LOBST.EQ.1 ) THEN
         CFLNM(IFLNM-3:IFLNM) = '.str'
         OPEN(IFLST,FILE=CFLNM(1:IFLNM),STATUS='OLD',
     $        FORM='UNFORMATTED',ERR=900)
         WRITE(LP,*) 'READING OBSTACLE DATA'
         READ(IFLST,ERR=910) NX1,NY1,NZ1
         IF(IAUTOD.EQ.0)THEN         
         IF( NX1.NE.MX-2 .OR. NY1.NE.MY-2 .OR. NZ1.NE.MZ-2 ) THEN
            CALL ERRMS2('MKIND1',6940)
            WRITE(LP,*) 'MESH NUMBER IS DIFFERENT FROM INPUT DATA'
            WRITE(LP,*) 'NX,NY,NZ=',NX1,NY1,NZ1,' IN STRUCTURE FILE'
            WRITE(LP,*) '        =',MX-2,MY-2,MZ-2,' IN GRID DATA'
         END IF
         ELSE
         IF( MYPROC.EQ.1.AND.
     $       NX1.NE.MXG-2 .OR. NY1.NE.MYG-2 .OR. NZ1.NE.MZG-2 ) THEN
            CALL ERRMS2('MKIND1',6941)
            WRITE(LP,*) 'MESH NUMBER IS DIFFERENT FROM INPUT DATA'
            WRITE(LP,*) 'NX,NY,NZ=',NX1,NY1,NZ1,' IN STRUCTURE FILE'
            WRITE(LP,*) '        =',MXG-2,MYG-2,MZG-2,' IN GRID DATA'
         END IF
         END IF
C
         IF(IAUTOD.EQ.0)THEN
            READ(IFLST,ERR=910)(((INDC(I,J,K),I=2,MXM),J=2,MYM),K=2,MZM)
            READ(IFLST) ! (((GV(I,J,K),I=1,MXM),J=2,MYM),K=2,MZM)
            READ(IFLST,ERR=920)(((GX(I,J,K),I=1,MXM),J=2,MYM),K=2,MZM)
            READ(IFLST,ERR=930)(((GY(I,J,K),I=2,MXM),J=1,MYM),K=2,MZM)
C
         ELSE
            CALL RDSUB3IB(INDC,IFLST,0,IRTN)
            IF(IRTN.EQ.1) GOTO 910
            READ(IFLST) ! SKIP
            CALL RDSUB3RB(GX,IFLST,1,IRTN)
            IF(IRTN.EQ.1) GOTO 920
            CALL RDSUB3RB(GY,IFLST,2,IRTN)
            IF(IRTN.EQ.1) GOTO 930
         ENDIF
C 20110907 for Hitachi FORTRAN
C         BACKSPACE IFLST
C         BACKSPACE IFLST
C         BACKSPACE IFLST
         REWIND IFLST
C 20110907 for Hitachi FORTRAN
C
         IF(NSEA.GE.1) THEN
           DO 1111 N=1,NSEA
             INDC(ISEA(1,N),ISEA(2,N),MZM)=1
 1111      CONTINUE
         END IF
C
C@@@@@@@@@@@@@@@@@@
         DO 90 K=2,MZM
         DO 90 J=2,MYM
           IF (NSOMER(2).GT.0) INDC(2    ,J,K)=INDC(3    ,J,K)
           IF (NSOMER(3).GT.0) INDC(MXM  ,J,K)=INDC(MXM-1,J,K)
 90      CONTINUE
         DO 91 K=2,MZM
         DO 91 I=2,MXM
           IF (NSOMER(1).GT.0) INDC(I,2    ,K)=INDC(I,3    ,K)
           IF (NSOMER(4).GT.0) INDC(I,MYM  ,K)=INDC(I,MYM-1,K)
 91      CONTINUE
C@@@@@@@@@@@@@@@@@@
      END IF
C
C
C----------------------------------------------------------------------
C     (2) 解析条件入力データファイル内で指定した
C         障害物データを設定する
C----------------------------------------------------------------------
C
C ... 入力データブロックで指定した立体構造物を設定
      DO 100 N=1,NOBSS
         IF( NB_SC.GT.0.AND.N.EQ.1 ) GOTO 100
         IS = IOBSS(1,N)
         IE = IOBSS(2,N)
         JS = IOBSS(3,N)
         JE = IOBSS(4,N)
         KS = IOBSS(5,N)
         KE = IOBSS(6,N)
C
         DO 110 K=KS,KE
         DO 110 J=JS,JE
         DO 110 I=IS,IE
            INDC(I,J,K) = 0
  110    CONTINUE
  100 CONTINUE
C
C
C ... 外側一層の仮想セルを除いて、INDCの値をINDPにコピー
      DO 200 K=2,MZM
      DO 200 J=2,MYM
      DO 200 I=2,MXM
         INDP(I,J,K) = INDC(I,J,K)
  200 CONTINUE
C
      IF(NFL.EQ.1) RETURN
   80 CONTINUE
C
C ... 両側が流体セルの場合に流速計算用インデックスを1にする
C     また、片側だけ流体セルの場合には壁面とみなし-2にする
      DO 300 K=2,MZM
      DO 300 J=2,MYM
      DO 300 I=1,MXM
         IF( INDP(I,J,K).EQ.1 .AND. INDP(I+1,J,K).EQ.1 ) THEN
            INDU(I,J,K) = 1
         ELSE IF( INDP(I,J,K).EQ.1 .OR. INDP(I+1,J,K).EQ.1 ) THEN
            INDU(I,J,K) = -2
            IF(NESTFL.NE.0) THEN
              IF(MX.NE.3.AND.I.EQ.1.AND.
     &           IPECON(5,NRANK+1).EQ.-1) INDU(I,J,K)=-1
              IF(MX.NE.3.AND.I.EQ.MXM.AND.
     &           IPECON(6,NRANK+1).EQ.-1) INDU(I,J,K)=-1
            END IF
         END IF
  300 CONTINUE
C
      DO 310 K=2,MZM
      DO 310 J=1,MYM
      DO 310 I=2,MXM
         IF( INDP(I,J,K).EQ.1 .AND. INDP(I,J+1,K).EQ.1 ) THEN
            INDV(I,J,K) = 1
         ELSE IF( INDP(I,J,K).EQ.1 .OR. INDP(I,J+1,K).EQ.1 ) THEN
            INDV(I,J,K) = -2
            IF(NESTFL.NE.0) THEN
              IF(MY.NE.3.AND.J.EQ.1.AND.
     &           IPECON(4,NRANK+1).EQ.-1) INDV(I,J,K)=-1
              IF(MY.NE.3.AND.J.EQ.MYM.AND.
     &           IPECON(7,NRANK+1).EQ.-1) INDV(I,J,K)=-1
            END IF
         END IF
  310 CONTINUE
C
      DO 320 K=1,MZM
      DO 320 J=2,MYM
      DO 320 I=2,MXM
         IF( INDP(I,J,K).EQ.1 .AND. INDP(I,J,K+1).EQ.1 ) THEN
            INDW(I,J,K) = 1
         ELSE IF( INDP(I,J,K).EQ.1 .OR. INDP(I,J,K+1).EQ.1 ) THEN
            INDW(I,J,K) = -2
         END IF
  320 CONTINUE
C
C
C
C#    ( この時点でINDPは 0:構造物 or 1:流体 )
C
C ... 入力データブロックで指定した板状構造物を設定
      MLWALP = 0
      M1 = 0
      M2 = 0
C
C ... *.strのGX=0.0,GY=0.0のデータを板状構造物として処理
C
      IF(LOBST.EQ.1) THEN
        DO 350 J=1,MYM
        DO 350 I=1,MXM
          IF(J.GT.1) THEN
          KS = 2
          KE = 0
          DO 360 K=2,MZM
            IF(GX(I,J,K).EQ.0.0D0) THEN
              KE = K
              IF( INDP(I,J,K).EQ.1 .AND. INDP(I+1,J,K).EQ.1 ) THEN
                MLWALP = MLWALP + 1
                M1 = M1 + 1
                INDU(I,J,K) = -3
              ELSE
C ............. 片側が構造物セルの場合、板状構造物としては扱わない
C                CALL ERRMS2('MKIND1',6942)
C                WRITE(LP,*) 'PLATE OBSTACLE IS IGNORED AT ',
C     $                      'INDEX (',I,J,K,')'
              END IF
            END IF
  360     CONTINUE
          END IF
C
  365     CONTINUE
          IF(I.GT.1) THEN
          KS = 2
          KE = 0
          DO 370 K=2,MZM
            IF(GY(I,J,K).EQ.0.0D0) THEN
              KE = K
              IF( INDP(I,J,K).EQ.1 .AND. INDP(I,J+1,K).EQ.1 ) THEN
                MLWALP = MLWALP + 1
                M2 = M2 + 1
                INDV(I,J,K) = -3
              ELSE
C ............. 片側が構造物セルの場合、板状構造物としては扱わない
C                CALL ERRMS2('MKIND1',6943)
C                WRITE(LP,*) 'PLATE OBSTACLE IS IGNORED AT ',
C     $                      'INDEX (',I,J,K,')'
              END IF
            END IF
  370     CONTINUE
          END IF
C
  350   CONTINUE
      END IF
C
      DO 400 N=1,NOBSP
         IS = IOBSP(1,N)
         IE = IOBSP(2,N)
         JS = IOBSP(3,N)
         JE = IOBSP(4,N)
         KS = IOBSP(5,N)
         KE = IOBSP(6,N)
         IDIR = IOBSP(7,N)
C
         IF( IDIR.EQ.1 ) THEN
            I = IS
            DO 410 K=KS,KE
            DO 410 J=JS,JE
               IF( INDP(I,J,K).EQ.1 .AND. INDP(I+1,J,K).EQ.1 ) THEN
                  MLWALP = MLWALP + 1
                  M1 = M1 + 1
                  INDU(I,J,K) = -3
               ELSE
C ............... 片側が構造物セルの場合、板状構造物としては扱わない
                  CALL ERRMS2('MKIND1',6944)
                  WRITE(LP,*) 'PLATE OBSTACLE IS IGNORED AT ',
     $                        'INDEX (',I,J,K,')'
               END IF
  410       CONTINUE
         ELSE IF( IDIR.EQ.2 ) THEN
            J = JS
            DO 420 K=KS,KE
            DO 420 I=IS,IE
               IF( INDP(I,J,K).EQ.1 .AND. INDP(I,J+1,K).EQ.1 ) THEN
                  MLWALP = MLWALP + 1
                  M2 = M2 + 1
                  INDV(I,J,K) = -3
               ELSE
C ............... 片側が構造物セルの場合、板状構造物としては扱わない
                  CALL ERRMS2('MKIND1',6945)
                  WRITE(LP,*) 'PLATE OBSTACLE IS IGNORED AT ',
     $                        'INDEX (',I,J,K,')'
               END IF
  420       CONTINUE
         ELSE IF( IDIR.EQ.3 ) THEN
            K = KS
            DO 430 J=JS,JE
            DO 430 I=IS,IE
               IF( INDP(I,J,K).EQ.1 .AND. INDP(I,J,K+1).EQ.1 ) THEN
                  MLWALP = MLWALP + 1
                  INDW(I,J,K) = -3
               ELSE
C ............... 片側が構造物セルの場合、板状構造物としては扱わない
                  CALL ERRMS2('MKIND1',6946)
                  WRITE(LP,*) 'PLATE OBSTACLE IS IGNORED AT ',
     $                        'INDEX (',I,J,K,')'
               END IF
  430       CONTINUE
         END IF
  400 CONTINUE
      M1WALP = M1
      M2WALP = M1 + M2
C
C
C#    ( この時点でIND[UVW]は  -4:構造物内部 or -3:板 or -2:壁 or 1:計算点 )
C#    ( この時点でINDPは 0:構造物 or 1:流体 )
C
C----------------------------------------------------------------------
C     (3) 流速固定境界(-1)を設定
C----------------------------------------------------------------------
      DO 500 N=1,NINLT
         IF( NB_SC.GT.0.AND.N.LE.4 ) GOTO 500
         M  = MINLT(N)
         IS = IAREA(1,M)
         IE = IAREA(2,M)
         JS = IAREA(3,M)
         JE = IAREA(4,M)
         KS = IAREA(5,M)
         KE = IAREA(6,M)
         IDIR = IAREA(7,M)
C
C ...... 法線方向がX方向の面
         IF( IDIR.EQ.1 ) THEN
            I = IS
            DO 510 K=KS,KE
            DO 510 J=JS,JE
C
C ............ 指定が正常な場合(流体と構造物セルの境界に位置する場合)
ccc               IF( INDU(I,J,K).EQ.-2 ) THEN
               IF( INDU(I,J,K).EQ.-2 .OR. INDU(I,J,K).EQ.-4 ) THEN
                  INDU(I,J,K) = -1
C
C ............ 指定位置が不適切でエラーの場合
               ELSE
                  IF( INDU(I,J,K).EQ.1 ) THEN
C                    < 流速の計算点に流入境界条件を指定 >
                     CALL ERRMS2('MKIND1',6947)
                     WRITE(LP,*) 'FIXED-INLET AREA IS SET BETWEEN FLUID'
C
                  ELSE IF( INDU(I,J,K).EQ.-3 ) THEN
C                    < 板状構造物上に流入境界条件を指定 >
                     CALL ERRMS2('MKIND1',6948)
                     WRITE(LP,*) 'FIXED-INLET AREA IS SET',
     $                           ' ON PLATE-OBSTACLE'
C
                  ELSE IF( INDU(I,J,K).EQ.-4 ) THEN
C                    < 構造物内部に流入境界条件を指定 >
ccc                     CALL ERRMS2('MKIND1',6949)
ccc                     WRITE(LP,*) 'FIXED-INLET AREA IS SET',
ccc     $                           ' INSIDE OF OBSTACLE'
C
                  ELSE
C                    < 流速固定境界の位置が重複している >
                     CALL ERRMS2('MKIND1',6950)
                     WRITE(LP,*) 'FIXED-INLET AREA IS SET',
     $                           ' ON THE SAME AREA OF OTHER'
                  END IF
                  WRITE(LP,*) 'I,J,K=',I,J,K
                  WRITE(LP,*) 'AREA NUM.=',M
                  WRITE(LP,*) 'IS,IE=',IS,IE
                  WRITE(LP,*) 'JS,JE=',JS,JE
                  WRITE(LP,*) 'KS,KE=',KS,KE
                  WRITE(LP,*) 'DIR  =',IDIR,'(=1:X,=2:Y,=3:Z)'
               END IF
  510       CONTINUE
C
C ...... 法線方向がY方向の面
         ELSE IF( IDIR.EQ.2 ) THEN
            J = JS
            DO 520 K=KS,KE
            DO 520 I=IS,IE
C
C ............ 指定が正常な場合(流体と構造物セルの境界に位置する場合)
ccc               IF( INDV(I,J,K).EQ.-2 ) THEN
               IF( INDV(I,J,K).EQ.-2.OR.INDV(I,J,K).EQ.-4 ) THEN
                  INDV(I,J,K) = -1
C
C ............ 指定位置が不適切でエラーの場合
               ELSE
                  IF( INDV(I,J,K).EQ.1 ) THEN
C                    < 流速の計算点に流入境界条件を指定 >
                     CALL ERRMS2('MKIND1',6951)
                     WRITE(LP,*) 'FIXED-INLET AREA IS SET BETWEEN FLUID'
C
                  ELSE IF( INDV(I,J,K).EQ.-3 ) THEN
C                    < 板状構造物上に流入境界条件を指定 >
                     CALL ERRMS2('MKIND1',6952)
                     WRITE(LP,*) 'FIXED-INLET AREA IS SET',
     $                           ' ON PLATE-OBSTACLE'
C
                  ELSE IF( INDV(I,J,K).EQ.-4 ) THEN
C                    < 構造物内部に流入境界条件を指定 >
ccc                     CALL ERRMS2('MKIND1',6953)
ccc                     WRITE(LP,*) 'FIXED-INLET AREA IS SET',
ccc     $                           ' INSIDE OF OBSTACLE'
C
                  ELSE
C                    < 流速固定境界の位置が重複している >
                     CALL ERRMS2('MKIND1',6954)
                     WRITE(LP,*) 'FIXED-INLET AREA IS SET',
     $                           ' ON THE SAME AREA OF OTHER'
                  END IF
                  WRITE(LP,*) 'I,J,K=',I,J,K
                  WRITE(LP,*) 'AREA NUM.=',M
                  WRITE(LP,*) 'IS,IE=',IS,IE
                  WRITE(LP,*) 'JS,JE=',JS,JE
                  WRITE(LP,*) 'KS,KE=',KS,KE
                  WRITE(LP,*) 'DIR  =',IDIR,'(=1:X,=2:Y,=3:Z)'
               END IF
  520       CONTINUE
C
C ...... 法線方向がZ方向の面
         ELSE IF( IDIR.EQ.3 ) THEN
            K = KS
            DO 530 J=JS,JE
            DO 530 I=IS,IE
C
C ............ 指定が正常な場合(流体と構造物セルの境界に位置する場合)
               IF( INDW(I,J,K).EQ.-2 ) THEN
                  INDW(I,J,K) = -1
C
C ............ 指定位置が不適切でエラーの場合
               ELSE
                  IF( INDW(I,J,K).EQ.1 ) THEN
C                    < 流速の計算点に流入境界条件を指定 >
                     CALL ERRMS2('MKIND1',6955)
                     WRITE(LP,*) 'FIXED-INLET AREA IS SET BETWEEN FLUID'
C
                  ELSE IF( INDW(I,J,K).EQ.-3 ) THEN
C                    < 板状構造物上に流入境界条件を指定 >
                     CALL ERRMS2('MKIND1',6956)
                     WRITE(LP,*) 'FIXED-INLET AREA IS SET',
     $                           ' ON PLATE-OBSTACLE'
C
                  ELSE IF( INDW(I,J,K).EQ.-4 ) THEN
C                    < 構造物内部に流入境界条件を指定 >
ccc                     CALL ERRMS2('MKIND1',6957)
ccc                     WRITE(LP,*) 'FIXED-INLET AREA IS SET',
ccc     $                           ' INSIDE OF OBSTACLE'
C
                  ELSE
C                    < 流速固定境界の位置が重複している >
                     CALL ERRMS2('MKIND1',6958)
                     WRITE(LP,*) 'FIXED-INLET AREA IS SET',
     $                           ' ON THE SAME AREA OF OTHER'
                  END IF
                  WRITE(LP,*) 'I,J,K=',I,J,K
                  WRITE(LP,*) 'AREA NUM.=',M
                  WRITE(LP,*) 'IS,IE=',IS,IE
                  WRITE(LP,*) 'JS,JE=',JS,JE
                  WRITE(LP,*) 'KS,KE=',KS,KE
                  WRITE(LP,*) 'DIR  =',IDIR,'(=1:X,=2:Y,=3:Z)'
               END IF
  530       CONTINUE
         END IF
  500 CONTINUE
C
C
C----------------------------------------------------------------------
C     (4) 自由流入出境界(0)を設定
C----------------------------------------------------------------------
      DO 600 N=1,NOUTLT
         M  = MOUTLT(N)
         IS = IAREA(1,M)
         IE = IAREA(2,M)
         JS = IAREA(3,M)
         JE = IAREA(4,M)
         KS = IAREA(5,M)
         KE = IAREA(6,M)
         IDIR = IAREA(7,M)
C
C ...... 法線方向がX方向の面
         IF( IDIR.EQ.1 ) THEN
            I = IS
            DO 610 K=KS,KE
            DO 610 J=JS,JE
C
C ............ 指定が正常な場合(流体と構造物セルの境界に位置する場合)
ccc               IF( INDU(I,J,K).EQ.-2 ) THEN
               IF( INDU(I,J,K).EQ.-2.or.INDU(i,j,k).EQ.-4 ) THEN
                  INDU(I,J,K) = 0
C
C ............ 指定位置が不適切でエラーの場合
               ELSE
                  IF( INDU(I,J,K).EQ.1 ) THEN
C                    < 流速の計算点に流入境界条件を指定 >
                     CALL ERRMS2('MKIND1',6959)
                     WRITE(LP,*) 'FREE-OUTLET AREA IS SET BETWEEN FLUID'
C
                  ELSE IF( INDU(I,J,K).EQ.-3 ) THEN
C                    < 板状構造物上に流入境界条件を指定 >
                     CALL ERRMS2('MKIND1',6960)
                     WRITE(LP,*) 'FREE-OUTLET AREA IS SET',
     $                           ' ON PLATE-OBSTACLE'
C
                  ELSE IF( INDU(I,J,K).EQ.-4 ) THEN
C                    < 構造物内部に流入境界条件を指定 >
ccc                     CALL ERRMS2('MKIND1',6961)
ccc                     WRITE(LP,*) 'FREE-OUTLET AREA IS SET',
ccc     $                           ' INSIDE OF OBSTACLE'
C
                  ELSE
C                    < 流速固定または自由流入出境界の位置が重複している >
                     CALL ERRMS2('MKIND1',6962)
                     WRITE(LP,*) 'FREE-OUTLET AREA IS SET',
     $                           ' ON THE SAME AREA OF OTHER'
                  END IF
                  WRITE(LP,*) 'I,J,K,INDU=',I,J,K,INDU(I,J,K)
                  WRITE(LP,*) 'AREA NUM.=',M
                  WRITE(LP,*) 'IS,IE=',IS,IE
                  WRITE(LP,*) 'JS,JE=',JS,JE
                  WRITE(LP,*) 'KS,KE=',KS,KE
                  WRITE(LP,*) 'DIR  =',IDIR,'(=1:X,=2:Y,=3:Z)'
               END IF
  610       CONTINUE
C
C ...... 法線方向がY方向の面
         ELSE IF( IDIR.EQ.2 ) THEN
            J = JS
            DO 620 K=KS,KE
            DO 620 I=IS,IE
C
C ............ 指定が正常な場合(流体と構造物セルの境界に位置する場合)
ccc               IF( INDV(I,J,K).EQ.-2 ) THEN
               IF( INDV(I,J,K).EQ.-2.or.INDV(I,J,K).EQ.-4 ) THEN
                  INDV(I,J,K) = 0
C
C ............ 指定位置が不適切でエラーの場合
               ELSE
                  IF( INDV(I,J,K).EQ.1 ) THEN
C                    < 流速の計算点に流入境界条件を指定 >
                     CALL ERRMS2('MKIND1',6963)
                     WRITE(LP,*) 'FREE-OUTLET AREA IS SET BETWEEN FLUID'
C
                  ELSE IF( INDV(I,J,K).EQ.-3 ) THEN
C                    < 板状構造物上に流入境界条件を指定 >
                     CALL ERRMS2('MKIND1',6964)
                     WRITE(LP,*) 'FREE-OUTLET AREA IS SET',
     $                           ' ON PLATE-OBSTACLE'
C
                  ELSE IF( INDV(I,J,K).EQ.-4 ) THEN
C                    < 構造物内部に流入境界条件を指定 >
ccc                     CALL ERRMS2('MKIND1',6965)
ccc                     WRITE(LP,*) 'FREE-OUTLET AREA IS SET',
ccc     $                           ' INSIDE OF OBSTACLE'
C
                  ELSE
C                    < 流速固定または自由流入出境界の位置が重複している >
                     CALL ERRMS2('MKIND1',6966)
                     WRITE(LP,*) 'FREE-OUTLET AREA IS SET',
     $                           ' ON THE SAME AREA OF OTHER'
                  END IF
                  WRITE(LP,*) 'I,J,K=',I,J,K
                  WRITE(LP,*) 'AREA NUM.=',M
                  WRITE(LP,*) 'IS,IE=',IS,IE
                  WRITE(LP,*) 'JS,JE=',JS,JE
                  WRITE(LP,*) 'KS,KE=',KS,KE
                  WRITE(LP,*) 'DIR  =',IDIR,'(=1:X,=2:Y,=3:Z)'
               END IF
  620       CONTINUE
C
C ...... 法線方向がZ方向の面
         ELSE IF( IDIR.EQ.3 ) THEN
            K = KS
            DO 630 J=JS,JE
            DO 630 I=IS,IE
C
C ............ 指定が正常な場合(流体と構造物セルの境界に位置する場合)
               IF( INDW(I,J,K).EQ.-2 ) THEN
                  INDW(I,J,K) = 0
C
C ............ 指定位置が不適切でエラーの場合
               ELSE
                  IF( INDW(I,J,K).EQ.1 ) THEN
C                    < 流速の計算点に流入境界条件を指定 >
                     CALL ERRMS2('MKIND1',6967)
                     WRITE(LP,*) 'FREE-OUTLET AREA IS SET BETWEEN FLUID'
C
                  ELSE IF( INDW(I,J,K).EQ.-3 ) THEN
C                    < 板状構造物上に流入境界条件を指定 >
                     CALL ERRMS2('MKIND1',6968)
                     WRITE(LP,*) 'FREE-OUTLET AREA IS SET',
     $                           ' ON PLATE-OBSTACLE'
C
                  ELSE IF( INDW(I,J,K).EQ.-4 ) THEN
C                    < 構造物内部に流入境界条件を指定 >
ccc                     CALL ERRMS2('MKIND1',6969)
ccc                     WRITE(LP,*) 'FREE-OUTLET AREA IS SET',
ccc     $                           ' INSIDE OF OBSTACLE'
C
                  ELSE
C                    < 流速固定または自由流入出境界の位置が重複している >
                     CALL ERRMS2('MKIND1',6970)
                     WRITE(LP,*) 'FREE-OUTLET AREA IS SET',
     $                           ' ON THE SAME AREA OF OTHER'
                  END IF
                  WRITE(LP,*) 'I,J,K=',I,J,K
                  WRITE(LP,*) 'AREA NUM.=',M
                  WRITE(LP,*) 'IS,IE=',IS,IE
                  WRITE(LP,*) 'JS,JE=',JS,JE
                  WRITE(LP,*) 'KS,KE=',KS,KE
                  WRITE(LP,*) 'DIR  =',IDIR,'(=1:X,=2:Y,=3:Z)'
               END IF
  630       CONTINUE
         END IF
  600 CONTINUE
C
C
C#    ( この時点でIND[UVW]は  -4:構造物内部 or  -3:板 or -2:壁 or
C                             -1:流速固定境界 or 0:自由流入出境界 or 1:計算点 )
C#    ( この時点でINDPは 0:構造物 or 1:計算点 )
C
CCCC----------------------------------------------------------------------
CCCC     (5) 計算点のインデックスに壁面の方向を設定
CCCC----------------------------------------------------------------------
CCCC
CCCC ... INDPに壁の方向を設定する
CCC      DO 700 K=2,MZM
CCC      DO 700 J=2,MYM
CCC      DO 700 I=2,MXM
CCC         IF( INDP(I,J,K).EQ.1 ) THEN
CCCC ......... 壁なし条件で初期化
CCC            IWM1 = 0
CCC            IWP1 = 0
CCC            JWM1 = 0
CCC            JWP1 = 0
CCC            KWM1 = 0
CCC            KWP1 = 0
CCCC ......... 壁があったらフラグを立てる
CCC            IF( INDU(I-1,J,K).LE.-2 ) IWM1 = 1
CCC            IF( INDU(I  ,J,K).LE.-2 ) IWP1 = 1
CCC            IF( INDV(I,J-1,K).LE.-2 ) JWM1 = 1
CCC            IF( INDV(I,J  ,K).LE.-2 ) JWP1 = 1
CCC            IF( INDW(I,J,K-1).LE.-2 ) KWM1 = 1
CCC            IF( INDW(I,J,K  ).LE.-2 ) KWP1 = 1
CCC            INDP(I,J,K) = 1 + IWM1 + IWP1*2 + JWM1*4
CCC     $                  + JWP1*8 + KWM1*16 + KWP1*32
CCC         END IF
CCC  700 CONTINUE
C
C
C----------------------------------------------------------------------
C     (6) 壁面境界の数をカウントする
C----------------------------------------------------------------------
      MLWALL = 0
      DO 800 K=2,MZM
      DO 800 J=2,MYM
      DO 800 I=1,MXM
         IF( INDU(I,J,K).EQ.-2 ) MLWALL = MLWALL + 1
  800 CONTINUE
      M1WALL = MLWALL
C
      DO 810 K=2,MZM
      DO 810 J=1,MYM
      DO 810 I=2,MXM
         IF( INDV(I,J,K).EQ.-2 ) MLWALL = MLWALL + 1
  810 CONTINUE
      M2WALL = MLWALL
C
      DO 820 K=1,MZM
      DO 820 J=2,MYM
      DO 820 I=2,MXM
         IF( INDW(I,J,K).EQ.-2 ) MLWALL = MLWALL + 1
  820 CONTINUE
C
      MLWALL1=MLWALL
      IF( LSTOCDS.EQ.1 ) THEN
         MLWALL =MLWALL*10
      ENDIF
C
C
C ... デバッグ出力
      IF( IDB.GT.0 ) THEN
      IF(MX.EQ.3) THEN
        WRITE(LP,*) 'INDP:I=2'
        CALL DBWI2D(INDP,1,2)
        WRITE(LP,*) 'INDU:I=1'
        CALL DBWI2D(INDU,1,1)
        WRITE(LP,*) 'INDU:I=2'
        CALL DBWI2D(INDU,1,2)
        WRITE(LP,*) 'INDV:I=2'
        CALL DBWI2D(INDV,1,2)
        WRITE(LP,*) 'INDW:I=2'
        CALL DBWI2D(INDW,1,2)
      ELSE IF(MZ.EQ.3) THEN
        WRITE(LP,*) 'INDP:K=2'
        CALL DBWI2D(INDP,3,2)
        WRITE(LP,*) 'INDU:K=2'
        CALL DBWI2D(INDU,3,2)
        WRITE(LP,*) 'INDV:K=2'
        CALL DBWI2D(INDV,3,2)
        WRITE(LP,*) 'INDW:K=1'
        CALL DBWI2D(INDW,3,1)
        WRITE(LP,*) 'INDW:K=2'
        CALL DBWI2D(INDW,3,2)
      ELSE IF(MY.EQ.3) THEN
        WRITE(LP,*) 'INDP:J=2'
        CALL DBWI2D(INDP,2,2)
        WRITE(LP,*) 'INDU:J=2'
        CALL DBWI2D(INDU,2,2)
        WRITE(LP,*) 'INDV:J=1'
        CALL DBWI2D(INDV,2,1)
        WRITE(LP,*) 'INDV:J=2'
        CALL DBWI2D(INDV,2,2)
        WRITE(LP,*) 'INDW:J=2'
        CALL DBWI2D(INDW,2,2)
      ELSE IF(IDB.NE.0) THEN
        IF(MZ.GT. 3.AND.MZ.LE. 8.AND.IDB.GT.MZ-1) IDB=4
        IF(MZ.GT. 8.AND.MZ.LE.12.AND.IDB.GT.MZ-1) IDB=8
        IF(MZ.GT.12.AND.MZ.LE.16.AND.IDB.GT.MZ-1) IDB=12
        IF(MZ.GT.16.AND.IDB.GT.MZ-1) IDB=MZ-5
C
        WRITE(LP,*) 'INDP:K=',IDB
        CALL DBWI2D(INDP,3,IDB)
        WRITE(LP,*) 'INDU:K=',IDB
        CALL DBWI2D(INDU,3,IDB)
        WRITE(LP,*) 'INDV:K=',IDB
        CALL DBWI2D(INDV,3,IDB)
        WRITE(LP,*) 'INDW:K=',IDB
        CALL DBWI2D(INDW,3,IDB)
      END IF
      ENDIF
C
      RETURN
C
C ... ファイルオープンエラー
  900 CONTINUE
      CALL ERRMSG('MKIND1',6971)
      WRITE(LP,*) 'FILE OPEN ERROR: STRUCTURE FILE'
      WRITE(LP,*) 'FILE NUMBER=',IFLST
      CALL ABORT1('')
C
C ... ファイル読み込みエラー
  910 CONTINUE
      CALL ERRMSG('MKIND1',6972)
      WRITE(LP,*) 'FILE READ ERROR: STRUCTURE FILE'
      WRITE(LP,*) 'FILE NUMBER=',IFLST
      CALL ABORT1('')
C
  920 CONTINUE
      CALL ERRMSG('MKIND1',6973)
      WRITE(LP,*) 'FILE READ ERROR: STRUCTURE FILE'
      WRITE(LP,*) 'VARIABLE=GX'
      WRITE(LP,*) 'FILE NUMBER=',IFLST
      CALL ABORT1('')
C
  930 CONTINUE
      CALL ERRMSG('MKIND1',6974)
      WRITE(LP,*) 'FILE READ ERROR: STRUCTURE FILE'
      WRITE(LP,*) 'VARIABLE=GY'
      WRITE(LP,*) 'FILE NUMBER=',IFLST
      CALL ABORT1('')
      END
