      SUBROUTINE MKIND4(GV,GX,GY,XC,YC,ZC,HDEP,HHOFL,
     $                  INDP,INDU,INDV,LLWALB,LLOFL,IFLAG,JFLAG)
C======================================================================
C     防潮堤処理用インデックス LLWALB を設定する
C     越流量計算用インデックス LLOFL  を設定する
C
C     IFLAG = 0 : LLWALB の配列サイズ MLWALB を計算する
C           = 1 : LLWALB を設定する
C======================================================================
      use mod_comm,only: comm_model
      IMPLICIT NONE
C
      INCLUDE 'FILE.h'
      INCLUDE 'FILEI.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'OBSTI.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'mpif.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
C
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GX(MX,MY,MZ),GY(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)::HDEP(MX,MY)
      REAL(8),INTENT(INOUT)::HHOFL(MLOFL)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDU(MX,MY,MZ),INDV(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::LLWALB(3,MLWALB),LLOFL(3,MLOFL),IFLAG,JFLAG
C
      INTEGER,ALLOCATABLE  :: INDX(:,:,:),INDY(:,:,:)
      REAL(8)::GV1,FV1,FV2,FXY
      REAL(8),PARAMETER:: EPF=1.0D-5,EPSX=1.0D-10
      INTEGER::I,J,K,M,N,IS,IE,JS,JE,KS,KE,IERR,NN,M1,M2,IX,IY,ICODE
      INTEGER::IDUM,JDUM,INSIDE
      CHARACTER(1):: CH
C
C
      M = 0
C
C ... X方向が法線方向の防潮堤を調べる
C
      DO 100 K=2,MZM
      DO 100 J=2,MYM
      DO 100 I=1,MXM
         IF( INDU(I,J,K).GT.0 ) THEN
            GV1 = GV(I,J,K)*XC(7,I,J)+GV(I+1,J,K)*XC(8,I,J)
            IF( GX(I,J,K)+EPSX.LT.GV1 ) THEN
               M = M+1
               IF( IFLAG.EQ.1 ) THEN
                  LLWALB(1,M) = I
                  LLWALB(2,M) = J
                  LLWALB(3,M) = K
               END IF
            END IF
         END IF
  100 CONTINUE
C
C
      IF( IFLAG.EQ.0 ) MLWALBX = M
C
C ... Y方向が法線方向の防潮堤を調べる
C
      DO 200 K=2,MZM
      DO 200 J=1,MYM
      DO 200 I=2,MXM
         IF( INDV(I,J,K).GT.0 ) THEN
            GV1 = GV(I,J,K)*YC(7,J)+GV(I,J+1,K)*YC(8,J)
            IF( GY(I,J,K)+EPSX.LT.GV1 ) THEN
               M = M+1
               IF( IFLAG.EQ.1 ) THEN
                  LLWALB(1,M) = I
                  LLWALB(2,M) = J
                  LLWALB(3,M) = K
               END IF
            END IF
         END IF
  200 CONTINUE
C
      IF( IFLAG.EQ.0 ) MLWALB = M
C
      IF( IHONMA.GT.0.OR.IAIDA.GT.0.OR.IBKSTP.GT.0 ) THEN
         M1 = 0
         M2 = 0
         IF( IFLAG.EQ.1 ) M2=MLOFLX
         CFLNM(IFLNM-3:IFLNM) = '.ofl'
         if( JFLAG.EQ.0 ) write(lp,*) CFLNM
         OPEN(IFLOF,FILE=CFLNM(1:IFLNM),STATUS='OLD',
     $        FORM='FORMATTED',ERR=900)
C
C        1行目はスキップ
         READ(IFLOF,*)
         DO
            READ(IFLOF,*,END=250) I,J,IX
            IF(IAUTOD.EQ.0)THEN
               IF( (I.LE.1.OR.I.GE.MX.OR.J.LE.1.OR.J.GE.MY)
     $            .AND.JFLAG.EQ.0 ) THEN
                  CALL ERRMSG('MKIND4',6990)
                  WRITE(LP,*) 'ERROR: OFL-FILE:I,J VALUE IS INVALID'
                  WRITE(LP,*) '                I J FLAG=',I,J,IX
                  CALL ABORT1('')
               ENDIF
            ELSE
               IF( MYPROC.EQ.1.AND.
     $            (I.LE.1.OR.I.GE.MXG.OR.J.LE.1.OR.J.GE.MYG)
     $            .AND.JFLAG.EQ.0 ) THEN
                  CALL ERRMSG('MKIND4',6991)
                  WRITE(LP,*) 'ERROR: OFL-FILE:I,J VALUE IS INVALID'
                  WRITE(LP,*) '                I J FLAG=',I,J,IX
                  CALL ABORT1('')
               ENDIF
C
               IDUM=I
               JDUM=J
               IF(IX.EQ.1.OR.IX.EQ.3)THEN
                  CALL MODIJ(I,IDUM,J,JDUM,2,INSIDE)
               ELSEIF(IX.EQ.2.OR.IX.EQ.4)THEN
                  CALL MODIJ(I,IDUM,J,JDUM,3,INSIDE)
               ENDIF
               IF(INSIDE.EQ.0) CYCLE
            ENDIF
C
            IF( IX.EQ.1 .AND. IHONMA.GT.0 ) THEN
               M1=M1+1
               IF( IFLAG.EQ.1 ) THEN
                  LLOFL(1,M1)=I
                  LLOFL(2,M1)=J
                  LLOFL(3,M1)=1
               ENDIF
            ELSEIF( IX.EQ.2 .AND. IHONMA.GT.0 ) THEN   
               M2=M2+1
               IF( IFLAG.EQ.1 ) THEN
                  LLOFL(1,M2)=I
                  LLOFL(2,M2)=J
                  LLOFL(3,M2)=1
               ENDIF
            ELSEIF( IX.EQ.3 .AND.(IAIDA.GT.0.OR.IBKSTP.GT.0) ) THEN
               M1=M1+1
               IF( IFLAG.EQ.1 ) THEN
                  LLOFL(1,M1)=I
                  LLOFL(2,M1)=J
                  IF( HDEP(I,J).LT.HDEP(I+1,J) ) THEN
                     LLOFL(3,M1)=2
                  ELSE
                     LLOFL(3,M1)=3
                  ENDIF
               ENDIF
            ELSEIF( IX.EQ.4 .AND.(IAIDA.GT.0.OR.IBKSTP.GT.0) ) THEN
               M2=M2+1
               IF( IFLAG.EQ.1 ) THEN
                  LLOFL(1,M2)=I
                  LLOFL(2,M2)=J
                  IF( HDEP(I,J).LT.HDEP(I,J+1) ) THEN
                     LLOFL(3,M2)=2
                  ELSE
                     LLOFL(3,M2)=3
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
  250    CONTINUE
C
         IF( IFLAG.EQ.0 ) THEN
            MLOFLX=M1
            MLOFL =M1+M2
            IF( MLOFL.EQ.0.AND.JFLAG.EQ.0 ) THEN
               CALL ERRMSG('MKIND4',6992)
               WRITE(LP,*) 'ERROR: OFL-FILE:NO DATA ARE APPLIED'
               WRITE(LP,*) '       CHANGE FLAG DATA OF OFL-FILE, OR'
               IF( IHONMA.GT.0 )
     $         WRITE(LP,*) '       SET OVERFLOW-HONMA = OFF'
               IF( IAIDA.GT.0 )
     $         WRITE(LP,*) '       SET OVERFLOW-AIDA  = OFF'
               IF( IBKSTP.GT.0 )
     $         WRITE(LP,*) '       SET OVERFLOW-BKSTP = OFF'
               CALL ABORT1('')
            ENDIF
         ENDIF
         CLOSE(IFLOF)
C
C ...... 越流位置リストのチェックとHHOFLの設定
         IF( IFLAG.EQ.1 )THEN
            IERR=0
            DO M=1,MLOFL
               I=LLOFL(1,M)
               J=LLOFL(2,M)
               IX=LLOFL(3,M)
               IF( M.LE.MLOFLX ) CH='X'
               IF( M.GT.MLOFLX ) CH='Y'
C
               FXY=0.0D0
               FV1=0.0D0
               FV2=0.0D0
               DO K=2,MZM
                  IF( CH.EQ.'X' ) THEN
                     IF(INDU(I,J,K).GT.0) FXY=FXY+GX(I,J,K)*ZC(4,K)
                     IF(INDP(I,J,K).GT.0) FV1=FV1+GV(I,J,K)*ZC(4,K)
                     IF(INDP(I+1,J,K).GT.0) FV2=FV2+GV(I+1,J,K)*ZC(4,K)
                  ELSEIF( CH.EQ.'Y' ) THEN
                     IF(INDV(I,J,K).GT.0) FXY=FXY+GY(I,J,K)*ZC(4,K)
                     IF(INDP(I,J,K).GT.0) FV1=FV1+GV(I,J,K)*ZC(4,K)
                     IF(INDP(I,J+1,K).GT.0) FV2=FV2+GV(I,J+1,K)*ZC(4,K)
                  ELSE
                     CALL ERRMSG('MKIND4',6993)
                     WRITE(LP,*) 'X OR Y IS AVAILABLE FOR DIRECTION'
                     CALL ABORT1('')
                  ENDIF
               ENDDO
C
               IF( IX.EQ.2 ) THEN
                  IF( ABS(FXY-FV2).GT.EPF .OR. FV1.LT.FV2 ) THEN
                     IF(CH.EQ.'X') IY=3
                     IF(CH.EQ.'Y') IY=4
                     IF( JFLAG.EQ.0 )
     $               WRITE(LP,*) 'ERROR: OFL-FILE:I,J,FLAG=',I,J,IY
                     IERR=1
                  ELSE
                     HHOFL(M)=ZC(1,MZM)-FXY
                  ENDIF
               ELSEIF( IX.EQ.3 ) THEN
                  IF( ABS(FXY-FV1).GT.EPF .OR. FV2.LT.FV1 ) THEN
                     IF(CH.EQ.'X') I=I-1
                     IF(CH.EQ.'Y') J=J-1
                     IF(CH.EQ.'X') IY=3
                     IF(CH.EQ.'Y') IY=4
                     IF( JFLAG.EQ.0 )
     $               WRITE(LP,*) 'ERROR: OFL-FILE:I,J,FLAG=',I,J,IY
                     IERR=1
                  ELSE
                     HHOFL(M)=ZC(1,MZM)-FXY
                  ENDIF
               ELSEIF( IX.EQ.1 ) THEN
                  IF( FXY.GE.FV1 .OR. FXY.GE.FV2 ) THEN
                     IF(CH.EQ.'X') IY=1
                     IF(CH.EQ.'Y') IY=2
                     IF( JFLAG.EQ.0 )
     $               WRITE(LP,*) 'ERROR: OFL-FILE:I,J,FLAG=',I,J,IY
                     IERR=1
                  ELSE
                     HHOFL(M)=ZC(1,MZM)-FXY
                  ENDIF
               ENDIF
            ENDDO
            IF( IERR.EQ.1.AND.JFLAG.EQ.0 ) THEN
               CALL ERRMSG('MKIND4',6994)
               WRITE(LP,*) 'ERROR: OFL-FILE DATA IS NOT ',
     $                      'CONSISTENT WITH STR-FILE'
               CALL ABORT1('')
            ENDIF
         ENDIF
C
      ENDIF
C
      IF( IFLAG.EQ.0.OR.LFOBS.EQ.0) RETURN
C
C ... 浮上型防潮堤データのインデックス修正（LLWALBデータの再設定）
C     -メッシュサイズ以下の高さの防潮堤としていたものを除く-
C
      ALLOCATE (INDX(MX,MY,MZ),INDY(MX,MY,MZ),STAT=IERR)
      IF(IERR.NE.0) THEN
         CALL ERRMSG('MKIND4',6995)
         WRITE(LP,*)'CAN NOT ALLOCATE IN SUB. MKIND4'
         CALL ABORT1('')
      END IF
C
      CALL ZERCLI(INDX,MXYZ,0)
      CALL ZERCLI(INDY,MXYZ,0)
C
      DO 300 N=1,NPORS
         IS = IPORS(1,N)
         IE = IPORS(2,N)
         JS = IPORS(3,N)
         JE = IPORS(4,N)
         KS = IPORS(5,N)
         KE = IPORS(6,N)
         IF(IPORS(7,N).NE.1) GO TO 300
C
         DO 310 K=KS,KE
         DO 310 J=JS,JE
            INDX(IS-1,J,K) = 1
            DO 315 I=IS,IE-1
               INDX(IS,J,K) = 1
  315       CONTINUE
            INDX(IE,J,K) = 1
  310    CONTINUE
C
         DO 320 K=KS,KE
         DO 320 I=IS,IE
            INDY(I,JS-1,K) = 1
            DO 325 J=JS,JE-1
               INDY(I,J,K) = 1
  325       CONTINUE
            INDY(I,JE,K)   = 1
  320    CONTINUE
  300 CONTINUE
C
      M = 0
      DO 400 N=1,MLWALBX
         I = LLWALB(1,N)
         J = LLWALB(2,N)
         K = LLWALB(3,N)
         IF( INDX(I,J,K).EQ.0 ) THEN
            M = M+1
            LLWALB(1,M) = LLWALB(1,N)
            LLWALB(2,M) = LLWALB(2,N)
            LLWALB(3,M) = LLWALB(3,N)
         END IF
  400 CONTINUE
      NN = M
C     
      DO 410 N=MLWALBX+1,MLWALB
         I = LLWALB(1,N)
         J = LLWALB(2,N)
         K = LLWALB(3,N)
         IF( INDY(I,J,K).EQ.0 ) THEN
            M = M+1
            LLWALB(1,M) = LLWALB(1,N)
            LLWALB(2,M) = LLWALB(2,N)
            LLWALB(3,M) = LLWALB(3,N)
         END IF
  410 CONTINUE
C ... MLWALBの値は小くなったけれど、ALLOCATEしたサイズは大きいので値はそのまま
      MLWALBX = NN
      MLWALB = M
      DEALLOCATE (INDX,INDY)
C
      RETURN
C
C ... ファイルオープンエラー
  900 CONTINUE
      CALL ERRMSG('MKIND4',6996)
      WRITE(LP,*) 'FILE OPEN ERROR: OVERFLOW INPUT FILE'
      WRITE(LP,*) 'FILE NUMBER=',IFLOF
      CALL ABORT1('')
      END
