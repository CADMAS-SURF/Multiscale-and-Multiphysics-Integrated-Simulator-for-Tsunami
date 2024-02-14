      SUBROUTINE SETFENCE(IFLAG)
C----------------------------------------
C     フェンス状構造物の高さを設定(地盤高＋入力値の場合)
C----------------------------------------
      USE M_GRID,ONLY:MXAREA,XG,YG,NI,NJ
      USE M_GEOM,ONLY:HT
      USE M_FENCE,ONLY:NFC,XFC1,YFC1,XFC2,YFC2,ZFC,KFC,LFC,DFC_LIMIT
      USE M_OUTPUT,ONLY:IFL
C
      IMPLICIT NONE
C
      INTEGER,INTENT(IN)::IFLAG
      INTEGER::N,NA,INFC
      REAL(8)::XX,YY,HF0,HT0
C
C
C----------------------------------------
C     (IFLAG=0)フェンス状構造物の高さを設定
C----------------------------------------
      IF( IFLAG.EQ.0 ) THEN
C
      DO N=1,NFC
C
C ...... フェンスの重心位置
         XX = 0.5D0*(XFC1(N)+XFC2(N))
         YY = 0.5D0*(YFC1(N)+YFC2(N))
C
C ...... 重心を含む領域を探す(子側から)
         INFC=0
         DO NA=MXAREA,1,-1
            IF( XX.GE.XG(NA,0) .AND. XX.LE.XG(NA,NI(NA)) .AND.
     &          YY.GE.YG(NA,0) .AND. YY.LE.YG(NA,NJ(NA)) ) THEN
               INFC = NA
               EXIT
            ENDIF
         ENDDO
C
         IF( INFC.EQ.0 ) THEN
            LFC(N)=-1
            WRITE(IFL,*) 'FENCE IS LOCATED OUT OF REGION'
            WRITE(IFL,*) '  FENCE NO.=',N
            WRITE(IFL,*) '  (X1,Y1)=',XFC1(N),YFC1(N)
            WRITE(IFL,*) '  (X2,Y2)=',XFC2(N),YFC2(N)
            CALL ERRMSG('SETFENCE',1)
C
         ELSE
            IF( KFC(N).EQ.0 ) THEN
               CALL INTERP_H(HF0,HT0,XX,YY,INFC) ! 重心位置における標高を補間
               ZFC(N)=ZFC(N)+HT0
            ENDIF
         ENDIF
C
      ENDDO
C
C----------------------------------------
C     (IFLAG=1) フェンス状構造物の浸水深による破壊を判定
C----------------------------------------
      ELSE
C
      DO N=1,NFC
         IF( LFC(N).NE.0 ) CYCLE
C
C ...... フェンスの重心位置
         XX = 0.5D0*(XFC1(N)+XFC2(N))
         YY = 0.5D0*(YFC1(N)+YFC2(N))
C
C ...... 重心を含む領域を探す(子側から)
         INFC=0
         DO NA=MXAREA,1,-1
            IF( XX.GE.XG(NA,0) .AND. XX.LE.XG(NA,NI(NA)) .AND.
     &          YY.GE.YG(NA,0) .AND. YY.LE.YG(NA,NJ(NA)) ) THEN
               INFC = NA
               EXIT
            ENDIF
         ENDDO
C
         IF( INFC.GE.1 ) THEN
            CALL INTERP_H(HF0,HT0,XX,YY,INFC) ! 重心位置における標高を補間
            IF( HF0-HT0.GT.DFC_LIMIT(N) ) THEN
               LFC(N)=1                       ! フェンス状構造物の破壊
               WRITE(IFL,*) 'FENCE IS DESTROYED BY WATER DEPTH.',
     $                      ' NO=',N
            ENDIF
         ELSE
            WRITE(IFL,*) 'UNEXPECTED ERROR'
            WRITE(IFL,*) '  FENCE NO.=',N
            WRITE(IFL,*) '       INFC=',INFC
            CALL ERRMSG('SETFENCE',-1)            
         ENDIF
C
      ENDDO

      ENDIF
C
      RETURN
      END
