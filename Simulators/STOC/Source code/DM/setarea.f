      SUBROUTINE SETAREA
C----------------------------------------
C     漂流物が存在する領域番号を設定
C----------------------------------------
      USE M_GRID
      USE M_DRIFT
C
      IMPLICIT NONE
C
      REAL(8)::XMIN,XMAX,YMIN,YMAX
      INTEGER::N,NA,IFLAG
C
C
C
      DO N=1,ND
C
      XMIN = MIN(XCOLLIS(1,N),XCOLLIS(2,N)) - 0.5D0*BB(N)
      XMAX = MAX(XCOLLIS(1,N),XCOLLIS(2,N)) + 0.5D0*BB(N)
      YMIN = MIN(YCOLLIS(1,N),YCOLLIS(2,N)) - 0.5D0*BB(N)
      YMAX = MAX(YCOLLIS(1,N),YCOLLIS(2,N)) + 0.5D0*BB(N)
C
      IFLAG=0
         DO NA=MXAREA,1,-1
            IF( XMIN.GE.XG(NA,0) .AND. XMAX.LE.XG(NA,NI(NA)) .AND.
     &          YMIN.GE.YG(NA,0) .AND. YMAX.LE.YG(NA,NJ(NA)) ) THEN
               INAR(N) = NA
               IFLAG=1
               EXIT
            ENDIF
         ENDDO
CCC      IF( IFLAG.EQ.0 ) GOTO 900
C
      ENDDO
C
C
      RETURN
  900 CONTINUE
      WRITE(*,*) 'ERROR: DRIFT OBJECT No.',N,' IS OUT OF BOUNDS'
      CALL ERRMSG('SETAREA',-1)
      END
