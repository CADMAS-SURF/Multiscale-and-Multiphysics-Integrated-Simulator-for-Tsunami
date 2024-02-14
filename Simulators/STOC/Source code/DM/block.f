      SUBROUTINE BLOCK
C----------------------------------------
C     閉塞格子の判定を行う
C----------------------------------------
      USE M_GRID
      USE M_DRIFT
      USE M_GEOM
C
      IMPLICIT NONE
      INTEGER,PARAMETER::MXLIST=1000
C
      INTEGER N,M,I,J,IERR,NA
      INTEGER NLIST,LIST(2,MXLIST)
C
C----------------------------------------
C     (A) 閉塞格子の判定
C----------------------------------------
      KBLC = 0
C
      DO N=1,ND
         IF( LBLC(N).EQ.0 ) CYCLE
         CALL MKDLIST(NLIST,LIST,
     &                XCOLLIS(1,N),YCOLLIS(1,N),
     &                XCOLLIS(2,N),YCOLLIS(2,N),0.5D0*BB(N),
     &                MXLIST,IERR,INAR(N))
cs 2014/03/25 honda
         IF( IERR.EQ.-2 .AND. INAR(N).EQ.1 ) THEN
            LD(N) = -2
c            WRITE(*,*) '   SHIP NO.=',N
            CYCLE
         ENDIF
ce 2014/03/25 honda
         DO M=1,NLIST
            I = LIST(1,M)
            J = LIST(2,M)
            NA = INAR(N)
            KBLC(NA,I,J) = KZD(N)
         ENDDO
      ENDDO
C
C----------------------------------------
      RETURN
      END
