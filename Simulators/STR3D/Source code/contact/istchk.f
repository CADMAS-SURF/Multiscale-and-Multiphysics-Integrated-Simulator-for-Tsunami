      SUBROUTINE ISTCHK(ICONV2,NNOD,NINDC,INDG,IELC,INDC,IEDG,ISLV,ISLVO
     &                 ,ISTEP,ITER2,MAXITER2,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*14 MSGCONV,BLK
      DIMENSION ISLVO(2,NINDC),ISLV(2,NINDC),MASTERO(3),MASTER(3)
     &         ,IEDG(6,*),IELC(3,*),INDC(NINDC),ISTMAT(4,4),INDG(NNOD)
C
      DATA BLK / '              ' /
      DATA ICHK / 0 /
C----&------------------------------------------------------------------
      ICONV2 = 1
C
      DO I = 1, NINDC
        IF( ISLV(1,I) == ISLVO(1,I) ) CYCLE
        ICONV2 = 0
        EXIT
      ENDDO
C
      IF( ICONV2 == 1 ) THEN
        MSGCONV = 'CONVERGED!'
      ELSEIF( ITER2 == MAXITER2 ) THEN
        MSGCONV = 'NOT CONVERGED!'
      ELSE
        MSGCONV = BLK
      ENDIF
C
      ISTMAT(:,:) = 0
C
      DO I = 1, NINDC
        IOLD = ISLVO(1,I) + 1
        IF( IOLD >= 5 ) IOLD = IOLD - 2
        INEW = MOD(ISLV(1,I),10) + 1
        ISTMAT(IOLD,INEW) = ISTMAT(IOLD,INEW) + 1
      ENDDO
C
      WRITE(ITO,'(/A,I0,2X,A//A)') 
     &  '   * CONTACT ITER. = ', ITER2, MSGCONV,
     &  '             FREE  POINT   EDGE   FACE'
C
      WRITE(ITO,'(A,4I7)') '      FREE', ISTMAT(1,:)
      WRITE(ITO,'(A,4I7)') '     POINT', ISTMAT(2,:)
      WRITE(ITO,'(A,4I7)') '      EDGE', ISTMAT(3,:)
      WRITE(ITO,'(A,4I7)') '      FACE', ISTMAT(4,:)
C
      IF( ICHK == 0 ) RETURN
C
      WRITE(51,'(A,I6,I3)') 'STEP, ITER2 = ',ISTEP, ITER2
C
      MAX_INDG = MAXVAL( INDG )
C
      DO I = 1, NINDC
C
        IF( ISLV(1,I) == ISLVO(1,I) ) CYCLE
C
        MASTERO(:) = 0
        MASTER(:) = 0
C
        SELECT CASE( ISLVO(1,I) )
        CASE( 1 )
          MASTERO(1) = ISLVO(2,I)
        CASE( 2, 4 )
          MASTERO(1:2) = IEDG(1:2,ISLVO(2,I))
        CASE( 3, 5 )
          MASTERO(1:3) = IELC(1:3,ISLVO(2,I))
        END SELECT
C
        SELECT CASE( ISLV(1,I) )
        CASE( 1, 11 )
          MASTER(1) = ISLV(2,I)
        CASE( 2, 12 )
          MASTER(1:2) = IEDG(1:2,ISLV(2,I))
        CASE( 3, 13 )
          MASTER(1:3) = IELC(1:3,ISLV(2,I))
        END SELECT
C
        ISLAVE = INDG( INDC(I) )
C
        DO J = 1, 3
          IF( MASTERO(J) > 0 .AND. MASTERO(J) <= NNOD ) THEN
            MASTERO(J) = INDG( MASTERO(J) )
          ELSEIF( MASTERO(J) > NNOD ) THEN
            MASTERO(J) = MAX_INDG + MASTERO(J) - NNOD
          ENDIF
          IF( MASTER(J) > 0 .AND. MASTER(J) <= NNOD ) THEN
            MASTER(J) = INDG( MASTER(J) )
          ELSEIF( MASTER(J) > NNOD ) THEN
            MASTER(J) = MAX_INDG + MASTER(J) - NNOD
          ENDIF
        ENDDO
C
        WRITE(51,'(I8,I6,4I8,I6,4I8)') 
     &    ISLAVE,ISLVO(:,I),MASTERO(:),ISLV(:,I),MASTER(:)
C
      ENDDO
C
      END