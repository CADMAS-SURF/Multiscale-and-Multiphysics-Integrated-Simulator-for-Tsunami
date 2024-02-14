      SUBROUTINE ISTCHK0(ICONV2,NNOD,NINDC,INDG,IELC,INDC,IEDG,ISLV
     &                  ,ISLVO,ISLVP,ISLVPO,NN_EXT,NN_EXTC,NOD,MN,NODG
     &                  ,MG,IEG,MIEG,IEC,MIEC,ISTEP,ITER2,MAXITER2,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*14 MSGCONV,BLK
      DIMENSION ISLVO(2,NINDC),ISLV(2,NINDC),MASTERO(3),MASTER(3)
     &         ,IEDG(6,*),IELC(3,*),INDC(NINDC),ISTMAT(4,4),INDG(NNOD)
     &         ,ISLVPO(NINDC),ISLVP(NINDC),NN_EXT(*),NN_EXTC(*)
     &         ,NOD(MN,*),NODG(MG,*),IEG(MIEG,*),IEC(MIEC,*)
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
      WRITE(51,'(A,2I3)') 'STEP, ITER2 = ',ISTEP, ITER2
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
        IST = ISLVO(1,I)
        MA  = ISLVO(2,I)
        IP  = ISLVPO(I)
C
        SELECT CASE( IST )
        CASE( 1 )
          IF( MA <= NN_EXT(IP) + NN_EXTC(IP) ) THEN
            MAG = NOD(MA,IP)
          ELSE
            MA = MA - ( NN_EXT(IP) + NN_EXTC(IP) )
            MAG = NODG(MA,IP)
          ENDIF
          MASTERO(1) = MAG
        CASE( 2, 4 )
          MAG = IEG(MA,IP)
          MASTERO(1:2) = IEDG(1:2,MAG)
        CASE( 3, 5 )
          MAG = IEC(MA,IP)
          MASTERO(1:3) = IELC(1:3,MAG)
        END SELECT
C
        IST = ISLV(1,I)
        MA  = ISLV(2,I)
        IP  = ISLVP(I)
C
        SELECT CASE( IST )
        CASE( 1, 11 )
          IF( MA <= NN_EXT(IP) + NN_EXTC(IP) ) THEN
            MAG = NOD(MA,IP)
          ELSE
            MA = MA - ( NN_EXT(IP) + NN_EXTC(IP) )
            MAG = NODG(MA,IP)
          ENDIF
          MASTER(1) = MAG
        CASE( 2, 12 )
          MAG = IEG(MA,IP)
          MASTER(1:2) = IEDG(1:2,MAG)
        CASE( 3, 13 )
          MAG = IEC(MA,IP)
          MASTER(1:3) = IELC(1:3,MAG)
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