      SUBROUTINE GRAZEEL(ICHK,POSS,RL,IELC,ICELA,ICEL,IBTE,POS)
C
      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL VOLIN3
      DIMENSION RL(3),RN(4),IELC(3),NOD(2),POS(3,*),G(3,4),ICELA(2,*)
     &         ,ICEL(*),IBTE(4,*),POSS(3)
C----&------------------------------------------------------------------
      IF( RL(1) < 1.D-10 ) THEN
        NOD(1) = IELC(2)
        NOD(2) = IELC(3)
      ELSEIF( RL(2) < 1.D-10 ) THEN
        NOD(1) = IELC(3)
        NOD(2) = IELC(1)
      ELSEIF( RL(3) < 1.D-10 ) THEN
        NOD(1) = IELC(1)
        NOD(2) = IELC(2)
      ELSE
        ICHK = 1
        RETURN
      ENDIF
C
      DO I = 1, 2
        NODE = NOD(I)
        JS = ICELA(1,NODE)
        JE = ICELA(2,NODE)
        DO J = JS, JE
          K = ICEL(J)
          G(:,:) = POS( :, IBTE(:,K) )
          CALL VOLCD(RN,G,POSS)
          IF( VOLIN3(RN,1.D-10) )THEN
            ICHK = 1
            RETURN
          ENDIF
        ENDDO
      ENDDO
C
      ICHK = 0
C
      END
