      SUBROUTINE RFCFACE(RFCI,NS,IELC,IFCQ,IELQ,POS,F)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(3,4),POS(3,*),IELC(3),RL(3),E(3),RF(3),F(3),KN(4)
     &         ,RFCI(3,*),IELQ(4,*)
C-----------------------------------------------------------------------
      X(:,1:3) = POS(:,IELC(:))
      X(:,4)   = POS(:,NS)
C
      CALL AREACD(RL,E,H,X)
C
      RF(:) = DOT_PRODUCT(F,E) * E(:)
C
      IF( IFCQ == 0 ) THEN
C
        KN(1:3) = IELC(:)
C
        DO I = 1, 3
          RFCI(:,KN(I)) = RFCI(:,KN(I)) - RL(I) * RF(:)
        ENDDO
C
      ELSE
C
        KN(1:2) = IELC(1:2)
C
        IF( IELC(1) == IELQ(1,IFCQ) ) THEN
          KN(3)=IELQ(3,IFCQ)
          KN(4)=IELQ(4,IFCQ)
        ELSEIF( IELC(1) == IELQ(2,IFCQ) ) THEN
          KN(3)=IELQ(1,IFCQ)
          KN(4)=IELQ(4,IFCQ)
        ELSEIF( IELC(1) == IELQ(3,IFCQ) ) THEN
          KN(3)=IELQ(1,IFCQ)
          KN(4)=IELQ(2,IFCQ)
        ELSEIF( IELC(1) == IELQ(4,IFCQ) ) THEN
          KN(3)=IELQ(2,IFCQ)
          KN(4)=IELQ(3,IFCQ)
        ENDIF
C
        RFCI(:,KN(1)) = RFCI(:,KN(1)) - RL(1) * RF(:)
     &                    - .25D0*RL(3)*RF(:)
        RFCI(:,KN(2)) = RFCI(:,KN(2)) - RL(2) * RF(:)
     &                    - .25D0*RL(3)*RF(:)
        RFCI(:,KN(3)) = RFCI(:,KN(3)) - .25D0*RL(3)*RF(:)
        RFCI(:,KN(4)) = RFCI(:,KN(4)) - .25D0*RL(3)*RF(:)
C
      ENDIF
C
      END
