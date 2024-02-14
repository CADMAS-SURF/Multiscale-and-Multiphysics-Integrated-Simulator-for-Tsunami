      SUBROUTINE RFCEDGE(RFCI,NS,IG,IEDQ,IELQ,POS,F)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(3,3),POS(3,*),IG(2),RL(2),E(3),V(3),RF(3),F(3),KN(4)
     &         ,RFCI(3,*),IELQ(4,*)
C-----------------------------------------------------------------------
      X(:,1:2) = POS(:,IG(:))
      X(:,3)   = POS(:,NS)
C
      CALL LENCD(RL(2),V,X)
      RL(1) = 1.D0 - RL(2)
C
      E(:) = X(:,2) - X(:,1)
      CALL DIRCOS(E,E,3)
C
      RF(:) = F(:) - DOT_PRODUCT(F,E) * E(:)
C
      IF( IEDQ == 0 ) THEN
C
        KN(1:2) = IG(:)
C
        DO I = 1, 2
          RFCI(:,KN(I)) = RFCI(:,KN(I)) - RL(I) * RF(:)
        ENDDO
C
      ELSE
C
        KN(1) = IG(1)
C
        IF( IG(1) == IELQ(1,IEDQ) ) THEN
          KN(2)=IELQ(2,IEDQ)
          KN(3)=IELQ(3,IEDQ)
          KN(4)=IELQ(4,IEDQ)
        ELSEIF( IG(1) == IELQ(2,IEDQ) ) THEN
          KN(2)=IELQ(1,IEDQ)
          KN(3)=IELQ(3,IEDQ)
          KN(4)=IELQ(4,IEDQ)
        ELSEIF( IG(1) == IELQ(3,IEDQ) ) THEN
          KN(2)=IELQ(1,IEDQ)
          KN(3)=IELQ(2,IEDQ)
          KN(4)=IELQ(4,IEDQ)
        ELSEIF( IG(1) == IELQ(4,IEDQ) ) THEN
          KN(2)=IELQ(1,IEDQ)
          KN(3)=IELQ(2,IEDQ)
          KN(4)=IELQ(3,IEDQ)
        ENDIF
C
        RFCI(:,KN(1)) = RFCI(:,KN(1)) - RL(1) * RF(:)
     &                    - .25D0*RL(2)*RF(:)
        RFCI(:,KN(2)) = RFCI(:,KN(2)) - .25D0*RL(2)*RF(:)
        RFCI(:,KN(3)) = RFCI(:,KN(3)) - .25D0*RL(2)*RF(:)
        RFCI(:,KN(4)) = RFCI(:,KN(4)) - .25D0*RL(2)*RF(:)
C
      ENDIF
C
      END
