      SUBROUTINE FACESTK(ISLV,RSLV,POSS,MA,IELC,POS,EPS)

      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL AREAIN
      DIMENSION X(3,4),POS(3,*),IELC(3),RL(3),E(3),ISLV(2),RSLV(3)
     &         ,POSS(3)

      X(:,1:3) = POS(:,IELC(:))
      X(:,4) = POSS(:)

      CALL AREACD(RL,E,H,X)

      IF( AREAIN(RL,0.D0) .AND. DABS(H) < EPS ) THEN
        ISLV(1) = 15
        ISLV(2) = MA
        RSLV(:) = RL(:)
      ENDIF

      END
