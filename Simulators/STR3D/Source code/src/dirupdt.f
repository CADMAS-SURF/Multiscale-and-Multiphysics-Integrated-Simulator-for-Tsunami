      SUBROUTINE DIRUPDT(VG,VG0,NP,DUG,IGNL)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION THETA(3),NP(2),DUG(6,*),V(3,2),DV(3,2),VG(3,2,2)
     &         ,VG0(3,2,2),R(3,3),E(3,3),P(3,3),P2(3,3)
      DATA E / 1.D0, 0.D0, 0.D0,
     &         0.D0, 1.D0, 0.D0, 
     &         0.D0, 0.D0, 1.D0 / 
C-----------------------------------------------------------------------
      DO K = 1, 2
C
        THETA(:) = DUG(4:6,NP(K))
C
        OMEGA = DSQRT( DOT_PRODUCT(THETA,THETA) )
C
        IF( OMEGA == 0. ) CYCLE
C
        P(:,:) = 0.
        P(1,2) = -THETA(3)
        P(1,3) =  THETA(2)
        P(2,3) = -THETA(1)
        P(2,1) = -P(1,2)
        P(3,1) = -P(1,3)
        P(3,2) = -P(2,3)
C
        IF( IGNL > 0 ) THEN
C
          CALL AXB(P2,P,P,3,3,3)
C
          R(:,:) = E(:,:) + DSIN(OMEGA)/OMEGA*P(:,:) 
     &             + .5D0*( DSIN(.5D0*OMEGA) / (.5D0*OMEGA) )**2.D0
     &               *P2(:,:)
C
          V(:,:) = VG(:,:,K)
          CALL AXB(VG(1,1,K),R,V,3,3,2)
C
        ELSE
C
          CALL AXB(DV,P,VG0(1,1,K),3,3,2)
          VG(:,:,K) = VG(:,:,K) + DV(:,:)
C
        ENDIF
C
      ENDDO
C
      END
