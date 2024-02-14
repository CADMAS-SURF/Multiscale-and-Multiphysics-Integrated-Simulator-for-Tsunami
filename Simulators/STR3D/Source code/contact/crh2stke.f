      SUBROUTINE CRH2STKE(CRH,NS,IG,IEDQ,POS)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(3,3),POS(3,*),IG(2),CRH(*),V(3)

      X(:,1:2) = POS(:,IG(:))
      X(:,3) = POS(:,NS)

      CALL LENCD(T,V,X)
      TM = 1.D0 - T

      IF( IEDQ == 0 ) THEN

        CRH(1) = TM
        CRH(2) = T
        CRH(3) = TM
        CRH(4) = T
        CRH(5) = TM
        CRH(6) = T

      ELSE

        CRH(1)  = TM + .25D0 * T
        CRH(2)  = .25D0 * T
        CRH(3)  = .25D0 * T
        CRH(4)  = .25D0 * T
        CRH(5)  = TM + .25D0 * T
        CRH(6)  = .25D0 * T
        CRH(7)  = .25D0 * T
        CRH(8)  = .25D0 * T
        CRH(9)  = TM + .25D0 * T
        CRH(10) = .25D0 * T
        CRH(11) = .25D0 * T
        CRH(12) = .25D0 * T

      ENDIF

      END
