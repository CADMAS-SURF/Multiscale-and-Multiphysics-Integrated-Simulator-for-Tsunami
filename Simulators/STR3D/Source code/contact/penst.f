      SUBROUTINE PENST(ISLV,RSLV,IEDG,IELA,IFC,RL,P,RN,POS,EN,TOL1)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION RL(3),ISLV(2),RSLV(3),IELA(3),X(3,3),POS(3,*),IEDG(6,*)
     &         ,P(3,3),V(3),RN(3),EN(3)
      DATA TOL /75.D0/

      COST=DCOS(TOL/90.D0*DASIN(1.D0))

      CALL VECML1(COS,EN,RN,3)

      TOL2 = TOL1 * 1.D0

      RL_MIN = 1.1D0

      DO I = 1, 3
        IF( RL(I) < RL_MIN ) THEN
          I_MIN = I
          RL_MIN = RL(I)
        ENDIF
      ENDDO

      IF( COS > COST .OR. RL_MIN > TOL1 ) THEN
        ISLV(1) = 13
        ISLV(2) = IFC
        RSLV(:) = RL(:)
      ELSE
        IE = IELA(I_MIN)
        X(:,1:2) = POS(:,IEDG(1:2,IE))
        X(:,3) = RL(1) * P(:,1) + RL(2) * P(:,2) + RL(3) * P(:,3)
        CALL LENCD(RT,V,X)
        IF( RT < TOL2 ) THEN
          ISLV(1) = 11
          ISLV(2) = IEDG(1,IE)
        ELSEIF( RT > 1.D0 - TOL2 ) THEN
          ISLV(1) = 11
          ISLV(2) = IEDG(2,IE)
        ELSE
          ISLV(1) = 12
          ISLV(2) = IE
          RSLV(1) = RT
        ENDIF
      ENDIF

      END