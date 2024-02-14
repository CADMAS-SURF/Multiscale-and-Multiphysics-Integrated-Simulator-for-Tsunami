      SUBROUTINE BDFPN2(FC,X,GRAV,R0,IGR,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(3,15),RG(4),SG(4),TG(3),X(3,15),XJI(3,3),H(15),VEC(3)
     &         ,GRAV(3),R0(3),FC(3,15),WGI(4),WGJ(3)
C
      DATA RG / 0.2D0, 0.6D0, 0.2D0, 0.333333333333333D0 /
      DATA SG / 0.2D0, 0.2D0, 0.6D0, 0.333333333333333D0 /
      DATA TG / -.774596669241483D0, 0.0D0, .774596669241483D0 /
C
      DATA WGI / 3*0.260416666666667D0, -0.28125D0 /
      DATA WGJ / 0.55555555555555556D0, 0.88888888888888889D0
     &         , 0.55555555555555556D0 /
C-----------------------------------------------------------------------
      DO JG = 1, 3
        DO IG = 1, 4
C
          CALL DERPN2(P,15,RG(IG),SG(IG),TG(JG))
          CALL JACOB3(15,P,X,XJI,DET,ITO)
C
          CALL SFNPN2(H,15,RG(IG),SG(IG),TG(JG))
C
          DO I = 1, 15
C
            IF( IGR == 1 ) THEN
              VEC(:) = GRAV(:)
            ELSE
              VEC(:) = X(:,I) - R0(:)
            ENDIF
C
            FC(:,I) = FC(:,I) + H(I) * VEC(:) * DET * WGI(IG) * WGJ(JG)
C
          ENDDO
C
        ENDDO
      ENDDO
C
      END
