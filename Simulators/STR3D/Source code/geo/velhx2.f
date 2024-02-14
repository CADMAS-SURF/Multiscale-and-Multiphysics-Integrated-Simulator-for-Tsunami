      SUBROUTINE VELHX2(VEL,VELG,VELE,ND,NG)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XG(3,3),IG1(3,8),IG2(3,20),VELG(3,NG,NG,NG),VELE(3)
     &         ,VEL(3,ND)

      DATA XG / 3*0.0D0
     &        , -.577350269189626D0, .577350269189626D0, 0.0D0
     &        , -.774596669241483D0, 0.0D0, .774596669241483D0 /
      DATA IG1 / 1,1,1, 2,1,1, 2,2,1, 1,2,1,
     &           1,1,2, 2,1,2, 2,2,2, 1,2,2 /
      DATA IG2 / 1,1,1, 3,1,1, 3,3,1, 1,3,1,
     &           1,1,3, 3,1,3, 3,3,3, 1,3,3,
     &           2,1,1, 3,2,1, 2,3,1, 1,2,1,
     &           1,1,2, 3,1,2, 3,3,2, 1,3,2,
     &           2,1,3, 3,2,3, 2,3,3, 1,2,3 /

      FAC = 1.D0 / XG(NG,NG)

      DO I = 1, ND

        IF( ND == 8 ) THEN
          II = IG1(1,I)
          JJ = IG1(2,I)
          KK = IG1(3,I)
        ELSE
          II = IG2(1,I)
          JJ = IG2(2,I)
          KK = IG2(3,I)
       ENDIF

        VEL(:,I) = ( 1.D0 - FAC ) * VELE(:) + FAC * VELG(:,II,JJ,KK)

      ENDDO

      END
