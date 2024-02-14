      SUBROUTINE DERXHX1(DNDX,DETG,XG1,XG2,XG3,X,ITO)

!     DNDX : OUT : DNDX(i,j,1) = ∂Nj/∂xi at (XG1,XG2,XG3)
!                  DNDX(i,j,2) = ∂Nj/∂xi at (0,0,XG3)
!                  DNDX(i,j,3) = ∂Nj/∂xi at (XG1,0,0)
!                  DNDX(i,j,4) = ∂Nj/∂xi at (0,XG2,0)
!     DETG : OUT : |J| ( Jij = ∂xj/∂ξi ) at (XG1,XG2,XG3)
!     XGi  : IN  : ξi
!     X    : IN  : 節点座標

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION DNDX(3,8,4),X(3,8),XG(3),P(3,8),XJI(3,3)

      DO I = 1, 4

        SELECT CASE( I )
        CASE( 1 )
          XG(1) = XG1
          XG(2) = XG2
          XG(3) = XG3
        CASE( 2 )
          XG(1) = 0.
          XG(2) = 0.
          XG(3) = XG3
        CASE( 3 )
          XG(1) = XG1
          XG(2) = 0.
          XG(3) = 0.
        CASE( 4 )
          XG(1) = 0.
          XG(2) = XG2
          XG(3) = 0.
        END SELECT

        CALL DERHX2(XG(1),XG(2),XG(3),8,P)
        CALL JACOB3(8,P,X,XJI,DET,ITO)
        IF( I == 1 ) DETG = DET
        CALL AXB(DNDX(1,1,I),XJI,P,3,3,8)

      ENDDO      

      END
