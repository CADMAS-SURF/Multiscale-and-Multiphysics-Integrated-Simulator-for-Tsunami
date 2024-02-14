      SUBROUTINE DERXPN2(DNDX,DET,N,X,XG1,XG2,XG3,ITO)

!     DNDX : OUT : DNDX(i,j) = ∂Nj/∂xi
!     DET  : OUT : |J| ( Jij = ∂xj/∂ξi )
!     N    : IN  : 節点数
!     X    : IN  : 節点座標
!     XGi  : IN  : ξi

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(3,N),X(3,N),XJI(3,3),DNDX(3,N)

      CALL DERPN2(P,N,XG1,XG2,XG3)
      CALL JACOB3(N,P,X,XJI,DET,ITO)
      CALL AXB(DNDX,XJI,P,3,3,N)

      END
