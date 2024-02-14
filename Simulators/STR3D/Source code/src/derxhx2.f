      SUBROUTINE DERXHX2(DNDX,DET,XG1,XG2,XG3,X,N,ITO)

!     DNDX : OUT : DNDX(i,j) = ∂Nj/∂xi
!     DET  : OUT : |J| ( Jij = ∂xj/∂ξi )
!     XGi  : IN  : ξi
!     X    : IN  : 節点座標
!     N    : IN  : 節点数

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION DNDX(3,N),X(3,N),P(3,N),XJI(3,3)

      CALL DERHX2(XG1,XG2,XG3,N,P)
      CALL JACOB3(N,P,X,XJI,DET,ITO)
      CALL AXB(DNDX,XJI,P,3,3,N)

      END
