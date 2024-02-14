      SUBROUTINE STRPN1(EPS0,SIG0,EPSG,SIGG,GRID,KN)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION TE(3,3),TE1(6,6),TE2(6,6),X(3,6),GRID(3,*),KN(6)
     &         ,EPS(6,6),SIG(6,6),EPSG(6,3,2),SIGG(6,3,2),EPS0(6,6)
     &         ,SIG0(6,6)
C-----------------------------------------------------------------------
      CALL TRNSMTX7(TE,X,GRID,KN)
C
      CALL CHTNSR(TE,TE1,TE2)
C
      CALL INTPLPN1(EPS,EPSG)
      CALL INTPLPN1(SIG,SIGG)
C
      CALL ATXB(EPS0,TE1,EPS,6,6,6)
      CALL ATXB(SIG0,TE2,SIG,6,6,6)
C
      END
