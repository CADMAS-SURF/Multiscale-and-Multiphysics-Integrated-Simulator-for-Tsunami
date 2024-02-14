      SUBROUTINE TRNSMTX7(TE,GE,GRID,KN)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(3,3),GRID(3,*),KN(6),V12(3),V13(3),E(3,3),TE(3,3)
     &          ,GG(3,6),GE(3,6)
C-----------------------------------------------------------------------
      X(:,:) = ( GRID(:,KN(1:3)) + GRID(:,KN(4:6)) ) * .5D0
C
      V12(:) = X(:,2) - X(:,1)
      V13(:) = X(:,3) - X(:,1)
C
      CALL DIRCOS(E(1,1),V12,3)
      CALL CROSS2(E(1,1),V13,E(1,3))
      CALL CROSS2(E(1,3),E(1,1),E(1,2))
C
      DO I=1,3
        DO J=1,3
          TE(I,J)=E(J,I)
        ENDDO
      ENDDO
C
      DO I=1,6
        GG(:,I) = GRID(:,KN(I)) - X(:,1)
      ENDDO
C
      CALL AXB(GE,TE,GG,3,3,6)
C
      END
