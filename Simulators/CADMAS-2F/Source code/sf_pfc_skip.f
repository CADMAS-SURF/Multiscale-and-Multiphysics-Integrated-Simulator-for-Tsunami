      SUBROUTINE SF_PFC_SKIP(ISKIP,X,N,NP,GRID)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION XG(3),X(2,3),NP(N),GRID(3,*)

      XG(:) = 0.

      DO I = 1, N
        XG(:) = XG(:) + GRID(:,NP(I))
      ENDDO

      XG(:) = XG(:) / DBLE(N)

      IF( XG(1) >= X(1,1) .AND. XG(1) < X(2,1) .AND. 
     &    XG(2) >= X(1,2) .AND. XG(2) < X(2,2) .AND. 
     &    XG(3) >= X(1,3) .AND. XG(3) < X(2,3) ) THEN
        ISKIP = 0
      ELSE
        ISKIP = 1
      ENDIF

      END
