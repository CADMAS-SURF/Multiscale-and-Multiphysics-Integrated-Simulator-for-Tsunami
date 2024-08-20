      SUBROUTINE SF_NRMVEC(E,NP,N,GRID)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(3,4),GRID(3,*),NP(N),R(3),S(3),E(3)

      X(:,1:N) = GRID(:,NP(:))

      SELECT CASE( N )
      CASE( 3 )
        R(:) = X(:,2) - X(:,1)
        S(:) = X(:,3) - X(:,1)
      CASE( 4 )
        R(:) = .25D0 * ( -X(:,1) + X(:,2) + X(:,3) - X(:,4) )
        S(:) = .25D0 * ( -X(:,1) - X(:,2) + X(:,3) + X(:,4) )
      END SELECT

      CALL SF_CROSS2(R,S,E)

      END
