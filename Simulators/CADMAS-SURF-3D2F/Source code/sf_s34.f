      SUBROUTINE SF_S34(S,X,N)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION X(2,N),R12(2),R13(2),R14(2)

      IF( N == 3 ) THEN
        R12(:) = X(:,2) - X(:,1)
        R13(:) = X(:,3) - X(:,1)
        S = .5D0 * ( R12(1)*R13(2) - R12(2)*R13(1) )
      ELSEIF( N == 4 ) THEN
        R12(:) = X(:,2) - X(:,1)
        R13(:) = X(:,3) - X(:,1)
        R14(:) = X(:,4) - X(:,1)
        S = .5D0 * ( R12(1)*R13(2) - R12(2)*R13(1) )
     &    + .5D0 * ( R13(1)*R14(2) - R13(2)*R14(1) )
      ENDIF

!     IF( S < 0. ) CALL VF_A2ERR('SF_S34','P.G ERROR.')
    
      END