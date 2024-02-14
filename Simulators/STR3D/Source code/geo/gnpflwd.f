      SUBROUTINE GNPFLWD(FT,ELHM,DPG,NP,N)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION FT(6,*),ELHM(3*N,N),DPG(*),NP(N),DP(N),DF(3,N)

      DP(:) = DPG(NP(:))

      CALL AXB(DF,ELHM,DP,3*N,N,1)

      FT(1:3,NP(:)) = FT(1:3,NP(:)) + DF(:,:)

      END
