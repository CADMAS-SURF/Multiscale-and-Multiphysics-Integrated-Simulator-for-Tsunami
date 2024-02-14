      SUBROUTINE ENPFADDM(FT,FTD,EMASS,UG,KN,NDF,ND,CM)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION FT(6,*),FTD(6,*),FC(NDF,20),EMASS(*),UE(NDF,20),UG(6,*)
     &         ,KN(ND)
C-----------------------------------------------------------------------
      N = NDF * ND

      UE(:,1:ND) = UG(1:NDF,KN(:))

      CALL HARFB(FC,EMASS,UE,N,N,1)

      FT(1:NDF,KN(:)) = FT(1:NDF,KN(:)) + FC(:,1:ND)
      FTD(1:NDF,KN(:)) = FTD(1:NDF,KN(:)) + CM * FC(:,1:ND)

      END