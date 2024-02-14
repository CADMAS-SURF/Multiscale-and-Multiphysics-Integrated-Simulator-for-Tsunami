      SUBROUTINE GMTXTE1(EKUP,EKPP,ECPU,ECPP,EMPU,X,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION EKUP(12,4),EKPP(4,4),ECPU(4,12),ECPP(4,4),EMPU(4,12)
     &         ,RN(4),X(3,4),DNDX(3,4)

      RN(:) = .25D0

      CALL DERXTE1(DNDX,DET,X,ITO)

      CALL GMTX(EKUP,EKPP,ECPU,ECPP,EMPU,RN,DNDX,4,12)

      EKUP(:,:) = EKUP(:,:)*DET/6.D0
      EKPP(:,:) = EKPP(:,:)*DET/6.D0
      ECPU(:,:) = ECPU(:,:)*DET/6.D0
      ECPP(:,:) = ECPP(:,:)*DET/6.D0
      EMPU(:,:) = EMPU(:,:)*DET/6.D0

      END
