      SUBROUTINE GMTXHX2(EKUP,EKPP,ECPU,ECPP,EMPU,ND,ND3,X,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION WG(3,3),XG(3,3),EKUP(ND3,ND),EKPP(ND,ND),ECPU(ND,ND3)
     &         ,ECPP(ND,ND),EMPU(ND,ND3),RN(ND),X(3,ND),DNDX(3,ND)
     &         ,RKUP(ND3,ND),RKPP(ND,ND),CPU(ND,ND3),CPP(ND,ND)
     &         ,RMPU(ND,ND3)
      DATA XG / 3*0.0D0
     &        , -.577350269189626D0, .577350269189626D0, 0.0D0
     &        , -.774596669241483D0, 0.0D0, .774596669241483D0 /
      DATA WG / 2.0D0, 2*0.0D0
     &        , 2*1.0D0, 0.0D0
     &        , 0.55555555555555556D0, 0.88888888888888889D0
     &        , 0.55555555555555556D0 /

      IF( ND == 8 ) THEN
        NG = 2
      ELSE
        NG = 3
      ENDIF

      EKUP(:,:) = 0.
      EKPP(:,:) = 0.
      ECPU(:,:) = 0.
      ECPP(:,:) = 0.
      EMPU(:,:) = 0.

      DO I = 1, NG
      DO J = 1, NG
      DO K = 1, NG

        XG1 = XG(I,NG)
        XG2 = XG(J,NG)
        XG3 = XG(K,NG)

        WGT = WG(I,NG)*WG(J,NG)*WG(K,NG)

        CALL SFNHX2(XG1,XG2,XG3,ND,RN)

        CALL DERXHX2(DNDX,DET,XG1,XG2,XG3,X,ND,ITO)

        CALL GMTX(RKUP,RKPP,CPU,CPP,RMPU,RN,DNDX,ND,ND3)

        EKUP(:,:) = EKUP(:,:) + RKUP(:,:)*WGT*DET
        EKPP(:,:) = EKPP(:,:) + RKPP(:,:)*WGT*DET
        ECPU(:,:) = ECPU(:,:) + CPU(:,:)*WGT*DET
        ECPP(:,:) = ECPP(:,:) + CPP(:,:)*WGT*DET
        EMPU(:,:) = EMPU(:,:) + RMPU(:,:)*WGT*DET

      ENDDO
      ENDDO
      ENDDO

      END
