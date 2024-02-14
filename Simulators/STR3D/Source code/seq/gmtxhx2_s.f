      SUBROUTINE GMTXHX2_S(EKPP,ECPP,ND,NN,X,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION WG(3,3),XG(3,3),EKPP(NN),ECPP(NN),RN(ND),X(3,ND)
     &         ,DNDX(3,ND),RKPP(NN),CPP(NN)
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

      EKPP(:) = 0.
      ECPP(:) = 0.

      DO I = 1, NG
      DO J = 1, NG
      DO K = 1, NG

        XG1 = XG(I,NG)
        XG2 = XG(J,NG)
        XG3 = XG(K,NG)

        WGT = WG(I,NG)*WG(J,NG)*WG(K,NG)

        CALL SFNHX2(XG1,XG2,XG3,ND,RN)

        CALL DERXHX2(DNDX,DET,XG1,XG2,XG3,X,ND,ITO)

        CALL MATML(RKPP,1,DNDX,3,DNDX,2,ND,ND,3)
        CALL MATML(CPP,1,RN,3,RN,2,ND,ND,1)

        EKPP(:) = EKPP(:) + RKPP(:)*WGT*DET
        ECPP(:) = ECPP(:) + CPP(:)*WGT*DET

      ENDDO
      ENDDO
      ENDDO

      END
