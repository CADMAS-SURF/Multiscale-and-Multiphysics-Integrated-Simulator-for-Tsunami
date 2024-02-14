      SUBROUTINE DERQU2(PFNC,RINT,SINT,IEJO,NJ)
C
C     PFNC : OUT : PFNC(i,j) = ∂Nj/∂ξi
C     RINT : IN  : ξ (ξ1)
C     SINT : IN  : η (ξ2)
C     IEJO : IN  : 節点番号
C     NJ   : IN  : 節点数
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION PFNC(2,9),IEJO(8)
C
      C1=0.5
      RP=C1*(1.0D0+RINT)
      RM=C1*(1.0D0-RINT)
      SP=C1*(1.0D0+SINT)
      SM=C1*(1.0D0-SINT)
C     
      PFNC(1,1)=C1*SP
      PFNC(1,2)=-PFNC(1,1)
      PFNC(1,3)=-C1*SM
      PFNC(1,4)=-PFNC(1,3)
      PFNC(2,1)=C1*RP
      PFNC(2,2)=C1*RM
      PFNC(2,3)=-PFNC(2,2)
      PFNC(2,4)=-PFNC(2,1)
      IF(NJ.GT.4) THEN
        DO I=1,2
          DO J=5,8
            PFNC(I,J)=0.0
          ENDDO
        ENDDO
C
         S2=1.0-SINT*SINT
        IF(IEJO(5).GT.0) PFNC(1,5)=-2.0D0*RINT*SP  
        IF(IEJO(6).GT.0) PFNC(1,6)=-C1*S2
        IF(IEJO(7).GT.0) PFNC(1,7)=-2.0D0*RINT*SM  
        IF(IEJO(8).GT.0) PFNC(1,8)= C1*S2
        PFNC(1,1)=PFNC(1,1)-C1*(PFNC(1,5)+PFNC(1,8))
        PFNC(1,2)=PFNC(1,2)-C1*(PFNC(1,6)+PFNC(1,5))
        PFNC(1,3)=PFNC(1,3)-C1*(PFNC(1,7)+PFNC(1,6))
        PFNC(1,4)=PFNC(1,4)-C1*(PFNC(1,8)+PFNC(1,7))
C
         R2=1.0-RINT*RINT
        IF(IEJO(5).GT.0) PFNC(2,5)= C1*R2
        IF(IEJO(6).GT.0) PFNC(2,6)=-2.0D0*SINT*RM  
        IF(IEJO(7).GT.0) PFNC(2,7)=-C1*R2
        IF(IEJO(8).GT.0) PFNC(2,8)=-2.0D0*SINT*RP  
        PFNC(2,1)=PFNC(2,1)-C1*(PFNC(2,5)+PFNC(2,8))
        PFNC(2,2)=PFNC(2,2)-C1*(PFNC(2,6)+PFNC(2,5))
        PFNC(2,3)=PFNC(2,3)-C1*(PFNC(2,7)+PFNC(2,6))
        PFNC(2,4)=PFNC(2,4)-C1*(PFNC(2,8)+PFNC(2,7))
      END IF
C
      IF( NJ .EQ. 9 ) THEN
C
        PFNC(1,9) = -2.*RINT*S2
C
        PFNC(1,1) = PFNC(1,1) + .25*PFNC(1,9)
        PFNC(1,2) = PFNC(1,2) + .25*PFNC(1,9)
        PFNC(1,3) = PFNC(1,3) + .25*PFNC(1,9)
        PFNC(1,4) = PFNC(1,4) + .25*PFNC(1,9)
        PFNC(1,5) = PFNC(1,5) - .50*PFNC(1,9)
        PFNC(1,6) = PFNC(1,6) - .50*PFNC(1,9)
        PFNC(1,7) = PFNC(1,7) - .50*PFNC(1,9)
        PFNC(1,8) = PFNC(1,8) - .50*PFNC(1,9)
C
        PFNC(2,9) = -2.*SINT*R2
C
        PFNC(2,1) = PFNC(2,1) + .25*PFNC(2,9)
        PFNC(2,2) = PFNC(2,2) + .25*PFNC(2,9)
        PFNC(2,3) = PFNC(2,3) + .25*PFNC(2,9)
        PFNC(2,4) = PFNC(2,4) + .25*PFNC(2,9)
        PFNC(2,5) = PFNC(2,5) - .50*PFNC(2,9)
        PFNC(2,6) = PFNC(2,6) - .50*PFNC(2,9)
        PFNC(2,7) = PFNC(2,7) - .50*PFNC(2,9)
        PFNC(2,8) = PFNC(2,8) - .50*PFNC(2,9)
C
      ENDIF
C
      RETURN
      END
