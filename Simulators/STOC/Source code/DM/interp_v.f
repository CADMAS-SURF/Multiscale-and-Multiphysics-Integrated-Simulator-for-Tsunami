      SUBROUTINE INTERP_V(UF0,VF0,X1,Y1,K1,K2,IFLAG,NA)
C----------------------------------------
C     Kの各高さレベルにおいて、
C     指定した座標(X,Y)における流体の速度を補間して返す
C
C     IFLAGが0のときは、n時刻の流速を補間する
C            1のときは、(n+1)時刻の流速を補間する
C----------------------------------------
      USE M_GRID
      USE M_FLUID
C
      IMPLICIT NONE
C
      REAL(8),INTENT(OUT)::UF0(MXNK),VF0(MXNK)
      REAL(8),INTENT(IN)::X1,Y1
      INTEGER,INTENT(IN)::K1,K2,IFLAG,NA
C
      INTEGER::I,J,K
      REAL(8)::XX,YY,CX,CY
C
      INTEGER::NNN1,NNN2,NNN3,NNN4
C
C
      XX = X1
      YY = Y1
C
      IF( XX.LT.XC(NA,1)  ) THEN
         XX = XC(NA,1)
         I = 2
      ELSEIF( XX.GE.XC(NA,NI(NA)) ) THEN
         XX = XC(NA,NI(NA))
         I = NI(NA)
      ELSE
         DO I=2,NI(NA)
            IF( XX.GE.XC(NA,I-1).AND.XX.LT.XC(NA,I) ) EXIT
         ENDDO
      ENDIF
      CX = (XC(NA,I)-XX)/DX(NA)
C
      IF( YY.LT.YC(NA,1)  ) THEN
         YY = YC(NA,1)
         J = 2
      ELSEIF( YY.GE.YC(NA,NJ(NA)) ) THEN
         YY = YC(NA,NJ(NA))
         J  = NJ(NA)
      ELSE
         DO J=2,NJ(NA)
            IF( YY.GE.YC(NA,J-1).AND.YY.LT.YC(NA,J) ) EXIT
         ENDDO
      ENDIF
      CY = (YC(NA,J)-YY)/DY(NA)
C
      IF( IFLAG.EQ.0 ) THEN
         DO K=K1,K2
            NNN1 = NI(NA)*NJ(NA) * (K-1) + (NI(NA) * ((J-1)-1) + (I-1))
            NNN2 = NI(NA)*NJ(NA) * (K-1) + (NI(NA) * ((J-1)-1) + (I  ))
            NNN3 = NI(NA)*NJ(NA) * (K-1) + (NI(NA) * ((J  )-1) + (I-1))
            NNN4 = NI(NA)*NJ(NA) * (K-1) + (NI(NA) * ((J  )-1) + (I  ))
C
            UF0(K) = CX        *CY        *UFAR(NA,NNN1)
     $             + (1.0D0-CX)*CY        *UFAR(NA,NNN2)
     $             + CX        *(1.0D0-CY)*UFAR(NA,NNN3)
     $             + (1.0D0-CX)*(1.0D0-CY)*UFAR(NA,NNN4)
C
            VF0(K) = CX        *CY        *VFAR(NA,NNN1)
     $             + (1.0D0-CX)*CY        *VFAR(NA,NNN2)
     $             + CX        *(1.0D0-CY)*VFAR(NA,NNN3)
     $             + (1.0D0-CX)*(1.0D0-CY)*VFAR(NA,NNN4)
         ENDDO
      ELSE
         DO K=K1,K2
            NNN1 = NI(NA)*NJ(NA) * (K-1) + (NI(NA) * ((J-1)-1) + (I-1))
            NNN2 = NI(NA)*NJ(NA) * (K-1) + (NI(NA) * ((J-1)-1) + (I  ))
            NNN3 = NI(NA)*NJ(NA) * (K-1) + (NI(NA) * ((J  )-1) + (I-1))
            NNN4 = NI(NA)*NJ(NA) * (K-1) + (NI(NA) * ((J  )-1) + (I  ))
C
            UF0(K) = CX        *CY        *UFAR1(NA,NNN1)
     $             + (1.0D0-CX)*CY        *UFAR1(NA,NNN2)
     $             + CX        *(1.0D0-CY)*UFAR1(NA,NNN3)
     $             + (1.0D0-CX)*(1.0D0-CY)*UFAR1(NA,NNN4)
C
            VF0(K) = CX        *CY        *VFAR1(NA,NNN1)
     $             + (1.0D0-CX)*CY        *VFAR1(NA,NNN2)
     $             + CX        *(1.0D0-CY)*VFAR1(NA,NNN3)
     $             + (1.0D0-CX)*(1.0D0-CY)*VFAR1(NA,NNN4)
         ENDDO
      ENDIF
C
      RETURN
      END
