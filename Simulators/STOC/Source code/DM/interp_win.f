      SUBROUTINE INTERP_WIN(WX0,WY0,X1,Y1,NA)
C----------------------------------------
C     指定した座標における風速WX,WYを返す
C----------------------------------------
      USE M_GRID
      USE M_FLUID
      USE M_GEOM
C
      IMPLICIT NONE
C
      REAL(8),INTENT(OUT)::WX0,WY0
      REAL(8),INTENT(IN)::X1,Y1
      INTEGER,INTENT(IN)::NA
C
      INTEGER::I,J
      REAL(8)::XX,YY,CX,CY
C
      INTEGER::NNN1,NNN2,NNN3,NNN4
C
C
      XX = X1
      YY = Y1
C
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
      NNN1 = NI(NA) * ((J-1)-1) + (I-1)
      NNN2 = NI(NA) * ((J-1)-1) + (I  )
      NNN3 = NI(NA) * ((J  )-1) + (I-1)
      NNN4 = NI(NA) * ((J  )-1) + (I  )
C
      WX0 = CX        *CY        *WXAR(NA,NNN1)
     $    + (1.0D0-CX)*CY        *WXAR(NA,NNN2)
     $    + CX        *(1.0D0-CY)*WXAR(NA,NNN3)
     $    + (1.0D0-CX)*(1.0D0-CY)*WXAR(NA,NNN4)
C
      WY0 = CX        *CY        *WYAR(NA,NNN1)
     $    + (1.0D0-CX)*CY        *WYAR(NA,NNN2)
     $    + CX        *(1.0D0-CY)*WYAR(NA,NNN3)
     $    + (1.0D0-CX)*(1.0D0-CY)*WYAR(NA,NNN4)
C
      RETURN
      END
