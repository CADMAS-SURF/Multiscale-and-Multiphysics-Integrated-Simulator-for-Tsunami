C-----------------------------------------------------------
      SUBROUTINE CISTN (TXX,TYY,XLN,YLT,XT,YT)
C-----------------------------------------------------------
      IMPLICIT NONE
C
      REAL(8),INTENT(INOUT)::TXX,TYY,XLN,YLT
      REAL(8),INTENT(INOUT)::XT,YT
C
      REAL(8)::ZF,ZFI0,ZR,ZRA0,ZX,ZY
C
CC      COMMON /BL80/ ZFI0,ZRA0
CC      COMMON /BL90/ ZF,ZR
CC      COMMON /BL91/ ZY,ZX
C
      ZFI0= YLT
      ZRA0= XLN
      ZF  = TYY
      ZR  = TXX
C    ---------------
      CALL XYCV(ZFI0,ZRA0,ZF,ZR,ZY,ZX)
C    ---------------
      XT= -ZY
      YT= -ZX
C
      RETURN
      END
