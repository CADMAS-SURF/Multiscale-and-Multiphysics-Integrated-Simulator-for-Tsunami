C-------------------------------------------------------------------
      SUBROUTINE XYCV(ZFI0,ZRA0,ZF,ZR,ZY,ZX)
C-------------------------------------------------------------------
C     LAT. & LON. ----> X,Y CONVERT PROGRAM
C         INPUT DATA
C                ZF  : LAT.( HOKUI  ,DEG ,REAL*8 )
C                ZR  : LON.( TOUKEI ,DEG ,REAL*8 )
C         OUTPUT DATA
C                ZY  : X-DIR ( GRID DATA M ( REAL*8 ))
C                ZX  : Y-DIR ( GRID DATA M ( REAL*8 ))
C-------------------------------------------------------------------
C
      IMPLICIT NONE
C
      REAL(8),INTENT(INOUT)::ZFI0,ZRA0,ZF,ZR,ZY,ZX
C
      REAL(8)::ZA,ZAA,ZBB,ZCC,ZDR,ZE2,ZET,ZF0,ZM00
      REAL(8)::ZM01,ZM02,ZM1,ZM10,ZM11,ZM12,ZNN,ZNN1
      REAL(8)::ZPI,ZR0,ZT2,ZTT1,ZX00,ZX10,ZX11,ZY1,ZY2
C
CC      COMMON /BL80/ ZFI0,ZRA0
CC      COMMON /BL90/ ZF,ZR
CC      COMMON /BL91/ ZY,ZX
C
      ZA =6377397.15D0
      ZE2=6.674372231D-3
      ZPI=3.14159265358979/180.0D0
C
      ZF0=ZFI0*ZPI
      ZR0=ZRA0*ZPI
      ZF =ZF  *ZPI
      ZR =ZR  *ZPI
C
      ZNN=ZA/DSQRT( 1.0-ZE2*DSIN(ZF )**2 )
      ZDR=(ZR -ZR0)*DCOS(ZF )
      ZT2=DTAN(ZF )**2
      ZET=ZE2*DCOS(ZF )**2/ (1.0D0-ZE2)
      ZY1=ZDR +  ZDR**3 *(1.0D0-ZT2+ZET) / 6.0D0
      ZY2=ZDR**5*(5.0D0-18.0D0*ZT2+ZT2**2+14.D0*ZET
     $                 -58.0D0*ZT2*ZET)/120.0D0
      ZY =(ZY1+ZY2)*ZNN*0.9999D0
C
      ZAA=1.005037306049D0
      ZBB=0.005047849240D0
      ZCC=0.000010563787D0
C
      ZM00=ZAA*ZF0
      ZM01=ZBB*DSIN(ZF0)*DCOS(ZF0)
      ZM02=0.5D0*ZCC*DSIN(2.0D0*ZF0)*DCOS(2.0D0*ZF0)
      ZX00=ZA*(1.0D0-ZE2)*(ZM00-ZM01+ZM02)
C
      ZM10=ZAA*ZF
      ZM11=ZBB*DSIN(ZF )*DCOS(ZF )
      ZM12=0.5*ZCC*DSIN(2.0D0*ZF )*DCOS(2.0*ZF )
      ZM1 =ZA*(1.0-ZE2)*(ZM10-ZM11+ZM12)
      ZTT1=DTAN(ZF )
      ZNN1=ZA/DSQRT( 1.0D0-ZE2*DSIN(ZF )**2 )
      ZX11=ZDR**2*ZTT1/2.0D0 + ZDR**4*ZTT1*(5.0D0-ZTT1**2)/24.0D0
      ZX10=ZM1 +ZX11*ZNN1
C
      ZX  =(ZX10-ZX00) *0.9999D0
C
      RETURN
      END
