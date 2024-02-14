      SUBROUTINE DISP_DIVUV(UU,VV,FF,GX0,GY0,HH,HDEP,XC,YC,ZC,INDU,INDV)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                    !!
!! 以下の式に従って流量を各セルに配分                 !!
!!                                                    !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      use mod_psi,only:PSI,INDEXPSI,ALPHA,BETA,DHDX2
      IMPLICIT NONE
      INCLUDE 'TIMER.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'VVMAX.h'
      REAL(8),INTENT(INOUT):: UU(MX,MY,MZ),VV(MX,MY,MZ)
      REAL(8),INTENT(IN)   :: FF(MX,MY,MZ)
      REAL(8),INTENT(IN)   :: GX0(MX,MY,MZ),GY0(MX,MY,MZ)
      REAL(8),INTENT(IN)   :: HDEP(MX,MY),HH(MX,MY)
      REAL(8),INTENT(IN)   :: XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      INTEGER,INTENT(IN)   :: INDU(MX,MY,MZ)
      INTEGER,INTENT(IN)   :: INDV(MX,MY,MZ)


      REAL(8) :: DPSIDX,DPSIDY
      INTEGER :: IX,IY,IZ
      INTEGER :: IPSI,IPSIXP,IPSIYP
      REAL(8) :: CCX,CCY
      REAL(8) :: DELTAX,DELTAY
      REAL(8) :: DZX,DZY,ZCX,ZCY
      REAL(8) :: FFI,FF0
      REAL(8) :: GX1,GY1
      REAL(8) :: HHX,HHY
      REAL(8) :: SMALL = 1.0d-20
      REAL(8) :: DHDX3,DHDY3


      DO IY = 2,MYM
         DO IX = 2,MXM
!            IF(INDEXPSI(IX,IY).LE.0) CYCLE
            IPSI   = INDEXPSI(IX  ,IY  )
            IPSIXP = INDEXPSI(IX+1,IY  )
            IPSIYP = INDEXPSI(IX  ,IY+1)

            HHX = HDEP(IX,IY)*XC(7,IX,IY)+HDEP(IX+1,IY)*XC(8,IX,IY)
            HHY = HDEP(IX,IY)*YC(7,   IY)+HDEP(IX,IY+1)*YC(8,   IY)

            DPSIDX = 0.d0
            DPSIDY = 0.d0
            DHDX3  = 0.d0
            DHDY3  = 0.d0

            IF(IPSI.GT.0.AND.IPSIXP.GT.0) THEN
              DPSIDX = (PSI(IX+1,IY)-PSI(IX,IY))*XC(5,IX,IY)
            ENDIF

            IF(IPSI.GT.0.AND.IPSIYP.GT.0) THEN
              DPSIDY = (PSI(IX,IY+1)-PSI(IX,IY))*YC(5,   IY)
            ENDIF

            IF(IPSI.GT.0.AND.IPSIXP.GT.0) THEN
              DHDX3 = (DHDX2(IX+1,IY)-DHDX2(IX,IY))*XC(5,IX,IY)
            ENDIF

            IF(IPSI.GT.0.AND.IPSIYP.GT.0) THEN
              DHDY3 = (DHDX2(IX,IY+1)-DHDX2(IX,IY))*YC(5,   IY)
            ENDIF



           DO IZ = 2,MZM
           IF(INDEXPSI(IX,IY).GE.1.AND.INDEXPSI(IX+1,IY).GE.1.AND.
     $     INDU(IX,IY,IZ).EQ.1) THEN

! Update U
             GX1 = 1.0D0-GX0(IX,IY,IZ)
             FFI = FF(IX,IY,IZ)*XC(7,IX,IY)+FF(IX+1,IY,IZ)*XC(8,IX,IY)
             FF0 = MAX(FFI        -GX1,0.0D0)

             DZX = ZC(4,IZ)*FF0
             ZCX = ZC(1,IZ-1) + GX1*ZC(4,IZ)
             DELTAX = -((ZCX+0.5d0*DZX)*(1-0.5d0*(ZCX+0.5d0*DZX)/HHX)
     $       -1.d0/24.d0*DZX**2/HHX)
     $       /(-HHX)

             CCX = DELTAX*3.d0

             IF(FF0.GT.SMALL.AND.IX.LT.MXM) THEN
               IF(dabs(ALPHA).LE.SMALL) Then
               UU(IX,IY,IZ) = UU(IX,IY,IZ)
     $         +DHDX3*DTV*BETA*(-GRAV)*HHX**2
               Else
               UU(IX,IY,IZ) = UU(IX,IY,IZ)
     $         +DPSIDX*DTV*CCX
               Endif
               IF(ABS(UU(IX,IY,IZ)).GT.VVMAX)
     $            UU(IX,IY,IZ)=SIGN(VVMAX,UU(IX,IY,IZ))
            ENDIF
         ENDIF



!     Update V
         IF(INDEXPSI(IX,IY).GE.1.AND.INDEXPSI(IX,IY+1).GE.1.AND.
     $      INDV(IX,IY,IZ).EQ.1) THEN
            GY1 = 1.0D0-GY0(IX,IY,IZ)
            FFI = FF(IX,IY,IZ)*YC(7,IY)+FF(IX,IY+1,IZ)*YC(8,IY)
            FF0 = MAX(FFI        -GY1,0.0D0)
            DZY = ZC(4,IZ)*FF0
            ZCY = ZC(1,IZ-1) + GY1*ZC(4,IZ)
            DELTAY = -((ZCY+0.5d0*DZY)*(1-0.5d0*(ZCY+0.5d0*DZY)/HHY)
     $         -1.d0/24.d0*DZY**2/HHY)
     $         /(-HHY)

            CCY = DELTAY*3.d0


            IF(FF0.GT.SMALL.AND.IY.LT.MYM) THEN
               IF(dabs(ALPHA).LE.SMALL) Then
                  VV(IX,IY,IZ) = VV(IX,IY,IZ)
     $               +DHDY3*DTV*BETA*(-GRAV)*HHY**2
               Else
                  VV(IX,IY,IZ) = VV(IX,IY,IZ)
     $               +DPSIDY*DTV*CCY
               Endif
               IF(ABS(VV(IX,IY,IZ)).GT.VVMAX)
     $            VV(IX,IY,IZ)=SIGN(VVMAX,VV(IX,IY,IZ))
             ENDIF
             ENDIF

           ENDDO
         ENDDO
      ENDDO

      END SUBROUTINE
