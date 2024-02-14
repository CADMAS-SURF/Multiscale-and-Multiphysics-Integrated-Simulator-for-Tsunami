      SUBROUTINE DISP_CLUV(MM,NN,MMO,NNO,HH,HDEP,XC,YC,INDU,INDV,
     $                     ICHILD,IEAS,IWES,JSOU,JNOR,KBOT,KTOP)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                    !!
!! 分散波項をポテンシャル関数を用いて計算             !!
!!                                                    !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      use mod_psi,only:PSI,INDEXPSI,ALPHA,BETA,DHDX2
      IMPLICIT NONE

      INCLUDE 'TIMER.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
      INCLUDE 'FILE.h'

      REAL(8),PARAMETER    :: EPSD = 1.D-1
      REAL(8),INTENT(IN)   :: MM(MX,MY),NN(MX,MY)
      REAL(8),INTENT(IN)   :: MMO(MX,MY),NNO(MX,MY)
      REAL(8),INTENT(IN)   :: HDEP(MX,MY),HH(MX,MY)
      REAL(8),INTENT(IN)   :: XC(8,MX,MY),YC(8,MY)
      INTEGER,INTENT(IN)   :: INDU(MX,MY,MZ)
      INTEGER,INTENT(IN)   :: INDV(MX,MY,MZ)
      INTEGER,INTENT(IN)   :: ICHILD,IEAS,IWES,JNOR,JSOU,KBOT,KTOP

      REAL(8),ALLOCATABLE:: FX(:,:),FY(:,:)
! PSIの境界値
      REAL(8),ALLOCATABLE:: BCPSI(:,:)
      REAL(8),ALLOCATABLE:: AD(:,:)
      REAL(8),ALLOCATABLE:: AL(:,:,:),AU(:,:,:)
      REAL(8),ALLOCATABLE:: BB(:,:),DD(:,:)
      REAL(8),ALLOCATABLE:: PI(:,:),SI(:,:)
      REAL(8),ALLOCATABLE:: RI(:,:),R0(:,:)
      REAL(8),ALLOCATABLE:: W1(:,:),W2(:,:)
!
      INTEGER :: IX,IY,IZ,IERR
!
      INTEGER :: NPSI,IPSI,IPSIXP,IPSIXM,IPSIYP,IPSIYM
      REAL(8) :: PSI1,DTMP
      REAL(8) :: DHDXP,DHDXM,DHDYP,DHDYM
      INTEGER :: ICALCFLAG
      INTEGER :: MXG1,MYG1

      ALLOCATE(FX(MX,MY),FY(MX,MY),BCPSI(MX,MY),
     $         AD(MX,MY),AL(2,MX,MY),AU(2,MX,MY),BB(MX,MY),DD(MX,MY),
     $         PI(MX,MY),SI(MX,MY),RI(MX,MY),R0(MX,MY),W1(MX,MY),
     $         W2(MX,MY),STAT=IERR)
      IF(IERR.NE.0)THEN
         CALL ERRMSG('DISP_CLUV',6910)
         WRITE(LP,*) 'CANNOT ALLOCATE FX,...'
         CALL ABORT1('')
      ENDIF

      DO IY = 1,MY
         DO IX = 1,MX
            FX(IX,IY) = (MM(IX,IY) - MMO(IX,IY))/DTV
            FY(IX,IY) = (NN(IX,IY) - NNO(IX,IY))/DTV
         ENDDO
      ENDDO
      CALL CP_DSR_DC2(MX,MY,1,1,1,FX)
      CALL CP_DSR_DC2(MX,MY,1,2,1,FY)
!
      BCPSI    = 0.d0
      INDEXPSI = 0
! 陸側境界条件の設定
      DO IY = 2,MYM
         DO IX = 2,MXM
            IF( HDEP(IX,IY).LT.DISPLIM.AND.  
     $         (
     $          ((IPECON(6,NRANK+1).GE.0.OR.IX.NE.MXM)
     $                     .AND.HDEP(IX+1,IY  ).GE.DISPLIM)
     $      .OR.((IPECON(5,NRANK+1).GE.0.OR.IX.NE. 2 )
     $                     .AND.HDEP(IX-1,IY  ).GE.DISPLIM)
     $      .OR.((IPECON(7,NRANK+1).GE.0.OR.IY.NE.MYM)
     $                     .AND.HDEP(IX  ,IY+1).GE.DISPLIM)
     $      .OR.((IPECON(4,NRANK+1).GE.0.OR.IY.NE. 2 )
     $                     .AND.HDEP(IX  ,IY-1).GE.DISPLIM)
     $         )
     $        ) THEN

               INDEXPSI(IX,IY) = -1
               BCPSI(IX,IY) = ALPHA*HDEP(IX,IY)*
     $         (XC(6,IX,IY)*(FX(IX,IY)-FX(IX-1,IY  ))+
     $          YC(6,   IY)*(FY(IX,IY)-FY(IX  ,IY-1)))
            ENDIF
         ENDDO
      ENDDO
      CALL CP_DSR_DC2(MX,MY,1,0,1,BCPSI)
!
! INDEXPSI = -1 : 陸境界
! INDEXPSI =  0 : 計算対象外セル
! INDEXPSI >  0 : 計算対象セル
!
      mxg1=mxg
      myg1=myg
      if(mxg.eq.0) mxg1=mx
      if(myg.eq.0) myg1=my
      NPSI = 0
      DO IY = 2,MYM
        DO IX = 2,MXM


            ICALCFLAG = 1
            DO IZ = 2,MZM

!流速計算点以外は計算対象外

              IF(MXG1.GT.4.AND.
     $        (INDU(IX,IY,IZ).LE.0.OR.INDU(IX-1,IY,IZ).LE.0)) THEN
                ICALCFLAG = 0
                EXIT
              ENDIF
              IF(MYG1.GT.4.AND.
     $        (INDV(IX,IY,IZ).LE.0.OR.INDV(IX,IY-1,IZ).LE.0)) THEN
                ICALCFLAG = 0
                EXIT
              ENDIF

            ENDDO
            IF(ICALCFLAG.EQ.0 .OR.
     $         (INDEXPSI(IX,IY).EQ.-1.AND.HDEP(IX,IY).LE.DISPBCLIM))THEN
              INDEXPSI(IX,IY) =0
              CYCLE
            ENDIF

!子領域の周囲は計算対象外
            IF(ICHILD.GE.0.AND.
     $      IX.GE.IWES.AND.IX.LE.IEAS.AND.
     $      IY.GE.JSOU.AND.IY.LE.JNOR) CYCLE


! 水のないところは計算対象外
            IF( HH(IX,IY)-HDEP(IX,IY).LT.EPSD ) CYCLE
! 水深の浅いところは計算対象外
            IF(HDEP(IX,IY).GE.DISPLIM ) CYCLE
! 水位>水深のところは計算対象外
            IF( HH(IX,IY).GT.-HDEP(IX,IY) ) CYCLE
! 陸側境界条件設定セルは対象外
            IF( INDEXPSI(IX,IY).EQ.-1 ) CYCLE

            NPSI = NPSI + 1
            INDEXPSI(IX,IY) = NPSI
         ENDDO
      ENDDO

      CALL CP_BCINT(MX,MY,1,0,1,INDEXPSI)
! (d2HH/dx2+d2HH/dy2)の計算

      IF(BETA.NE.0.d0) THEN
         DO IY = 2,MYM
            DO IX = 2,MXM
            IPSI   = INDEXPSI(IX  ,IY  )
            IF(IPSI.LE.0) CYCLE

            IPSIXP = INDEXPSI(IX+1,IY  )
            IPSIXM = INDEXPSI(IX-1,IY  )
            IPSIYP = INDEXPSI(IX  ,IY+1)
            IPSIYM = INDEXPSI(IX  ,IY-1)
            DHDXP = 0.d0
            DHDXM = 0.d0
            DHDYP = 0.d0
            DHDYM = 0.d0

            IF(IPSIXP.GT.0)DHDXP=XC(5,IX  ,IY)*(HH(IX+1,IY)-HH(IX,IY))
            IF(IPSIXM.GT.0)DHDXM=XC(5,IX-1,IY)*(HH(IX,IY)-HH(IX-1,IY))
            IF(IPSIYP.GT.0)DHDYP=YC(5,   IY  )*(HH(IX,IY+1)-HH(IX,IY))
            IF(IPSIYM.GT.0)DHDYM=YC(5,   IY-1)*(HH(IX,IY)-HH(IX,IY-1))
            DHDX2(IX,IY) = 
     $       (DHDXP-DHDXM)*XC(6,IX,IY)
     $      +(DHDYP-DHDYM)*YC(6,   IY)
            ENDDO
         ENDDO
      ENDIF

      CALL CP_DSR_DC2(MX,MY,1,0,1,DHDX2)

      IF(DABS(ALPHA).LT.1.d-20) THEN
         DEALLOCATE(FX,FY,BCPSI,AD,AL,AU,BB,DD,PI,SI,RI,R0,W1,W2)
         RETURN
      ENDIF
!
      BB = 0.d0
      AD = 0.d0
      AU = 0.d0
      AL = 0.d0
      DD = 0.d0
      PI = 0.d0
      SI = 0.d0
      RI = 0.d0
      R0 = 0.d0
      W1 = 0.d0
      W2 = 0.d0

!
! PSIに関するPoisson方程式の作成
!
      DO IY = 2,MYM
         DO IX = 2,MXM
            IPSI   = INDEXPSI(IX  ,IY  )
            IF(IPSI.LE.0) CYCLE

            IPSIXP = INDEXPSI(IX+1,IY  )
            IPSIXM = INDEXPSI(IX-1,IY  )
            IPSIYP = INDEXPSI(IX  ,IY+1)
            IPSIYM = INDEXPSI(IX  ,IY-1)
! 対角成分
            DTMP = 0.d0 
            IF(IPSIXP.GT.0) DTMP = DTMP + XC(6,IX,IY)*XC(5,IX,IY  )
            IF(IPSIXM.GT.0) DTMP = DTMP + XC(6,IX,IY)*XC(5,IX-1,IY)
            IF(IPSIYP.GT.0) DTMP = DTMP + YC(6,   IY)*YC(5,   IY  )
            IF(IPSIYM.GT.0) DTMP = DTMP + YC(6,   IY)*YC(5,   IY-1)
            AD(IX,IY) = 1.d0+ALPHA*HDEP(IX,IY)**2*DTMP

! 右辺ベクトル
            BB(IX,IY) = ALPHA * (-HDEP(IX,IY))*(
     $      XC(6,IX,IY)*(FX(IX,IY)-FX(IX-1,IY  )) + 
     $      YC(6,   IY)*(FY(IX,IY)-FY(IX  ,IY-1)))
     $      +BETA*(-GRAV)*(-HDEP(IX,IY))**2*DHDX2(IX,IY)

! 非対角成分
            IF(IPSIXP.GT.0) THEN
            AU(1,IX,IY) =-ALPHA*HDEP(IX,IY)**2*XC(6,IX,IY)*XC(5,IX  ,IY)
            ENDIF

            IF(IPSIXM.GT.0) THEN
            AL(1,IX,IY) =-ALPHA*HDEP(IX,IY)**2*XC(6,IX,IY)*XC(5,IX-1,IY)
            ENDIF

            IF(IPSIYP.GT.0) THEN
            AU(2,IX,IY) =-ALPHA*HDEP(IX,IY)**2*YC(6,   IY)*YC(5,     IY)
            ENDIF

            IF(IPSIYM.GT.0) THEN
            AL(2,IX,IY) =-ALPHA*HDEP(IX,IY)**2*YC(6,   IY)*YC(5,   IY-1)
            ENDIF

! 陸側境界条件の処理
            PSI1 = 0.d0
            IF(IPSIXP.EQ.-1) THEN
            PSI1 = PSI1+BCPSI(IX+1,IY  )*XC(6,IX,IY)*XC(5,IX  ,IY)
            ENDIF
            IF(IPSIXM.EQ.-1) THEN 
            PSI1 = PSI1+BCPSI(IX-1,IY  )*XC(6,IX,IY)*XC(5,IX-1,IY)
            ENDIF
            IF(IPSIYP.EQ.-1) THEN
            PSI1 = PSI1+BCPSI(IX  ,IY+1)*YC(6,   IY)*YC(5,     IY)
            ENDIF
            IF(IPSIYM.EQ.-1) THEN
            PSI1 = PSI1+BCPSI(IX  ,IY-1)*YC(6,   IY)*YC(5,   IY-1)
            ENDIF
            BB(IX,IY) = BB(IX,IY) - HDEP(IX,IY)**2*PSI1/3.d0

         ENDDO
      ENDDO


      CALL CP_DSR_DC2(MX,MY,1,0,1,AD)
      CALL CP_DSR_DC3(2,MX,MY,1,0,1,AL)
      CALL CP_DSR_DC3(2,MX,MY,1,0,1,AU)
      CALL CP_DSR_DC2(MX,MY,1,0,1,BB)


      CALL QBICGS2D(PSI,AD,AL,AU,BB,DD,PI,SI,RI,R0,W1,W2,INDEXPSI)

      CALL CP_DSR_DC2(MX,MY,1,0,1,PSI)

      DEALLOCATE(FX,FY,BCPSI,AD,AL,AU,BB,DD,PI,SI,RI,R0,W1,W2)
      RETURN
      END SUBROUTINE
