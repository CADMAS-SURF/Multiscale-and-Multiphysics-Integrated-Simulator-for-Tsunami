      SUBROUTINE LOGLAW(VTAU,VEL,DD,ANU)
C======================================================================
C     壁関数から摩擦速度(VTAU)を求める
C      VEL/VTAU=(1/k)log(VTAU*DD/ANU)+A
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'FILE.h'
      INCLUDE 'TURBR.h'
C
      REAL(8),INTENT(INOUT)::VTAU,VEL,DD,ANU
C
      REAL(8)::DH=0.9D0
      INTEGER::MAXNUM=10,MAXNM2=10
      INTEGER::IDB=0
C
      REAL(8)::DDNU,DFX,DKAR,DX,FX,FX1,FX2,XLN,XX
      INTEGER::LL,NUM
C
      REAL(8),PARAMETER::ZERO=1.0D-20
C
      VTAU = 0.0D0
      XX   = ANU/DD
      DX   = XX*0.01D0
      NUM  = 0
      DKAR = 1.0D0/AKAR
      DDNU = DD/ANU
cxxxxxxxxxxx
      if(Idb.ne.0) then
      write(6,*) 'vel,dd  =',vel,dd
      write(6,*) 'anu,ddnu=',anu,ddnu
      write(6,*) 'xx,dx   =',xx,dx
      write(6,*) 'dkar,taa=',dkar,taa
      XLN = LOG(XX*DDNU)
      write(6,*) 'xdnu,xln=',xx*ddnu,xln
      FX1=DKAR*XX*XLN
      FX2=TAA*XX
      write(6,*) 'FX1,FX2 =',FX1,FX2
      FX  = DKAR*XX*XLN+TAA*XX-VEL
      write(6,*) 'fx      =',fx
      endif
cxxxxxxxxxxx
C
  110 CONTINUE
      DO 100 LL=1,MAXNM2
        XLN = LOG(XX*DDNU)
        FX  = DKAR*XX*XLN+TAA*XX-VEL
        DFX = DKAR*XLN+DKAR+TAA
        IF( ABS(DFX).GT.ZERO*FX ) GO TO 200
        XX = XX+DX*DH
 100  CONTINUE
      GO TO 902
C                                     << DX = - F(X) / D(F(X))/DX >>
 200  CONTINUE
      DX = -FX/DFX
C                                           << CONVERGENCE CHECK. >>
      IF( ABS(DX).LT.XX*1.0D-10 ) GO TO 9000
      IF( NUM.GE.MAXNUM ) GO TO 901
C
      XX=XX+DX
      NUM=NUM+1
      if(idb.ne.0) WRITE(LP,6010) NUM,LL,XX,DX,FX,DFX
 6010 FORMAT(' ','== LOGLAW ==> NUM,LL,XX,DX,FX,DFX=',2I5,1P,4D12.5)
      GO TO 110
C
C------------------------------------------------------<< WARNING >>--
  901 CONTINUE
      WRITE(LP,*) 'NOT CONVERGED SUB. LOGLAW   MAXNUM =',MAXNUM
      WRITE(LP,9010) XX,DX,FX
 9010 FORMAT(' ',18X,'XX,DX,FX=',1P,3D12.5)
      GO TO 9000
C
  902 CONTINUE
      WRITE(LP,*) 'NOT CONVERGED SUB. LOGLAW   MAXNM2 =',MAXNM2
      WRITE(LP,9020) XX,DX,FX,DFX,VEL
 9020 FORMAT(' ',18X,'XX,DX,FX,DFX,VEL=',1P,5D12.5)
C
 9000  CONTINUE
      VTAU = XX
      if(idb.ne.0) WRITE(LP,6020) NUM,VTAU,DX,FX,DFX
 6020 FORMAT(' ','== LOGLAW ==> NUM,VTAU,DX,FX,DFX=',I5,1P,4D12.5)
C
      RETURN
C
      END
