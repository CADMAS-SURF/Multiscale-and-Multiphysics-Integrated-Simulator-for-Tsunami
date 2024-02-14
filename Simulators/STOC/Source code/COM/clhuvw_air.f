      SUBROUTINE CLHUVW_AIR(HUA,HVA,HWA,UUA,VVA,WWA,FFA,GXA,GYA,GZA,
     $                      XC,YC,YCOSP,INDUA,INDVA,INDWA,KFA)
C======================================================================
C     HUA,HVA,HWA値を設定する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
      INCLUDE 'FILE.h'
C
      REAL(8),INTENT(OUT)::HUA(MX,MY,MZA),HVA(MX,MY,MZA),HWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::UUA(MX,MY,MZA),VVA(MX,MY,MZA),WWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::FFA(MX,MY,MZA)
      REAL(8),INTENT(IN)::GXA(MX,MY,MZA),GYA(MX,MY,MZA),GZA(MX,MY,MZA)
C
      REAL(8),INTENT(IN)::XC(8,MX,MY),YC(8,MY)
      REAL(8),INTENT(IN)::YCOSP(MY)
      INTEGER,INTENT(IN)::INDUA(MX,MY,MZA),INDVA(MX,MY,MZA)
      INTEGER,INTENT(IN)::INDWA(MX,MY,MZA),KFA(MX,MY)
C
      INTEGER::I,J,K
      REAL(8)::FFI,FFJ,FF0,FF1,FF2
C
C
      CALL ZERCLR(HUA,MXY*MZA,0.0D0)
      CALL ZERCLR(HVA,MXY*MZA,0.0D0)
      CALL ZERCLR(HWA,MXY*MZA,0.0D0)
C
C
      DO 200 K=2,MZMA
      DO 200 J=2,MYM
      DO 200 I=1,MXM
C ...... 計算点及び流入出境界
         IF( INDUA(I,J,K).GE.-1 ) THEN
            FFI = FFA(I,J,K)*XC(7,I,J)+FFA(I+1,J,K)*XC(8,I,J)
            FF0 = MAX(1.0D0-FFI-GXA(I,J,K),0.D0)
            HUA(I,J,K) = FF0*UUA(I,J,K)
CCC            FF1 = MAX(1.0D0-FFA(I  ,J,K)-GXA(I,J,K),0.D0)
CCC            FF2 = MAX(1.0D0-FFA(I+1,J,K)-GXA(I,J,K),0.D0)
C
CCC            HUA(I,J,K) = PARAMAIR2*FF0*UUA(I,J,K)
CCC     $                 + PARAMAIR*(FF1*MAX(UUA(I,J,K),0.0D0)
CCC     $                 +           FF2*MIN(UUA(I,J,K),0.0D0))
         END IF
  200 CONTINUE
C
C
      DO 300 K=2,MZMA
      DO 300 J=1,MYM
      DO 300 I=2,MXM
C ...... 計算点及び流入出境界
         IF( INDVA(I,J,K).GE.-1 ) THEN
            FFJ = FFA(I,J,K)*YC(7,J)+FFA(I,J+1,K)*YC(8,J)
            FF0 = MAX(1.0D0-FFJ-GYA(I,J,K),0.0D0)
            HVA(I,J,K) = FF0*VVA(I,J,K)
CCC            FF1 = MAX(1.0D0-FFA(I,J  ,K)-GYA(I,J,K),0.0D0)
CCC            FF2 = MAX(1.0D0-FFA(I,J+1,K)-GYA(I,J,K),0.0D0)
C
CCC            HVA(I,J,K) = PARAMAIR2*FF0*VVA(I,J,K)
CCC     $                 + PARAMAIR*(FF1*MAX(VVA(I,J,K),0.0D0)
CCC     $                 +           FF2*MIN(VVA(I,J,K),0.0D0))
            HVA(I,J,K) = HVA(I,J,K)*YCOSP(J)
         END IF
  300 CONTINUE
C
C
      DO 400 K=1,MZMA
      DO 400 J=2,MYM
      DO 400 I=2,MXM
C ...... 計算点及び流入出境界
         IF( INDWA(I,J,K).GE.-1 .AND. K.GE.KFA(I,J) )
     $      HWA(I,J,K) = (1.D0-GZA(I,J,K))*WWA(I,J,K)
  400 CONTINUE
C
C
C ... HUA,HVA,HWA (for DOMAIN-DECOMP)
      CALL CP_DSR_DC2(MX,MY,MZA,1,1,HUA)
      CALL CP_DSR_DC2(MX,MY,MZA,2,1,HVA)
      CALL CP_DSR_DC2(MX,MY,MZA,3,1,HWA)
C
Cdbgc ... debug write
Cdbg      if( debug_air10.eq.1 ) then
Cdbg         do j=1,my
Cdbg            write(lp,*) 'hua j=',j
Cdbg            write(lp,'(a5,<mxm>i8)') ' k%i|',(i,i=1,mxm)
Cdbg            do k=mza,1,-1
Cdbg               write(lp,'(i4,a1,<mxm>f8.3)') k,'|',(hua(i,j,k),i=1,mxm)
Cdbg            enddo
Cdbg            write(lp,*) ''
Cdbg         enddo
Cdbgc
Cdbg         do j=1,my
Cdbg            write(lp,*) 'hva j=',j
Cdbg            write(lp,'(a5,<mx>i8)') ' k%i|',(i,i=1,mx)
Cdbg            do k=mza,1,-1
Cdbg               write(lp,'(i4,a1,<mx>f8.3)') k,'|',(hva(i,j,k),i=1,mx)
Cdbg            enddo
Cdbg            write(lp,*) ''
Cdbg         enddo
Cdbgc
Cdbg         do j=1,my
Cdbg            write(lp,*) 'hwa j=',j
Cdbg            write(lp,'(a5,<mx>i8)') ' k%i|',(i,i=1,mx)
Cdbg            do k=mza,1,-1
Cdbg               write(lp,'(i4,a1,<mx>f8.3)') k,'|',(hwa(i,j,k),i=1,mx)
Cdbg            enddo
Cdbg            write(lp,*) ''
Cdbg         enddo
Cdbg      endif
Cdbgc
      RETURN
      END
