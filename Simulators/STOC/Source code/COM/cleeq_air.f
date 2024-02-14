      SUBROUTINE CLEEQ_AIR(EPA,AKNA,EPNA,HUA,HVA,HWA,FFA,TMUA,EPBCAIR,
     $                     GVA,GXA,GYA,GZA,XC,YC,ZCA,YCOS,
     $                     INDPA,INDUA,INDVA,INDWA,GS,FU,FV,FW)
C======================================================================
C     輸送方程式を解き新しい時刻の乱流エネルー散逸を計算する
C       FU: X方向熱流束,FV: Y方向熱流束,FW: Z方向熱流束
C       EP: 新しい時刻のε(m**2/s**3),AKN:古い時刻のε(m**/s**3)
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'TURBR.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'TIMER.h'
      include 'TIMEI.h'
      include 'FILE.h'
C
      REAL(8),INTENT(OUT)::EPA(MX,MY,MZA)
      REAL(8),INTENT(IN)::AKNA(MX,MY,MZA),EPNA(MX,MY,MZA)
      REAL(8),INTENT(IN)::HUA(MX,MY,MZA),HVA(MX,MY,MZA),HWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::FFA(MX,MY,MZA),TMUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::EPBCAIR(NXY,MZA,4)
      REAL(8),INTENT(IN)::GVA(MX,MY,MZA),GXA(MX,MY,MZA)
      REAL(8),INTENT(IN)::GYA(MX,MY,MZA),GZA(MX,MY,MZA)
      REAL(8),INTENT(IN)::XC(8,MX,MY),YC(8,MY),ZCA(8,MZA)
      REAL(8),INTENT(IN)::YCOS(MY)
      INTEGER,INTENT(IN)::INDPA(MX,MY,MZA),INDUA(MX,MY,MZA)
      INTEGER,INTENT(IN)::INDVA(MX,MY,MZA),INDWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::GS(MX,MY,MZA)
      REAL(8)::FU(MX,MY,MZA),FV(MX,MY,MZA),FW(MX,MY,MZA)
C
      REAL(8)::EDK,GV1,DE1
      INTEGER::I,J,K
C
C
      CALL FLUXSX_AIR(FU,EPNA,HUA,TMUA,FFA,GXA,XC,EPBCAIR,
     $                INDUA,SGE)
C
      CALL FLUXSY_AIR(FV,EPNA,HVA,TMUA,FFA,GYA,YC,EPBCAIR,
     $                INDVA,SGE)
C
      CALL FLUXSZ_AIR(FW,EPNA,HWA,TMUA,GZA,ZCA,INDWA,SGE)
C
C
C ... 新しい時刻のEPを計算する
      DO K=2,MZMA
      DO J=2,MYM
      DO I=2,MXM
         IF( INDPA(I,J,K).GT.0 ) THEN
            GV1=1.0D0-FFA(I,J,K)-GVA(I,J,K)
C
            EDK = 0.0D0
            IF(AKNA(I,J,K).GT.0.0D0) EDK=EPNA(I,J,K)/AKNA(I,J,K)
C
            DE1=EDK*(TC1*GS(I,J,K)-TC2*EPNA(I,J,K))
     $         +(FU(I,J,K)-FU(I-1,J,K))*XC(6,I,J)
     $         +(FV(I,J,K)-FV(I,J-1,K))*YC(6,J)/YCOS(J)
     $         +(FW(I,J,K)-FW(I,J,K-1))*ZCA(6,K)
C
            EPA(I,J,K)=EPNA(I,J,K)+DT*DE1/GV1
         ENDIF
      ENDDO
      ENDDO
      ENDDO
C
Cdbgc ... debug write
Cdbg      if( debug_air11.eq.1 ) then
Cdbg         do j=1,my
Cdbg            write(lp,*) 'epa j=',j,' istep=',istep
Cdbg            write(lp,'(a5,<mx>i10)') ' k%i|',(i,i=1,mx)
Cdbg            do k=mza,1,-1
Cdbg               write(lp,'(i4,a1,<mx>e10.3)') k,'|',(epa(i,j,k),i=1,mx)
Cdbg            enddo
Cdbg            write(lp,*) ''
Cdbg         enddo
Cdbg      endif
      RETURN
      END
