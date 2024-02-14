      SUBROUTINE CLKEQ_AIR(AKA,AKNA,EPNA,HUA,HVA,HWA,FFA,TMUA,AKBCAIR,
     $                     GVA,GXA,GYA,GZA,XC,YC,ZCA,YCOS,
     $                     INDPA,INDUA,INDVA,INDWA,GS,FU,FV,FW)
C======================================================================
C     輸送方程式を解き新しい時刻の乱流エネルギーを計算する
C       FU: X方向熱流束,FV: Y方向熱流束,FW: Z方向熱流束
C       AK: 新しい時刻のk(m**2/s**2),AKN:古い時刻のk(m**/s**2)
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
      REAL(8),INTENT(OUT)::AKA(MX,MY,MZA)
      REAL(8),INTENT(IN)::AKNA(MX,MY,MZA),EPNA(MX,MY,MZA)
      REAL(8),INTENT(IN)::HUA(MX,MY,MZA),HVA(MX,MY,MZA),HWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::FFA(MX,MY,MZA),TMUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::AKBCAIR(NXY,MZA,4)
      REAL(8),INTENT(IN)::GVA(MX,MY,MZA),GXA(MX,MY,MZA)
      REAL(8),INTENT(IN)::GYA(MX,MY,MZA),GZA(MX,MY,MZA)
      REAL(8),INTENT(IN)::XC(8,MX,MY),YC(8,MY),ZCA(8,MZA)
      REAL(8),INTENT(IN)::YCOS(MY)
      INTEGER,INTENT(IN)::INDPA(MX,MY,MZA),INDUA(MX,MY,MZA)
      INTEGER,INTENT(IN)::INDVA(MX,MY,MZA),INDWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::GS(MX,MY,MZA)
      REAL(8)::FU(MX,MY,MZA),FV(MX,MY,MZA),FW(MX,MY,MZA)
C
      REAL(8)::GV1,DK1
      INTEGER::I,J,K
C
C
      CALL FLUXSX_AIR(FU,AKNA,HUA,TMUA,FFA,GXA,XC,AKBCAIR,
     $                INDUA,SGK)
C
      CALL FLUXSY_AIR(FV,AKNA,HVA,TMUA,FFA,GYA,YC,AKBCAIR,
     $                INDVA,SGK)
C
      CALL FLUXSZ_AIR(FW,AKNA,HWA,TMUA,GZA,ZCA,INDWA,SGK)
C
C
C ... 新しい時刻のAKを計算する
      DO K=2,MZMA
      DO J=2,MYM
      DO I=2,MXM
         IF( INDPA(I,J,K).GT.0 ) THEN
            GV1=1.0D0-FFA(I,J,K)-GVA(I,J,K)
C
            DK1=GS(I,J,K)-EPNA(I,J,K)
     $         +(FU(I,J,K)-FU(I-1,J,K))*XC(6,I,J)
     $         +(FV(I,J,K)-FV(I,J-1,K))*YC(6,J)/YCOS(J)
     $         +(FW(I,J,K)-FW(I,J,K-1))*ZCA(6,K)
C
            AKA(I,J,K)=AKNA(I,J,K)+DT*DK1/GV1
c            if(i.eq.3.and.(k.eq.20.or.k.eq.21))then
c               write(lp,*)'aka:k=',k,gs(i,j,k),epna(i,j,k),
c     $            fu(i,j,k),fu(i-1,j,k),
c     $            fv(i,j,k),fv(i,j-1,k),
c     $            fw(i,j,k),fw(i,j,k-1),
c     $            hwa(i,j,k),hwa(i,j,k-1)
c            endif
         ENDIF
      ENDDO
      ENDDO
      ENDDO
C
Cdbgc ... debug write
Cdbg      if( debug_air11.eq.1 ) then
Cdbg         do j=1,my
Cdbg            write(lp,*) 'fw j=',j,' istep=',istep
Cdbg            write(lp,'(a5,<mx>i10)') ' k%i|',(i,i=1,mx)
Cdbg            do k=mza,1,-1
Cdbg               write(lp,'(i4,a1,<mx>e10.3)') k,'|',(fw(i,j,k),i=1,mx)
Cdbg            enddo
Cdbg            write(lp,*) ''
Cdbg         enddo
Cdbg         do j=1,my
Cdbg            write(lp,*) 'aka j=',j,' istep=',istep
Cdbg            write(lp,'(a5,<mx>i10)') ' k%i|',(i,i=1,mx)
Cdbg            do k=mza,1,-1
Cdbg               write(lp,'(i4,a1,<mx>e10.3)') k,'|',(aka(i,j,k),i=1,mx)
Cdbg            enddo
Cdbg            write(lp,*) ''
Cdbg         enddo
Cdbg      endif
      RETURN
      END
