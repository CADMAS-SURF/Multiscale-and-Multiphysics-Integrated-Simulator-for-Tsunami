      SUBROUTINE CLUEQ_AIR(UPA,UUA,VVA,WWA,HUA,HVA,HWA,PPA,FFA,
     $                     TMUA,UUBCAIR,GVA,GXA,GYA,GZA,XC,YC,ZCA,
     $                     XCP,YCOS,YSIN,INDPA,INDUA,INDVA,INDWA,KFA,
     $                     FU,FV,FW,UUY,UUZ)
C======================================================================
C     X方向の運動量の保存式を圧力項を陽に解いて仮流速Uを計算する
C     ※仮流速はUPUVWPルーチンで圧力補正量を用いて補正される
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'GRID.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
      INCLUDE 'FILE.h'
      include 'TIMEI.h'
C
      REAL(8),INTENT(OUT)::UPA(MX,MY,MZA)
C
      REAL(8),INTENT(IN)::UUA(MX,MY,MZA),VVA(MX,MY,MZA),WWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::HUA(MX,MY,MZA),HVA(MX,MY,MZA),HWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::PPA(MX,MY,MZA),FFA(MX,MY,MZA)
      REAL(8),INTENT(IN)::TMUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::UUBCAIR(NXY,MZA,4)
      REAL(8),INTENT(IN)::GVA(MX,MY,MZA),GXA(MX,MY,MZA)
      REAL(8),INTENT(IN)::GYA(MX,MY,MZA),GZA(MX,MY,MZA)
      REAL(8),INTENT(IN)::XC(8,MX,MY),YC(8,MY),ZCA(8,MZA)
      REAL(8),INTENT(IN)::XCP(8,MX,MY)
      REAL(8),INTENT(IN)::YCOS(MY),YSIN(MY)
C
      INTEGER,INTENT(IN)::INDPA(MX,MY,MZA),INDUA(MX,MY,MZA)
      INTEGER,INTENT(IN)::INDVA(MX,MY,MZA),INDWA(MX,MY,MZA)
      INTEGER,INTENT(IN)::KFA(MX,MY)
C
      REAL(8)::FU(MX,MY,MZA),FV(MX,MY,MZA),FW(MX,MY,MZA) ! WORK
      REAL(8)::UUY(MX,MY,MZA),UUZ(MX,MY,MZA)             ! WORK
C
      REAL(8)::GV1
      REAL(8)::DP1,VVUK,DU1,CORIV,DHDTU
      INTEGER::I,J,K
C
C
C----------------------------------------------------------------------
C     (1) 境界面上のUを設定する
C----------------------------------------------------------------------
      CALL BCUUYZ_AIR(UUY,UUZ,UUA,VVA,WWA,FFA,TMUA,UUBCAIR,
     $                GVA,YC,ZCA,INDPA,INDVA,INDWA)
Cdbgc ... debug write
Cdbg      if( debug_air10.eq.1 ) then
Cdbg         do j=1,my
Cdbg            write(lp,*) 'uuy j=',j
Cdbg            write(lp,'(a5,<mxm>i10)') ' k%i|',(i,i=1,mxm)
Cdbg            do k=mza,1,-1
Cdbg               write(lp,'(i4,a1,<mxm>e10.3)') k,'|',(uuy(i,j,k),i=1,mxm)
Cdbg            enddo
Cdbg            write(lp,*) ''
Cdbg         enddo
Cdbgc
Cdbg         do j=1,my
Cdbg            write(lp,*) 'uuz j=',j
Cdbg            write(lp,'(a5,<mx>i10)') ' k%i|',(i,i=1,mx)
Cdbg            do k=mza,1,-1
Cdbg               write(lp,'(i4,a1,<mx>e10.3)') k,'|',(uuz(i,j,k),i=1,mx)
Cdbg            enddo
Cdbg            write(lp,*) ''
Cdbg         enddo
Cdbg      endif
C
C ... UUY,UUZ
      CALL CP_DSR_DC2(MX,MY,MZA,2,1,UUY)
      CALL CP_DSR_DC2(MX,MY,MZA,3,1,UUZ)
C
C
C----------------------------------------------------------------------
C     (2) コントロールボリューム界面の運動量流束を計算する(板境界は除く)
C----------------------------------------------------------------------
      CALL FLUXUX_AIR(FU,UUA,HUA,FFA,TMUA,XC,GVA,INDUA,KFA)
C
      CALL FLUXUY_AIR(FV,UUA,VVA,HVA,UUY,FFA,TMUA,XC,YC,XCP,
     $                GVA,GYA,INDUA,INDVA,KFA)
C
      CALL FLUXUZ_AIR(FW,UUA,WWA,HWA,UUZ,TMUA,XC,ZCA,
     $                GZA,INDUA,INDWA,KFA)
C
Cdbgc ... debug write
Cdbg      if( debug_air10.eq.1 ) then
Cdbg         do j=1,my
Cdbg            write(lp,*) 'fu j=',j
Cdbg            write(lp,'(a5,<mxm>i10)') ' k%i|',(i,i=1,mxm)
Cdbg            do k=mza,1,-1
Cdbg               write(lp,'(i4,a1,<mxm>e10.3)') k,'|',(fu(i,j,k),i=1,mxm)
Cdbg            enddo
Cdbg            write(lp,*) ''
Cdbg         enddo
Cdbgc
Cdbg         do j=1,my
Cdbg            write(lp,*) 'fv j=',j
Cdbg            write(lp,'(a5,<mx>i10)') ' k%i|',(i,i=1,mx)
Cdbg            do k=mza,1,-1
Cdbg               write(lp,'(i4,a1,<mx>e10.3)') k,'|',(fv(i,j,k),i=1,mx)
Cdbg            enddo
Cdbg            write(lp,*) ''
Cdbg         enddo
Cdbgc
Cdbg         do j=1,my
Cdbg            write(lp,*) 'fw j=',j
Cdbg            write(lp,'(a5,<mx>i10)') ' k%i|',(i,i=1,mx)
Cdbg            do k=mza,1,-1
Cdbg               write(lp,'(i4,a1,<mx>e10.3)') k,'|',(fw(i,j,k),i=1,mx)
Cdbg            enddo
Cdbg            write(lp,*) ''
Cdbg         enddo
Cdbg      endif
Cdbgc
C ... FV,FW
      CALL CP_DSR_DC2(MX,MY,MZA,2,1,FV)
      CALL CP_DSR_DC2(MX,MY,MZA,3,1,FW)
C
C
C----------------------------------------------------------------------
C     (3) 仮流速のx方向成分を計算する(自由流入出境界上の点は除く)
C----------------------------------------------------------------------
      CALL ZERCLR(UPA,MXY*MZA,0.0D0)
C
      DO 100 K=2,MZMA
      DO 100 J=2,MYM
      DO 100 I=1,MXM
         IF( INDUA(I,J,K).GT.0 ) THEN
            GV1 = (1.0D0-FFA(I  ,J,K)-GVA(I  ,J,K))*XC(8,I,J)
     $          + (1.0D0-FFA(I+1,J,K)-GVA(I+1,J,K))*XC(7,I,J)
C
            VVUK=
     $     (VVA(I,J  ,K)*XCP(7,I,J  )+VVA(I+1,J  ,K)*XCP(8,I,J  ))*0.5D0
     $    +(VVA(I,J-1,K)*XCP(7,I,J-1)+VVA(I+1,J-1,K)*XCP(8,I,J-1))*0.5D0
            CORIV = GV1*2.0D0*CEARTH*YSIN(J)*VVUK
C
            DP1 = GV1/RHOAIR*(PPA(I+1,J,K)-PPA(I,J,K))*XC(5,I,J)
C
            DHDTU = UUA(I,J,K)
     $          *( ( HUA(I+1,J,K)-HUA(I-1,J,K)   )*0.5D0   *XC(5,I,J)
     $          + (( HVA(I  ,J,K)-HVA(I  ,J-1,K) )*XC(8,I,J)
     $          +  ( HVA(I+1,J,K)-HVA(I+1,J-1,K) )*XC(7,I,J))
     $          *YC(6,J)/YCOS(J)
     $          + (( HWA(I  ,J,K)-HWA(I  ,J,K-1) )*XC(8,I,J)
     $          +  ( HWA(I+1,J,K)-HWA(I+1,J,K-1) )*XC(7,I,J))*ZCA(6,K) )
C
            DU1 = - DP1 + CORIV + DHDTU
     $          + ( FU(I+1,J,K) - FU(I,J,K) ) * XC(5,I,J)
     $          + ( FV(I,J,K) - FV(I,J-1,K) ) * YC(6,J)/YCOS(J)
     $          + ( FW(I,J,K) - FW(I,J,K-1) ) * ZCA(6,K)
C
            UPA(I,J,K) = UUA(I,J,K) + DTV*DU1/GV1
         ELSEIF( INDUA(I,J,K).EQ. 0 ) THEN
C           (NOTHING TO DO)
         ELSEIF( INDUA(I,J,K).EQ.-1 ) THEN
            UPA(I,J,K) = UUA(I,J,K)
         ELSEIF( INDUA(I,J,K).LE.-2 ) THEN
            UPA(I,J,K) = 0.0D0
         ENDIF
  100 CONTINUE
C
Cdbgc ... debug write
Cdbg      if( debug_air9.eq.1 ) then
Cdbg         do j=1,my
Cdbg            write(lp,*) 'upa j=',j,' istep=',istep
Cdbg            write(lp,'(a5,<mxm>i8)') ' k%i|',(i,i=1,mxm)
Cdbg            do k=mza,1,-1
Cdbg               write(lp,'(i4,a1,<mxm>f8.3)') k,'|',(upa(i,j,k),i=1,mxm)
Cdbg            enddo
Cdbg            write(lp,*) ''
Cdbg         enddo
Cdbg      endif
Cdbgc
      RETURN
      END
