      SUBROUTINE CLWEQ_AIR(WPA,UUA,VVA,WWA,HUA,HVA,HWA,PPA,FFA,
     $                     TMUA,GVA,GXA,GYA,GZA,XC,YC,ZCA,
     $                     YCOS,INDPA,INDUA,INDVA,INDWA,KFA,
     $                     FU,FV,FW,WWX,WWY)
C======================================================================
C     Z方向の運動量の保存式を圧力項を陽に解いて仮流速Wを計算する
C     ※仮流速はUPUVWPルーチンで圧力補正量を用いて補正される
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'FILE.h'
      include 'TIMEI.h'
C
      REAL(8),INTENT(OUT)::WPA(MX,MY,MZA)
C
      REAL(8),INTENT(IN)::UUA(MX,MY,MZA),VVA(MX,MY,MZA),WWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::HUA(MX,MY,MZA),HVA(MX,MY,MZA),HWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::PPA(MX,MY,MZA),FFA(MX,MY,MZA)
      REAL(8),INTENT(IN)::TMUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::GVA(MX,MY,MZA),GXA(MX,MY,MZA)
      REAL(8),INTENT(IN)::GYA(MX,MY,MZA),GZA(MX,MY,MZA)
      REAL(8),INTENT(IN)::XC(8,MX,MY),YC(8,MY),ZCA(8,MZA)
      REAL(8),INTENT(IN)::YCOS(MY)
C
      INTEGER,INTENT(IN)::INDPA(MX,MY,MZA),INDUA(MX,MY,MZA)
      INTEGER,INTENT(IN)::INDVA(MX,MY,MZA),INDWA(MX,MY,MZA)
      INTEGER,INTENT(IN)::KFA(MX,MY)
C
      REAL(8)::FU(MX,MY,MZA),FV(MX,MY,MZA),FW(MX,MY,MZA) ! WORK
      REAL(8)::WWX(MX,MY,MZA),WWY(MX,MY,MZA)             ! WORK
C
      REAL(8)::GV1
      REAL(8)::DP1,DW1,GRAV1,DHDTW
      INTEGER::I,J,K
C
C
C----------------------------------------------------------------------
C     (1) 境界面上のWを設定する
C----------------------------------------------------------------------
      CALL BCWWXY_AIR(WWX,WWY,UUA,VVA,WWA,TMUA,
     $                XC,YC,INDPA,INDUA,INDVA)
C
C ... WWX,WWY
      CALL CP_DSR_DC2(MX,MY,MZA,1,1,WWX)
      CALL CP_DSR_DC2(MX,MY,MZA,2,1,WWY)
C
C
C----------------------------------------------------------------------
C     (2) コントロールボリューム界面の運動量流束を計算する(板境界は除く)
C----------------------------------------------------------------------
      CALL FLUXWX_AIR(FU,UUA,WWA,HUA,WWX,FFA,TMUA,XC,ZCA,GVA,GXA,
     $                INDUA,INDWA,KFA)
C
      CALL FLUXWY_AIR(FV,VVA,WWA,HVA,WWY,FFA,TMUA,YC,ZCA,GVA,GYA,
     $                INDVA,INDWA,KFA)
C
      CALL FLUXWZ_AIR(FW,WWA,HWA,TMUA,ZCA,GVA,INDWA,KFA)
C
C ... FU,FV
      CALL CP_DSR_DC2(MX,MY,MZA,1,1,FU)
      CALL CP_DSR_DC2(MX,MY,MZA,2,1,FV)
C
C
C----------------------------------------------------------------------
C     (3) 仮流速のz方向成分を計算する(自由流入出境界上の点は除く)
C----------------------------------------------------------------------
      CALL ZERCLR(WPA,MXY*MZA,0.0D0)
C
      DO 100 K=1,MZMA
      DO 100 J=2,MYM
      DO 100 I=2,MXM
         IF( INDWA(I,J,K).GT.0 ) THEN
            GV1 = (1.0D0-GVA(I,J,K  ))*ZCA(8,K)
     $          + (1.0D0-GVA(I,J,K+1))*ZCA(7,K)
C
            DP1 = GV1/RHOAIR*(PPA(I,J,K+1)-PPA(I,J,K))*ZCA(5,K)
C
            GRAV1 = GV1*GRAV
C
            DHDTW = WWA(I,J,K)
     $          *((( HUA(I,J,K  )-HUA(I-1,J,K  ) )*ZCA(8,K)
     $          +  ( HUA(I,J,K+1)-HUA(I-1,J,K+1) )*ZCA(7,K))*XC(6,I,J)
     $          + (( HVA(I,J,K  )-HVA(I,J-1,K  ) )*ZCA(8,K)
     $          +  ( HVA(I,J,K+1)-HVA(I,J-1,K+1) )*ZCA(7,K))
     $          *YC(6,J)/YCOS(J)
     $          +  ( HWA(I,J,K+1)-HWA(I,J,K-1)   )*0.5D0   *ZCA(5,K) )
C
            DW1 = - DP1 + GRAV1 + DHDTW
     $          + ( FU(I,J,K) - FU(I-1,J,K) ) * XC(6,I,J)
     $          + ( FV(I,J,K) - FV(I,J-1,K) ) * YC(6,J)/YCOS(J)
     $          + ( FW(I,J,K+1) - FW(I,J,K) ) * ZCA(5,K)
C
            WPA(I,J,K) = WWA(I,J,K) + DTV*DW1/GV1
         ELSEIF( INDWA(I,J,K).EQ. 0 ) THEN
C           (NOTHING TO DO)
         ELSEIF( INDWA(I,J,K).EQ.-1 ) THEN
            WPA(I,J,K) = WWA(I,J,K)
         ELSEIF( INDWA(I,J,K).LE.-2 ) THEN
            WPA(I,J,K) = 0.0D0
         ENDIF
  100 CONTINUE
C
C
C----------------------------------------------------------------------
C     (4) 自由流入出境界上の点にWPをコピー
C----------------------------------------------------------------------
      IF( IBCAIRTOP.EQ.-2 ) THEN
         K=MZMA
         DO J=2,MYM
         DO I=2,MXM
            WPA(I,J,K) = WPA(I,J,K-1)
         ENDDO
         ENDDO
      ENDIF
C
Cdbgc ... debug write
Cdbg      if( debug_air9.eq.1 ) then
Cdbg         do j=1,my
Cdbg            write(lp,*) 'wpa j=',j,' istep=',istep
Cdbg            write(lp,'(a5,<mx>i8)') ' k%i|',(i,i=1,mx)
Cdbg            do k=mza,1,-1
Cdbg               write(lp,'(i4,a1,<mx>f8.3)') k,'|',(wpa(i,j,k),i=1,mx)
Cdbg            enddo
Cdbg            write(lp,*) ''
Cdbg         enddo
Cdbg      endif
Cdbgc
      RETURN
      END
