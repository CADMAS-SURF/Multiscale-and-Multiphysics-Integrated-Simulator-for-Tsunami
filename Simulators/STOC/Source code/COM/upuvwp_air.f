      SUBROUTINE UPUVWP_AIR(UUA,VVA,WWA,PPA,DPA,XC,YC,ZCA,
     $                      INDPA,INDUA,INDVA,INDWA,KFA)
C======================================================================
C     流速と圧力を更新する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
      INCLUDE 'FILE.h'
      INCLUDE 'TIMER.h'
      include 'TIMEI.h'
C
      REAL(8)::UUA(MX,MY,MZA),VVA(MX,MY,MZA),WWA(MX,MY,MZA)
      REAL(8)::PPA(MX,MY,MZA)
      REAL(8),INTENT(IN):: DPA(MX,MY,MZA)
      REAL(8),INTENT(IN)::XC(8,MX,MY),YC(8,MY),ZCA(8,MZA)
C
      INTEGER,INTENT(IN)::INDPA(MX,MY,MZA),INDUA(MX,MY,MZA)
      INTEGER,INTENT(IN)::INDVA(MX,MY,MZA),INDWA(MX,MY,MZA)
      INTEGER,INTENT(IN)::KFA(MX,MY)
C
      REAL(8)::DP1,UT,VT,WT
      INTEGER::I,J,K
C
C
C----------------------------------------------------------------------
C     (1) 圧力の更新
C----------------------------------------------------------------------
      DO 100 K=2,MZMA
      DO 100 J=2,MYM
      DO 100 I=2,MXM
         IF( INDPA(I,J,K).GT.0 ) THEN
            PPA(I,J,K) = PPA(I,J,K) + DPA(I,J,K)
         END IF
  100 CONTINUE
C
C
C----------------------------------------------------------------------
C     (2) 流速の更新
C----------------------------------------------------------------------
      DO 200 K=2,MZMA
      DO 200 J=2,MYM
      DO 200 I=1,MXM
         IF( INDUA(I,J,K).GT.0 ) THEN
            DP1 = ( DPA(I+1,J,K) - DPA(I,J,K) ) * XC(5,I,J)
            UUA(I,J,K) = UUA(I,J,K) - DTV / RHOAIR * DP1
         ELSE IF( INDUA(I,J,K).EQ.0 ) THEN
            IF( INDPA(I,J,K).GT.0 ) THEN
               DP1 = - DPA(I,J,K)*2.0D0*XC(6,I,J)
            ELSE
               DP1 = DPA(I+1,J,K)*2.0D0*XC(6,I+1,J)
            END IF
            UUA(I,J,K) = UUA(I,J,K) - DTV / RHOAIR * DP1
         END IF
  200 CONTINUE
C
      DO 300 K=2,MZMA
      DO 300 J=1,MYM
      DO 300 I=2,MXM
         IF( INDVA(I,J,K).GT.0 ) THEN
            DP1 = ( DPA(I,J+1,K) - DPA(I,J,K) ) * YC(5,J)
            VVA(I,J,K) = VVA(I,J,K) - DTV / RHOAIR * DP1
         ELSE IF( INDVA(I,J,K).EQ.0 ) THEN
            IF( INDPA(I,J,K).GT.0 ) THEN
               DP1 = - DPA(I,J,K)*2.0D0*YC(6,J)
            ELSE
               DP1 = DPA(I,J+1,K)*2.0D0*YC(6,J+1)
            END IF
            VVA(I,J,K) = VVA(I,J,K) - DTV / RHOAIR * DP1
         END IF
  300 CONTINUE
C
      DO 400 K=1,MZMA
      DO 400 J=2,MYM
      DO 400 I=2,MXM
         IF( INDWA(I,J,K).GT.0 ) THEN
            DP1 = ( DPA(I,J,K+1) - DPA(I,J,K) ) * ZCA(5,K)
            WWA(I,J,K) = WWA(I,J,K) - DTV / RHOAIR * DP1
         ELSE IF( INDWA(I,J,K).EQ.0 ) THEN
            IF( INDPA(I,J,K).GT.0 ) THEN
               DP1 = - DPA(I,J,K)*2.0D0*ZCA(6,K)
            ELSE
               DP1 = DPA(I,J,K+1)*2.0D0*ZCA(6,K+1)
            END IF
            WWA(I,J,K) = WWA(I,J,K) - DTV / RHOAIR * DP1
         END IF
  400 CONTINUE
C
      VELMAXAIR = 0.0D0
      DO 500 K=2,MZMA
      DO 500 J=2,MYM
      DO 500 I=2,MXM
         IF( INDPA(I,J,K).GT.0 ) THEN
            UT = 0.5D0*(UUA(I-1,J,K)+UUA(I,J,K))
            VT = 0.5D0*(VVA(I,J-1,K)+VVA(I,J,K))
            WT = 0.5D0*(WWA(I,J,K-1)+WWA(I,J,K))
            VELMAXAIR = MAX(VELMAXAIR,DSQRT(UT*UT+VT*VT+WT*WT))
         END IF
  500 CONTINUE
C
Cdbgc ... debug write
Cdbg      if( debug_air9.eq.1 ) then
Cdbg         do j=1,my
Cdbg            write(lp,*) 'dpa j=',j,' istep=',istep
Cdbg            write(lp,'(a5,<mxm>i10)') ' k%i|',(i,i=1,mxm)
Cdbg            do k=mza,1,-1
Cdbg               write(lp,'(i4,a1,<mxm>e10.3)') k,'|',(dpa(i,j,k),i=1,mxm)
Cdbg            enddo
Cdbg            write(lp,*) ''
Cdbg         enddo
Cdbgc
Cdbg         do j=1,my
Cdbg            write(lp,*) 'ppnew j=',j,' istep=',istep
Cdbg            write(lp,'(a5,<mxm>i10)') ' k%i|',(i,i=1,mxm)
Cdbg            do k=mza,1,-1
Cdbg               write(lp,'(i4,a1,<mxm>e10.3)') k,'|',(ppa(i,j,k),i=1,mxm)
Cdbg            enddo
Cdbg            write(lp,*) ''
Cdbg         enddo
Cdbg         do j=1,my
Cdbg            write(lp,*) 'uunew j=',j,' istep=',istep
Cdbg            write(lp,'(a5,<mxm>i8)') ' k%i|',(i,i=1,mxm)
Cdbg            do k=mza,1,-1
Cdbg               write(lp,'(i4,a1,<mxm>f8.3)') k,'|',(uua(i,j,k),i=1,mxm)
Cdbg            enddo
Cdbg            write(lp,*) ''
Cdbg         enddo
Cdbgc
Cdbg         do j=1,my
Cdbg            write(lp,*) 'vvnew j=',j,' istep=',istep
Cdbg            write(lp,'(a5,<mxm>i8)') ' k%i|',(i,i=1,mxm)
Cdbg            do k=mza,1,-1
Cdbg               write(lp,'(i4,a1,<mxm>f8.3)') k,'|',(vva(i,j,k),i=1,mxm)
Cdbg            enddo
Cdbg            write(lp,*) ''
Cdbg         enddo
Cdbgc
Cdbg         do j=1,my
Cdbg            write(lp,*) 'wwnew j=',j,' istep=',istep
Cdbg            write(lp,'(a5,<mxm>i8)') ' k%i|',(i,i=1,mxm)
Cdbg            do k=mza,1,-1
Cdbg               write(lp,'(i4,a1,<mxm>f8.3)') k,'|',(wwa(i,j,k),i=1,mxm)
Cdbg            enddo
Cdbg            write(lp,*) ''
Cdbg         enddo
Cdbg      endif
Cdbgc         do j=1,my
Cdbgc            write(lp,*) 'uunew j=',j,' istep=',istep
Cdbgc            write(lp,'(a5,<mxm>i8)') ' k%i|',(i,i=1,mxm)
Cdbgc            do k=mza,1,-1
Cdbgc               write(lp,'(i4,a1,<mxm>f8.3)') k,'|',(uua(i,j,k),i=1,mxm)
Cdbgc            enddo
Cdbgc            write(lp,*) ''
Cdbgc         enddo
Cdbgc
      RETURN
      END
