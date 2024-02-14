      SUBROUTINE MKCOE2_AIR(ADA,ALA,AUA,BBA,AD0A,AL0A,UUA,VVA,WWA,
     $                      HUA,HVA,HWA,FFA,GVA,FFNA,GVNA,
     $                      GXA,GYA,GZA,XC,YC,ZCA,XCP,
     $                      INDUA,INDVA,INDWA,INDPA,KFA)
C======================================================================
C     圧力補正式の係数行列と右辺を更新する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'FILE.h'
      include 'TIMEI.h'
C
      REAL(8),INTENT(OUT)::ADA(MX,MY,MZA),ALA(3,MX,MY,MZA)
      REAL(8),INTENT(OUT)::AUA(3,MX,MY,MZA),BBA(MX,MY,MZA)
C
      REAL(8),INTENT(IN)::AD0A(MX,MY,MZA),AL0A(3,MX,MY,MZA)
      REAL(8),INTENT(IN)::UUA(MX,MY,MZA),VVA(MX,MY,MZA),WWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::HUA(MX,MY,MZA),HVA(MX,MY,MZA),HWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::FFA(MX,MY,MZA),GVA(MX,MY,MZA)
      REAL(8),INTENT(IN)::FFNA(MX,MY,MZA),GVNA(MX,MY,MZA)
      REAL(8),INTENT(IN)::GXA(MX,MY,MZA),GYA(MX,MY,MZA),GZA(MX,MY,MZA)
      REAL(8),INTENT(IN)::XC(8,MX,MY),YC(8,MY),ZCA(8,MZA)
      REAL(8),INTENT(IN)::XCP(8,MX,MY)
      INTEGER,INTENT(IN)::INDPA(MX,MY,MZA),INDUA(MX,MY,MZA)
      INTEGER,INTENT(IN)::INDVA(MX,MY,MZA),INDWA(MX,MY,MZA)
      INTEGER,INTENT(IN)::KFA(MX,MY)
C
      REAL(8)::XFLG,YFLG,ZFLG,FFI,FFJ,FFX,FFY,FFZ,FFV,FFVN
      INTEGER:: I,J,K
C
C
      CALL ZERCLR(AUA,3*MXY*MZA,0.0D0)
      CALL ZERCLR(BBA,MXY*MZA,0.0D0)
C
C
C ... (1) 設定済みの値をコピー
      ADA(:,:,:)  =AD0A(:,:,:)
      ALA(:,:,:,:)=AL0A(:,:,:,:)
C
C
C ... (2) 海面について非対角項の係数を更新
      DO 100 J=2,MY
      DO 100 I=2,MX
c         DO 110 K=2,KFA(I,J)
         DO 110 K=2,MZMA
            IF( INDPA(I,J,K).GT.0 ) THEN
               XFLG = 1.0D0
               YFLG = 1.0D0
               ZFLG = 1.0D0
C ............ 流速の非計算点の場合、係数を0にする
               IF( INDUA(I-1,J,K).LE.0 ) XFLG = 0.0D0
               IF( INDVA(I,J-1,K).LE.0 ) YFLG = 0.0D0
               IF( INDWA(I,J,K-1).LE.0 ) ZFLG = 0.0D0
C
               FFI = FFA(I-1,J,K)*XC(7,I-1,J)+FFA(I,J,K)*XC(8,I-1,J)
               FFX = MAX(1.0D0-FFI-GXA(I-1,J,K),0.D0)
C
               FFJ = FFA(I,J-1,K)*YC(7,J-1)+FFA(I,J,K)*YC(8,J-1)
               FFY = MAX(1.0D0-FFJ-GYA(I,J-1,K),0.0D0)
C
               FFZ = 1.0D0-GZA(I,J,K-1)
C
               ALA(1,I,J,K)=-FFX*XC(5,I-1,J)*YC(4,J)*ZCA(4,K)*XFLG
               ALA(2,I,J,K)=-FFY*YC(5,J-1)*XCP(4,I,J-1)*ZCA(4,K)*YFLG
               ALA(3,I,J,K)=-FFZ*ZCA(5,K-1)*XC(4,I,J)*YC(4,J)*ZFLG
            ELSE
               ALA(1,I,J,K)=0.0D0
               ALA(2,I,J,K)=0.0D0
               ALA(3,I,J,K)=0.0D0
            END IF
  110    CONTINUE
  100 CONTINUE
C
C
C ... (3) 上三角の係数を設定
      DO 200 K=2,MZA
      DO 200 J=2,MY
      DO 200 I=2,MX
         AUA(1,I-1,J,K) = ALA(1,I,J,K)
         AUA(2,I,J-1,K) = ALA(2,I,J,K)
         AUA(3,I,J,K-1) = ALA(3,I,J,K)
  200 CONTINUE
C
C
C ... (3) 海面について対角項の係数を更新
      DO 300 J=2,MYM
      DO 300 I=2,MXM
         DO 310 K=2,MZMA
            IF( INDPA(I,J,K).GT.0 ) THEN
               ADA(I,J,K) = - ( ALA(1,I,J,K) + AUA(1,I,J,K)
     $                    +     ALA(2,I,J,K) + AUA(2,I,J,K)
     $                    +     ALA(3,I,J,K) + AUA(3,I,J,K) )
            ELSE
               ADA(I,J,K) = 1.0D0
            END IF
  310    CONTINUE
  300 CONTINUE
C
C
C ... (4) 右辺を設定する
      DO 400 J=2,MYM
      DO 400 I=2,MXM
c         DO 410 K=KFA(I,J),MZMA
         DO 410 K=2,MZMA
            IF( INDPA(I,J,K).GT.0 ) THEN
               FFVN=1.0D0-FFNA(I,J,K)-GVNA(I,J,K)
               FFV =1.0D0-FFA(I,J,K)-GVA(I,J,K)
               BBA(I,J,K) = - RHOAIR / DTV
     $            *( ( HUA(I  ,J,K)
     $            -    HUA(I-1,J,K) )*YC(4,J)*ZCA(4,K)
     $            +  ( HVA(I,J  ,K)*XCP(4,I,J  )
     $            -    HVA(I,J-1,K)*XCP(4,I,J-1) )*ZCA(4,K)
     $            +  ( HWA(I,J,K  )
     $            -    HWA(I,J,K-1) )*XC(4,I,J)*YC(4,J) )
c     $            +  (FFV-FFVN)/DTV*XC(4,I,J)*YC(4,J)*ZCA(4,K) )
            ENDIF
  410    CONTINUE
  400 CONTINUE
C
Cdbgc ... debug write
Cdbg      if( debug_air3.eq.1 ) then
Cdbg      do j=1,my
Cdbg         write(lp,*) 'ada j=',j,' istep=',istep
Cdbg         write(lp,'(a5,<mx>i8)') ' k%i|',(i,i=1,mxm)
Cdbg         do k=mza,1,-1
Cdbg            write(lp,'(i4,a1,<mx>f8.3)') k,'|',(ada(i,j,k),i=1,mx)
Cdbg         enddo
Cdbg         write(lp,*) ''
Cdbg      enddo
Cdbgc
Cdbg      do j=1,my
Cdbg         write(lp,*) 'ala(1) j=',j,' istep=',istep
Cdbg         write(lp,'(a5,<mx>i8)') ' k%i|',(i,i=1,mxm)
Cdbg         do k=mza,1,-1
Cdbg            write(lp,'(i4,a1,<mx>f8.3)') k,'|',(ala(1,i,j,k),i=1,mx)
Cdbg         enddo
Cdbg         write(lp,*) ''
Cdbg      enddo
Cdbgc
Cdbg      do j=1,my
Cdbg         write(lp,*) 'ala(2) j=',j,' istep=',istep
Cdbg         write(lp,'(a5,<mx>i8)') ' k%i|',(i,i=1,mxm)
Cdbg         do k=mza,1,-1
Cdbg            write(lp,'(i4,a1,<mx>f8.3)') k,'|',(ala(2,i,j,k),i=1,mx)
Cdbg         enddo
Cdbg         write(lp,*) ''
Cdbg      enddo
Cdbgc
Cdbg      do j=1,my
Cdbg         write(lp,*) 'ala(3) j=',j,' istep=',istep
Cdbg         write(lp,'(a5,<mx>i8)') ' k%i|',(i,i=1,mxm)
Cdbg         do k=mza,1,-1
Cdbg            write(lp,'(i4,a1,<mx>f8.3)') k,'|',(ala(3,i,j,k),i=1,mx)
Cdbg         enddo
Cdbg         write(lp,*) ''
Cdbg      enddo
Cdbgc
Cdbg      do j=1,my
Cdbg         write(lp,*) 'bba j=',j,' istep=',istep
Cdbg         write(lp,'(a5,<mx>i8)') ' k%i|',(i,i=1,mxm)
Cdbg         do k=mza,1,-1
Cdbg            write(lp,'(i4,a1,<mx>f8.3)') k,'|',(-dtv*bba(i,j,k),i=1,mx)
Cdbg         enddo
Cdbg         write(lp,*) ''
Cdbg      enddo
Cdbg      endif
Cdbgc
      RETURN
      END
