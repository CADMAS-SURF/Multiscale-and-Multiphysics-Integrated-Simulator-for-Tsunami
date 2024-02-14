      SUBROUTINE MKCOE1_AIR(AD0A,AL0A,XC,YC,ZCA,XCP,
     $                      INDPA,INDUA,INDVA,INDWA)
C======================================================================
C     圧力補正式の係数行列を作成する(水面移動を考慮しない場合)
C
C     係数は以下の表式に基づく
C         ∂      ∂ δp                   ρ  ∂ u~_j
C     - ─── ( ──── ) ΔxΔyΔz = - ── ──── ΔxΔyΔz
C       ∂x_j      ∂x_j                  Δt   ∂x_j
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'FILE.h'
C
      REAL(8),INTENT(OUT)::AD0A(MX,MY,MZA),AL0A(3,MX,MY,MZA)
      REAL(8),INTENT(IN)::XC(8,MX,MY),YC(8,MY),ZCA(8,MZA)
      REAL(8),INTENT(IN)::XCP(8,MX,MY)
C
      INTEGER,INTENT(INOUT)::INDPA(MX,MY,MZA),INDUA(MX,MY,MZA)
      INTEGER,INTENT(INOUT)::INDVA(MX,MY,MZA),INDWA(MX,MY,MZA)
C
      REAL(8)::XFLG,YFLG,ZFLG
      INTEGER::I,J,K
C
C
C ... 初期化
      CALL ZERCLR(AD0A,MXY*MZA,0.0D0)
      CALL ZERCLR(AL0A,3*MXY*MZA,0.0D0)
C
C
C ... 非対角項の係数を設定
      DO 100 K=2,MZMA
      DO 100 J=2,MY
      DO 100 I=2,MX
         IF( INDPA(I,J,K).GT.0 ) THEN
            XFLG = 1.0D0
            YFLG = 1.0D0
            ZFLG = 1.0D0
C ......... 流速の非計算点の場合、係数を0にする
            IF( INDUA(I-1,J,K).LE.0 ) XFLG = 0.0D0
            IF( INDVA(I,J-1,K).LE.0 ) YFLG = 0.0D0
            IF( INDWA(I,J,K-1).LE.0 ) ZFLG = 0.0D0
C
            AL0A(1,I,J,K)=-XC(5,I-1,J)*YC(4,J)*ZCA(4,K)*XFLG
     $                   
            AL0A(2,I,J,K)=-YC(5,J-1)*XCP(4,I,J-1)*ZCA(4,K)*YFLG
            AL0A(3,I,J,K)=-ZCA(5,K-1)*XC(4,I,J)*YC(4,J)*ZFLG
         END IF
  100 CONTINUE
C
C
C ... 対角項の係数を設定
      DO 200 K=2,MZMA
      DO 200 J=2,MYM
      DO 200 I=2,MXM
         IF( INDPA(I,J,K).GT.0 ) THEN
            AD0A(I,J,K) = - ( AL0A(1,I,J,K) + AL0A(1,I+1,J,K)
     $                 +      AL0A(2,I,J,K) + AL0A(2,I,J+1,K)
     $                 +      AL0A(3,I,J,K) + AL0A(3,I,J,K+1) )
         END IF
  200 CONTINUE
C
C
C ... 自由流入出境界の補正(上面)
C
      K =MZMA
      DO J=2,MYM
      DO I=2,MXM
C ...... +Z方向が境界
         AD0A(I,J,K)=AD0A(I,J,K)+2.0D0*ZCA(6,K)*XC(4,I,J)*YC(4,J)
      ENDDO
      ENDDO
C
c ... debug write
      if( debug_air2.eq.1 ) then
      do j=1,my
         write(lp,*) 'ad0a j=',j
         do k=mza,1,-1
            write(lp,'(i4,a1,100f6.2)') k,'|',(ad0a(i,j,k),i=1,mx)
         enddo
         write(lp,*) ''
      enddo
c
      do j=1,my
         write(lp,*) 'al0a(1) j=',j
         do k=mza,1,-1
            write(lp,'(i4,a1,100f6.2)') k,'|',(al0a(1,i,j,k),i=1,mx)
         enddo
         write(lp,*) ''
      enddo
c
      do j=1,my
         write(lp,*) 'al0a(2) j=',j
         do k=mza,1,-1
            write(lp,'(i4,a1,100f6.2)') k,'|',(al0a(2,i,j,k),i=1,mx)
         enddo
         write(lp,*) ''
      enddo
c
      do j=1,my
         write(lp,*) 'al0a(3) j=',j
         do k=mza,1,-1
            write(lp,'(i4,a1,100f6.2)') k,'|',(al0a(3,i,j,k),i=1,mx)
         enddo
         write(lp,*) ''
      enddo
      endif
c
      RETURN
      END
