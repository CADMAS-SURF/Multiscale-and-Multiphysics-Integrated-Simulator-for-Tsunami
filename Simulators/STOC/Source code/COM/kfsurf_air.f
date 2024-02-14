      SUBROUTINE KFSURF_AIR(FFA,KFA,HH,ZCA,INDPA)
C======================================================================
C     水面に関する位置インデックスを設定する
C       KP: 圧力境界条件が適用されはじめるセルのz方向セルインデックス
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'FILE.h'
C
      REAL(8),INTENT(OUT)::FFA(MX,MY,MZA)
      INTEGER,INTENT(OUT)::KFA(MX,MY)
      REAL(8),INTENT(IN)::HH(MX,MY),ZCA(8,MZA)
      INTEGER,INTENT(IN)::INDPA(MX,MY,MZA)
C
      INTEGER::I,J,K
C
C
      FFA=0.D0
C
      DO 50 K=2,MZMA
      DO 50 J=2,MYM
      DO 50 I=2,MXM
         IF(INDPA(I,J,K).GT.0) THEN
            IF(HH(I,J).GE.ZCA(1,K)) THEN
               FFA(I,J,K) = 1.0D0
            ELSE IF(HH(I,J).LT.ZCA(1,K-1)) THEN
               FFA(I,J,K) = 0.0D0
            ELSE
               FFA(I,J,K) = (HH(I,J)-ZCA(1,K-1))*ZCA(6,K)
               KFA(I,J) = K
            END IF
         END IF
   50 CONTINUE
C
      if( debug_air8.eq.1 ) then
         j=2
         write(LP,'(a3,7x,a2,1x,a3)') '# i','hh','kfa'
         do i=2,mxm
            write(LP,'(i3,1x,f8.4,1x,i3)') i, hh(i,j),kfa(i,j)
         enddo
      endif
C
      RETURN
      END
