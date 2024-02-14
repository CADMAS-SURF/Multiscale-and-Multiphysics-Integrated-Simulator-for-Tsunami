      SUBROUTINE BCTSRF(SRCA,SRCB,FW,TT,HH,HX,HDEP,QQ,XC,YC,ZC,GV,
     $                  INDP,KF,KH,KG)
C======================================================================
C
C     エネルギー式のZ方向流束に表面熱条件を加える
C     (表面がz方向に2メッシュ以上差がないものと仮定) 
C       FW: X方向セル中心点、Y方向セル中心点、Z方向格子点で定義
C         -σT**4=-σ(T(n)**3*T(n+1)-3*T(n)**4)
C
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'BOUNDI.h'
      INCLUDE 'BOUNDR.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'TIMER.h'
C
      REAL(8),INTENT(INOUT)::SRCA(MX,MY,MZ),SRCB(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::FW(MX,MY,MZ),TT(MX,MY,MZ),GV(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HH(MX,MY),HX(MX,MY),HDEP(MX,MY)
      REAL(8),INTENT(INOUT)::QQ(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KH(MX,MY),KG(MX,MY)
C
      REAL(8)::STEFB=5.6704D-8
C
      REAL(8)::STEFG,VOL,DRHOCP,Q1,Q2,TN273,TS1
      REAL(8)::DH
      INTEGER::I,J,K,KF1
C
C----------------------------------------------------------------------
C     SURFACE-TYPE = FUNCTIONの場合
C----------------------------------------------------------------------
      IF(ISURF(1).EQ.-1) THEN
C
      STEFG = STEFB
      DRHOCP = 1.0D0/(RHO*CP)
C
      DO 100 J=2,MYM
      DO 100 I=2,MXM
      IF(KH(I,J).LT.MZ) THEN
C
        KF1 = KH(I,J)
        IF(INDP(I,J,KF1-1).GT.0) THEN
C
C ....... 表層部の体積(高さ)を合計する
          VOL = GV(I,J,KF1-1)*ZC(4,KF1-1)
     $         +GV(I,J,KF1  )*(HX(I,J)-ZC(1,KF1-1))
C
C ....... 上向き長波放射成分を減じる
          TS1     = TT(I,J,KF1-1)
          TN273   = TS1+273.15D0
          QQ(I,J,1) = QQ(I,J,1)-STEFG*TN273**4
          QQ(I,J,1) = QQ(I,J,1)*DRHOCP
C
          Q1 =QQ(I,J,1)*GV(I,J,KF1-1)*ZC(4,KF1-1)/VOL
          Q2 =QQ(I,J,1)*GV(I,J,KF1  )*(HX(I,J)-ZC(1,KF1-1))/VOL
C
          IF(VOL.GT.0.0) THEN
c            SRCB(I,J,KF1-1) = SRCB(I,J,KF1-1)+Q1*ZC(6,KF1-1)
c            SRCB(I,J,KF1  ) = SRCB(I,J,KF1  )+Q2/(HX(I,J)-ZC(1,KF1-1))
cZC(6,KF1  )
            FW(I,J,KF1-1) = FW(I,J,KF1-1)+Q1
            FW(I,J,KF1  ) = FW(I,J,KF1  )+QQ(I,J,1)
            DO 120 K=KF1+1,MZM
              FW(I,J,K) = FW(I,J,K)+QQ(I,J,1)
  120       CONTINUE         
          END IF
C
          SRCA(I,J,KF1-1)=4.0D0*STEFG*DRHOCP*TN273**3
          SRCA(I,J,KF1  )=4.0D0*STEFG*DRHOCP*TN273**3
C
        ELSE
          TS1     = TT(I,J,KF1)
          TN273   = TS1+273.15D0
          QQ(I,J,1) = QQ(I,J,1)-STEFG*TN273**4
          QQ(I,J,1) = QQ(I,J,1)*DRHOCP
C
c          SRCB(I,J,KF1  ) = SRCB(I,J,KF1  )+QQ(I,J,1)*ZC(6,KF1  )
c
          FW(I,J,KF1) = FW(I,J,KF1)+QQ(I,J,1)
          DO 130 K=KF1+1,MZM
             FW(I,J,K) = FW(I,J,K)+QQ(I,J,1)
  130     CONTINUE         
C
          SRCA(I,J,KF1)=4.0D0*STEFG*DRHOCP*TN273**3
        END IF
      END IF
  100 CONTINUE
C
C----------------------------------------------------------------------
C     SURFACE-TYPE = FUNCTION2の場合
C----------------------------------------------------------------------
      ELSE IF(ISURF(1).EQ.-2) THEN
         DRHOCP = 1.0D0/(RHO*CP)
         DO K=2,MZM
         DO J=2,MYM
         DO I=2,MXM
            DH = GV(I,J,K)*ZC(4,K)
            IF( K.EQ.KF(I,J) ) THEN
               IF( KF(I,J).EQ.KG(I,J) ) THEN
                  DH = HH(I,J)-HDEP(I,J)
               ELSE
                  DH = HH(I,J)-ZC(1,K-1)
               ENDIF
            ENDIF
            SRCB(I,J,K) = QQ(I,J,K)*DRHOCP/DH
CDEBUG            if( i.eq.5.and.j.eq.5 ) then
CDEBUG               write(33,*) k,qq(i,j,k)
CDEBUG            endif
         ENDDO
         ENDDO
         ENDDO
      ENDIF
C
      RETURN
      END
