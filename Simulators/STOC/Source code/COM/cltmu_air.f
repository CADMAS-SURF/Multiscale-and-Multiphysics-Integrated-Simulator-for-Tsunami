      SUBROUTINE CLTMU_AIR(TMUA,AKA,EPA,UUA,VVA,WWA,XC,YC,ZCA,
     $                     INDUA,INDVA,INDWA,INDPA,UT,VT,WT)
C----------------------------------------------------------------------
C     乱流粘性係数の計算
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'TURBR.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'AIRI.h'
C
      REAL(8),INTENT(OUT)::TMUA(MX,MY,MZA)
      REAL(8)::UT(MX,MY,MZA),VT(MX,MY,MZA),WT(MX,MY,MZA)
      REAL(8),INTENT(IN)::AKA(MX,MY,MZA),EPA(MX,MY,MZA)
      REAL(8),INTENT(IN)::UUA(MX,MY,MZA),VVA(MX,MY,MZA)
      REAL(8),INTENT(IN)::WWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::XC(8,MX,MY),YC(8,MY),ZCA(8,MZA)
      INTEGER,INTENT(IN)::INDUA(MX,MY,MZA),INDVA(MX,MY,MZA)
      INTEGER,INTENT(IN)::INDWA(MX,MY,MZA),INDPA(MX,MY,MZA)
C
      REAL(8)::CPWR=0.333333333333333D0
C
      REAL(8)::AKMAX,EPMAX,SXX,SXY,SYY,SYZ,SZX,SZZ,TMMAX
      REAL(8)::UTVM,UTVP,UTWM,UTWP,VTUM,VTUP,VTWM,VTWP
      REAL(8)::WTUM,WTUP,WTVM,WTVP
      INTEGER::I,IKMX,J,JKMX,K,KKMX
C
C
      CALL ZERCLR(TMUA,MXY*MZA,0.0D0)
C
C ... LESモデル
      IF(LTURBA.EQ.1) THEN
         CALL ZERCLR(UT,MXY*MZA,0.0D0)
         CALL ZERCLR(VT,MXY*MZA,0.0D0)
         CALL ZERCLR(WT,MXY*MZA,0.0D0)
C
         DO 100 K=2,MZMA
         DO 100 J=2,MYM
         DO 100 I=2,MXM
            IF(INDPA(I,J,K).GT.0) THEN
               UT(I,J,K) = 0.5D0*(UUA(I-1,J,K)+UUA(I,J,K))
               VT(I,J,K) = 0.5D0*(VVA(I,J-1,K)+VVA(I,J,K))
               WT(I,J,K) = 0.5D0*(WWA(I,J,K-1)+WWA(I,J,K))
            ENDIF
  100    CONTINUE
C
         DO 200 K=2,MZMA
         DO 200 J=2,MYM
         DO 200 I=2,MXM
            IF(INDPA(I,J,K).GT.0) THEN
               IF(INDVA(I,J,K).EQ.1) THEN
                  UTVP=YC(8,J)*UT(I,J+1,K)+YC(7,J)*UT(I,J,K)
                  WTVP=YC(8,J)*WT(I,J+1,K)+YC(7,J)*WT(I,J,K)
               ELSE
                  UTVP=UT(I,J,K)
                  WTVP=WT(I,J,K)
               ENDIF
C
               IF(INDVA(I,J-1,K).EQ.1) THEN
                  UTVM=YC(8,J-1)*UT(I,J,K)+YC(7,J-1)*UT(I,J-1,K)
                  WTVM=YC(8,J-1)*WT(I,J,K)+YC(7,J-1)*WT(I,J-1,K)
               ELSE
                  UTVM=UT(I,J,K)
                  WTVM=WT(I,J,K)
               ENDIF
C
               IF(INDWA(I,J,K).EQ.1) THEN
                  UTWP=ZCA(8,K)*UT(I,J,K+1)+ZCA(7,K)*UT(I,J,K)
                  VTWP=ZCA(8,K)*VT(I,J,K+1)+ZCA(7,K)*VT(I,J,K)
               ELSE
                  UTWP=UT(I,J,K)
                  VTWP=VT(I,J,K)
               ENDIF
C
               IF(INDWA(I,J,K-1).EQ.1) THEN
                  UTWM=ZCA(8,K-1)*UT(I,J,K  )+ZCA(7,K-1)*UT(I,J,K-1)
                  VTWM=ZCA(8,K-1)*VT(I,J,K  )+ZCA(7,K-1)*VT(I,J,K-1)
               ELSE
                  UTWM=UT(I,J,K)
                  VTWM=VT(I,J,K)
               ENDIF
C
               IF(INDUA(I,J,K).EQ.1) THEN
                  VTUP=XC(8,I,J)*VT(I+1,J,K)+XC(7,I,J)*VT(I,J,K)
                  WTUP=XC(8,I,J)*WT(I+1,J,K)+XC(7,I,J)*WT(I,J,K)
               ELSE
                  VTUP=VT(I,J,K)
                  WTUP=WT(I,J,K)
               ENDIF
C
               IF(INDUA(I-1,J,K).EQ.1) THEN
                  VTUM=XC(8,I-1,J)*VT(I,J,K)+XC(7,I-1,J)*VT(I-1,J,K)
                  WTUM=XC(8,I-1,J)*WT(I,J,K)+XC(7,I-1,J)*WT(I-1,J,K)
               ELSE
                  VTUM=VT(I,J,K)
                  WTUM=WT(I,J,K)
               ENDIF
C
               SXX=XC(6,I,J)*(UUA(I,J,K)-UUA(I-1,J,K))
               SYY=YC(6,J)*(VVA(I,J,K)-VVA(I,J-1,K))
               SZZ=ZCA(6,K)*(WWA(I,J,K)-WWA(I,J,K-1))
               SXY=(YC(6,J)*(UTVP-UTVM)+XC(6,I,J)*(VTUP-VTUM))*0.5D0
               SYZ=(ZCA(6,K)*(VTWP-VTWM)+YC(6,J)*(WTVP-WTVM))*0.5D0
               SZX=(XC(6,I,J)*(WTUP-WTUM)+ZCA(6,K)*(UTWP-UTWM))*0.5D0
C
               TMUA(I,J,K)=(CSMG*(XC(4,I,J)*YC(4,J)*ZCA(4,K))**CPWR)**2
     $          *SQRT(SXX**2+SYY**2+SZZ**2+2.0D0*(SXY**2+SYZ**2+SZX**2))
            ENDIF
  200    CONTINUE
C
C ... K-εモデル
      ELSEIF( LTURBA.EQ.2 ) THEN
         TMMAX = 1.0D5
         IKMX = 0
         JKMX = 0
         KKMX = 0
         DO 300 K=2,MZMA
         DO 300 J=2,MYM
         DO 300 I=2,MXM
            IF(INDPA(I,J,K).GT.0) THEN
               IF(EPA(I,J,K).GT.0.0D0)
     $            TMUA(I,J,K)=CMU*AKA(I,J,K)**2/EPA(I,J,K)
               IF(TMUA(I,J,K).GT.TMMAX) THEN
                  IKMX = I
                  JKMX = J
                  KKMX = K
                  AKMAX = AKA(I,J,K)
                  EPMAX = EPA(I,J,K)
                  TMMAX = TMUA(I,J,K)
               ENDIF
            ENDIF
  300    CONTINUE
C
         IF(IKMX.NE.0) THEN
            WRITE(6,600) IKMX,JKMX,KKMX,TMMAX,AKMAX,EPMAX
  600       FORMAT('## TMU-MAX-AIR I,J,K  TMUA,AKA,EPA=',3I5,1P,3E13.5)
         ENDIF
      ENDIF
C
      RETURN
      END
