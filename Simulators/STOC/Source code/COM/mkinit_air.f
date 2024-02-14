      SUBROUTINE MKINIT_AIR(PPA,UUA,VVA,WWA,AKA,EPA,
     $                      INDPA,INDUA,INDVA,INDWA,KFA)
C======================================================================
C     風場の初期条件の設定
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
      INCLUDE 'FILE.h'
      INCLUDE 'TURBR.h'
C
      REAL(8),INTENT(OUT):: PPA(MX,MY,MZA)
      REAL(8),INTENT(OUT):: UUA(MX,MY,MZA),VVA(MX,MY,MZA),WWA(MX,MY,MZA)
      REAL(8),INTENT(OUT):: AKA(MX,MY,MZA),EPA(MX,MY,MZA)
      INTEGER,INTENT(IN):: INDPA(MX,MY,MZA),INDUA(MX,MY,MZA)
      INTEGER,INTENT(IN):: INDVA(MX,MY,MZA),INDWA(MX,MY,MZA)
      INTEGER,INTENT(IN):: KFA(MX,MY)
C
      INTEGER::I,J,K
C
C
      PPA(:,:,:)=0.0D0
      UUA(:,:,:)=0.0D0
      VVA(:,:,:)=0.0D0
      WWA(:,:,:)=0.0D0
      IF(LTURBA.EQ.2) THEN
         AKA(:,:,:)=0.0D0
         EPA(:,:,:)=0.0D0
      ENDIF
C
      DO K=2,MZMA
      DO J=2,MYM
      DO I=2,MXM
         IF( INDPA(I,J,K).GT.0 ) THEN
            UUA(I,J,K) = UINITAIR
            VVA(I,J,K) = VINITAIR
            IF(LTURBA.EQ.2) THEN
               AKA(I,J,K) = AKINITAIR
               EPA(I,J,K) = EPINITAIR
            ENDIF
         ENDIF
      ENDDO
      ENDDO
      ENDDO
C
      RETURN
      END
