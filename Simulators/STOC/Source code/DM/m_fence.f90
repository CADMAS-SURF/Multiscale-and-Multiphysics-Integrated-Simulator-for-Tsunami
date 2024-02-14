MODULE M_FENCE
  IMPLICIT NONE
!----------------------------------------
!     フェンス状構造物データ
!
!     NFC       フェンス状構造物の数
!
!     XFC1(NFC) フェンス状構造物の線分の端点1のX座標(m)
!     YFC1(NFC) フェンス状構造物の線分の端点1のY座標(m)
!     XFC2(NFC) フェンス状構造物の線分の端点2のX座標(m)
!     YFC2(NFC) フェンス状構造物の線分の端点2のY座標(m)
!     XFCD(NFC) フェンス状構造物の線分の中点のX座標(m)
!     YFCD(NFC) フェンス状構造物の線分の中点のY座標(m)
!     ZFC(NFC)  フェンス状構造物の最上部のz座標(m)
!
!     DFC_LIMIT(NFC)  フェンス状構造物の浸水深による破壊判定値(m)
!     EFC_LIMIT(NFC)  フェンス状構造物の漂流物との衝突による破壊判定値(J)
!
!     LFC(NFC)  フェンス状構造物の状態フラグ
!               = 0: 通常状態
!               = 1: 破壊状態
!               = -1: 計算対象外(領域外にある場合等)
!     KFC(NFC)  フェンス状構造物の高さデータの意味(一時変数)
!               = 0: 地盤からの高さ
!               = 1: z座標値
!
!----------------------------------------
!
      INTEGER::NFC
!
      REAL(8),ALLOCATABLE::XFC1(:),YFC1(:)
      REAL(8),ALLOCATABLE::XFC2(:),YFC2(:)
      REAL(8),ALLOCATABLE::XFCD(:),YFCD(:)
      REAL(8),ALLOCATABLE::ZFC(:)
!
      REAL(8),ALLOCATABLE::DFC_LIMIT(:),EFC_LIMIT(:)
!
      INTEGER,ALLOCATABLE::LFC(:),KFC(:)
!
CONTAINS
!
SUBROUTINE ALLOCATE_FENCE(NFC,IERROR)
  INTEGER,INTENT(IN) :: NFC
  INTEGER,INTENT(OUT) :: IERROR
!
  IERROR=0
!
  ALLOCATE(XFC1(NFC),YFC1(NFC) &
       &  ,XFC2(NFC),YFC2(NFC) &
       &  ,XFCD(NFC),YFCD(NFC) &
       &  ,ZFC(NFC),DFC_LIMIT(NFC),EFC_LIMIT(NFC) &
       &  ,STAT=IERROR)
  IF(IERROR/=0) GOTO 900
!
  ALLOCATE(LFC(NFC),KFC(NFC) &
       &  ,STAT=IERROR)
  IF(IERROR/=0) GOTO 901
!
  RETURN
900 CONTINUE
  WRITE(*,*) '### ERROR : ARRAY ALLOCATE ERROR : FENCE : REAL'
  IERROR=1
  RETURN
901 CONTINUE
  WRITE(*,*) '### ERROR : ARRAY ALLOCATE ERROR : FENCE : INTEGER'
  IERROR=1
  RETURN
END SUBROUTINE ALLOCATE_FENCE
!
END MODULE M_FENCE
