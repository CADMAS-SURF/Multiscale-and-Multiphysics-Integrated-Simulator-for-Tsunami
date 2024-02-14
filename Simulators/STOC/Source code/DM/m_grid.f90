MODULE M_GRID
  USE M_COM_STOC,ONLY: MAXPE
  IMPLICIT NONE
!----------------------------------------
!     流体の格子データ
!
!     NA           領域番号
!
!     NI(NA)       X方向の流体セル数
!     NJ(NA)       Y方向の流体セル数
!     NK(NA)       Z方向の流体セル数
!     ISUB(3,NA)   部分領域のX方向の流体セルのサイズ等
!                  (1,*)：開始番号
!                  (2,*)：終了番号
!                  (3,*)：サイズ（=終了番号-開始番号+1）
!     JSUB(3,NA)   部分領域のY方向の流体セルのサイズ等
!     KSUB(3,NA)   部分領域のZ方向の流体セルのサイズ等
!
!     DX(NA)       X方向の格子間隔
!     DY(NA)       Y方向の格子間隔
!     DZ(NA,NK)    Z方向の格子間隔(Z方向のみ不等間隔を可能とする)
!
!     XG(NA,0:NI)  X方向の格子点座標値
!     YG(NA,0:NJ)  Y方向の格子点座標値
!     ZG(NA,0:NK)  Z方向の格子点座標値
!     XC(NA,NI)    X方向のセル中心座標値
!     YC(NA,NJ)    Y方向のセル中心座標値
!     ZC(NA,NK)    Z方向のセル中心座標値
!----------------------------------------
!
      INTEGER::MXAREA=1
      INTEGER::MXNI,MXNJ,MXNK
      INTEGER::MXNIJ
      INTEGER::MXNIJK
      INTEGER::ISUB(3,MAXPE),JSUB(3,MAXPE),KSUB(3,MAXPE)
      INTEGER::NI(MAXPE),NJ(MAXPE),NK(MAXPE)
      REAL(8),ALLOCATABLE::DX(:),DY(:),DZ(:,:)
      REAL(8),ALLOCATABLE::XG(:,:),YG(:,:),ZG(:,:)
      REAL(8),ALLOCATABLE::XC(:,:),YC(:,:),ZC(:,:)
!
CONTAINS
!
SUBROUTINE ALLOCATE_GRID(MXAREA,MXNI,MXNJ,MXNK,IERROR)
  INTEGER,INTENT(IN) :: MXAREA,MXNI,MXNJ,MXNK
  INTEGER,INTENT(OUT) :: IERROR
!
  IERROR=0
!
  ALLOCATE(DX(MXAREA) &
       &  ,DY(MXAREA) &
       &  ,DZ(MXAREA,MXNK) &
       &  ,XG(MXAREA,0:MXNI) &
       &  ,YG(MXAREA,0:MXNJ) &
       &  ,ZG(MXAREA,0:MXNK) &
       &  ,XC(MXAREA,MXNI) &
       &  ,YC(MXAREA,MXNJ) &
       &  ,ZC(MXAREA,MXNK) &
       &  ,STAT=IERROR)
  IF(IERROR/=0) GOTO 900
  DX(:)=0.0D0
  DY(:)=0.0D0
  DX(:)=0.0D0
  XG(:,:)=0.0D0
  YG(:,:)=0.0D0
  ZG(:,:)=0.0D0
  XC(:,:)=0.0D0
  YC(:,:)=0.0D0
  ZC(:,:)=0.0D0
!
  RETURN
900 CONTINUE
  WRITE(*,*) '### ERROR : ARRAY ALLOCATE ERROR : GRID'
  IERROR=1
END SUBROUTINE ALLOCATE_GRID
!
END MODULE M_GRID
