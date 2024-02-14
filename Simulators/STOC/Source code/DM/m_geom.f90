MODULE M_GEOM
  IMPLICIT NONE
!----------------------------------------
!     地形データ
!
!     HT(NA,NI,NJ) 標高値(m)
!     IDST(NA,NI,NJ) 破壊フラグ
!                     = -92  :  破壊の最中(漂流物による破壊)
!                     = -91  :  破壊の最中(流体による破壊)
!                     = -12  :  破壊後(漂流物による破壊)
!                     = -11  :  破壊後(流体による破壊)
!                     =  -1  :  非木造(非破壊)
!                     =   0  :  建物なし
!                     =  +1  :  木造(破壊の可能性あり)
!                     =  他の正の整数値を導入することで
!                        木造(破壊の可能性あり)に他の破壊基準を採用可能
!     KBLC(NA,NI,NJ) 閉塞フラグ
!                     =   0  :  閉塞なし
!                     =   K  :  閉塞
!                               漂流物底面(接触判定用)が含まれる流体側のセルインデックスK
!
!----------------------------------------
!
      REAL(8),ALLOCATABLE::HT(:,:,:)
!<<<<< (START) STOC-DS&DM VERSION  <<<<<<<
      INTEGER,ALLOCATABLE::IDST(:,:,:)
!<<<<<  (END)  STOC-DS&DM VERSION  <<<<<<<
!<<<<< (START) STOC-BLC VERSION  <<<<<<<
      INTEGER,ALLOCATABLE::KBLC(:,:,:)
!<<<<<  (END)  STOC-BLC VERSION  <<<<<<<
!
CONTAINS
!
SUBROUTINE ALLOCATE_GEOM(MXAREA,MXNI,MXNJ,IERROR)
  INTEGER,INTENT(IN) :: MXAREA,MXNI,MXNJ
  INTEGER,INTENT(OUT) :: IERROR
!
  IERROR=0
!
  ALLOCATE(HT(MXAREA,MXNI,MXNJ)   &
       &  ,IDST(MXAREA,MXNI,MXNJ) &
       &  ,KBLC(MXAREA,MXNI,MXNJ) &
       &  ,STAT=IERROR)
  IF(IERROR/=0) GOTO 900
!
  RETURN
900 CONTINUE
  WRITE(*,*) '### ERROR : ARRAY ALLOCATE ERROR : GEOM'
  IERROR=1
END SUBROUTINE ALLOCATE_GEOM
!
END MODULE M_GEOM
