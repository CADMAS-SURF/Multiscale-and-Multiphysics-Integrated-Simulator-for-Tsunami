MODULE M_FLUID
  IMPLICIT NONE
!----------------------------------------
!     流体データ
!
!     RHO       流体の密度(kg/m^3)
!
!     UFAR(NA,NI,NJ,NK)  流体のX方向速度成分(m／s)
!     VFAR(NA,NI,NJ,NK)  流体のY方向速度成分(m／s)
!     HFAR(NA,NI,NJ)     流体の水面位置のZ座標値(m)
!                        (ただし，配列は(NA,NNN)の２次元)
!                        NNN = NI(NA)*NJ(NA) * (K-1) + (NI(NA) * (J-1) + I)
!                        NNN = NI(NA) * (J-1) + I
!                        AR : ARRAY
!     WXAR(NA,NI,NJ)     風速のX方向速度成分(m／s)
!     WYAR(NA,NI,NJ)     風速のY方向速度成分(m／s)
!
!     UFAR1...WYAR1      時間積分により漂流物の状態を更新した後の流体の状態
!----------------------------------------
!
      REAL(8)::RHO
!
      REAL(8),ALLOCATABLE::UFAR(:,:)
      REAL(8),ALLOCATABLE::VFAR(:,:)
      REAL(8),ALLOCATABLE::HFAR(:,:)
      REAL(8),ALLOCATABLE::WXAR(:,:)
      REAL(8),ALLOCATABLE::WYAR(:,:)
!
      REAL(8),ALLOCATABLE::UFAR1(:,:)
      REAL(8),ALLOCATABLE::VFAR1(:,:)
      REAL(8),ALLOCATABLE::HFAR1(:,:)
      REAL(8),ALLOCATABLE::WXAR1(:,:)
      REAL(8),ALLOCATABLE::WYAR1(:,:)
!
CONTAINS
!
SUBROUTINE ALLOCATE_FLUID(MXAREA,MXNIJ,MXNIJK,IERROR)
  INTEGER,INTENT(IN) :: MXAREA,MXNIJ,MXNIJK
  INTEGER,INTENT(OUT) :: IERROR
!
  IERROR=0
!
  ALLOCATE(UFAR(MXAREA,MXNIJK) &
       &  ,VFAR(MXAREA,MXNIJK) &
       &  ,HFAR(MXAREA,MXNIJ ) &
       &  ,WXAR(MXAREA,MXNIJ ) &
       &  ,WYAR(MXAREA,MXNIJ ) &
       &  ,UFAR1(MXAREA,MXNIJK) &
       &  ,VFAR1(MXAREA,MXNIJK) &
       &  ,HFAR1(MXAREA,MXNIJ ) &
       &  ,WXAR1(MXAREA,MXNIJ ) &
       &  ,WYAR1(MXAREA,MXNIJ ) &
       &  ,STAT=IERROR)
  IF(IERROR/=0) GOTO 900
!
  RETURN
900 CONTINUE
  WRITE(*,*) '### ERROR : ARRAY ALLOCATE ERROR : FLUID'
  IERROR=1
END SUBROUTINE ALLOCATE_FLUID
!
END MODULE M_FLUID
