      SUBROUTINE DEFALT
!//////////////////////////////////////////////////
!     コモン変数を初期化する
!//////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'CONTROL.h'
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
!
      AREAFILE=''
      STRFILE=''
      ENDFILE=''
      CMODEL='AVERAGE'
      SEED =0
      N_TBL=0
      TBL_INUND(:)=0.D0
      TBL_PDST(:)=0.D0
      DIVX=2
      DIVY=2
      RHOD=380.D0
      QD  =230.D0
      HD  = QD/RHOD
      RHOL=1025.D0
      G_INUND=2.0D0
!
      INP=51
      LP=6
!
      CNUL=''
      CFLNM=''
      IFLNM=0
!
      RETURN
      END
