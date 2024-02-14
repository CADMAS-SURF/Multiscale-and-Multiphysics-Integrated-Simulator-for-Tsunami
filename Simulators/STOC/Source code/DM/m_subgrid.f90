MODULE M_SUBGRID
  IMPLICIT NONE
!----------------------------------------
!     接触計算で用いるサブグリッドに関するデータ
!
!     MAXDIV_X     サブグリッドのX方向の最大分割数
!     MAXDIV_Y     サブグリッドのY方向の最大分割数
!     NX_SUB       サブグリッドのX方向の分割数
!     NY_SUB       サブグリッドのY方向の分割数
!
!     X0_SUB       サブグリッドの原点のX座標
!     Y0_SUB       サブグリッドの原点のY座標
!     DX_SUB       サブグリッドのX方向の格子間隔
!     DY_SUB       サブグリッドのY方向の格子間隔
!     DMIN_SUB     サブグリッドの格子間隔の下限値
!
!     ND_SUB       サブグリッド(I,J)に存在する漂流物やフェンスの数
!     I1_SUB       サブグリッド(I,J)に存在する漂流物やフェンスの番号リスト
!                  IPTR_SUBの先頭要素へのポインタ
!     IPTR_SUB     サブグリッド順に並べ替えた漂流物やフェンスの番号リスト
!     I_SUB        漂流物が属するサブグリッドのインデックスI
!     J_SUB        漂流物が属するサブグリッドのインデックスJ
!----------------------------------------
!
      REAL(8),PARAMETER:: CSAFE_SUBGRID=1.5D0 ! 最小サブグリッドサイズの安全係数(>=1)
      INTEGER::MAXDIV_X=500
      INTEGER::MAXDIV_Y=500
      INTEGER::NX_SUB,NY_SUB
      REAL(8)::X0_SUB,Y0_SUB,DX_SUB,DY_SUB
      REAL(8)::DMIN_SUB
!
      INTEGER,ALLOCATABLE:: ND_SUB(:,:),I1_SUB(:,:)
      INTEGER,ALLOCATABLE:: IPTR_SUB(:)
      INTEGER,ALLOCATABLE:: I_SUB(:),J_SUB(:)
!
CONTAINS
!
!
SUBROUTINE SET_DMIN_SUBGRID(BL,BB,XFC1,XFC2,YFC1,YFC2,ND,NFC)
  INTEGER,INTENT(IN):: ND,NFC
  REAL(8),INTENT(IN):: BL(ND),BB(ND)
  REAL(8),INTENT(IN):: XFC1(NFC),XFC2(NFC)
  REAL(8),INTENT(IN):: YFC1(NFC),YFC2(NFC)
  REAL(8):: XXMIN,XLEN
  INTEGER:: N
!
  XXMIN=0.D0
  DO N=1,ND
     XXMIN=MAX(XXMIN,BL(N),BB(N))
  ENDDO
  DO N=1,NFC
     XLEN=SQRT((XFC1(N)-XFC2(N))**2+(YFC1(N)-YFC2(N))**2)
     XXMIN=MAX(XXMIN,XLEN)
  ENDDO
!
  DMIN_SUB=XXMIN*CSAFE_SUBGRID  
  write(*,*) 'DMIN_SUB=',DMIN_SUB
!
  RETURN
END SUBROUTINE SET_DMIN_SUBGRID
!
!
SUBROUTINE ALLOCATE_SUBGRID(XD,YD,XFCD,YFCD,ND,NFC,IERROR)
  INTEGER,INTENT(IN) :: ND,NFC
  REAL(8),INTENT(IN) :: XD(ND),YD(ND),XFCD(NFC),YFCD(NFC)
  INTEGER,INTENT(OUT) :: IERROR
  REAL(8),PARAMETER:: EPS=1.D-3
  REAL(8):: XMIN,XMAX,YMIN,YMAX
  INTEGER:: M,N,I,J
!
  IERROR=0
!
  XMIN=MIN(MINVAL(XD(:)),MINVAL(XFCD(:)))-EPS
  XMAX=MAX(MAXVAL(XD(:)),MAXVAL(XFCD(:)))+EPS
  YMIN=MIN(MINVAL(YD(:)),MINVAL(YFCD(:)))-EPS
  YMAX=MAX(MAXVAL(YD(:)),MAXVAL(YFCD(:)))+EPS
!
  X0_SUB=XMIN
  Y0_SUB=YMIN
!
  IF(XMAX-XMIN<DMIN_SUB*DBLE(MAXDIV_X)) THEN
     DX_SUB=DMIN_SUB
     NX_SUB=INT((XMAX-XMIN)/DMIN_SUB)+1
  ELSE
     DX_SUB=(XMAX-XMIN)/DBLE(MAXDIV_X)
     NX_SUB=MAXDIV_X
  ENDIF
!
  IF(YMAX-YMIN<DMIN_SUB*DBLE(MAXDIV_Y)) THEN
     DY_SUB=DMIN_SUB
     NY_SUB=INT((YMAX-YMIN)/DMIN_SUB)+1
  ELSE
     DY_SUB=(YMAX-YMIN)/DBLE(MAXDIV_Y)
     NY_SUB=MAXDIV_Y
  ENDIF
!
  IF( ALLOCATED(ND_SUB) ) DEALLOCATE(ND_SUB,I1_SUB,IPTR_SUB,I_SUB,J_SUB)
  ALLOCATE(ND_SUB(NX_SUB,NY_SUB),I1_SUB(NX_SUB,NY_SUB),IPTR_SUB(ND+NFC), &
       & I_SUB(-NFC:ND),J_SUB(-NFC:ND),STAT=IERROR)
  IF( IERROR/=0 ) GOTO 900
!
!
! (1) 各サブグリッドの中の漂流物の数をカウントする
  ND_SUB(:,:)=0
  I_SUB(:)=0
  J_SUB(:)=0
!
  DO N=1,ND
     I=INT((XD(N)-X0_SUB)/DX_SUB)+1
     J=INT((YD(N)-Y0_SUB)/DY_SUB)+1
     I_SUB(N)=I
     J_SUB(N)=J
     ND_SUB(I,J)=ND_SUB(I,J)+1
  ENDDO
  DO N=1,NFC
     I=INT((XFCD(N)-X0_SUB)/DX_SUB)+1
     J=INT((YFCD(N)-Y0_SUB)/DY_SUB)+1
     I_SUB(-N)=I
     J_SUB(-N)=J
     ND_SUB(I,J)=ND_SUB(I,J)+1
  ENDDO
!
!debug check
!  N=SUM(ND_SUB)
!  IF( N/=ND+NFC ) stop 9908
!
!
! (2) I1_SUBを作成する
  N=1
  DO J=1,NY_SUB
  DO I=1,NX_SUB
     I1_SUB(I,J)=N
     N=N+ND_SUB(I,J)
  ENDDO
  ENDDO
!
!debug check
!  IF( N-1/=ND+NFC ) stop 9909
!
!
! (3) IPTR_SUBを作成する
  ND_SUB(:,:)=0
  DO N=1,ND
     I=I_SUB(N)
     J=J_SUB(N)
     M=I1_SUB(I,J)+ND_SUB(I,J)
     IPTR_SUB(M)=N
     ND_SUB(I,J)=ND_SUB(I,J)+1
  ENDDO
  DO N=1,NFC
     I=I_SUB(-N)
     J=J_SUB(-N)
     M=I1_SUB(I,J)+ND_SUB(I,J)
     IPTR_SUB(M)=-N
     ND_SUB(I,J)=ND_SUB(I,J)+1
  ENDDO
!
!debug check
!  N=SUM(ND_SUB)
!  IF( N/=ND+NFC ) stop 9909
!
  RETURN
900 CONTINUE
  WRITE(*,*) '### ERROR : ARRAY ALLOCATE ERROR : SUBGRID'
  IERROR=1
END SUBROUTINE ALLOCATE_SUBGRID
!
END MODULE M_SUBGRID
