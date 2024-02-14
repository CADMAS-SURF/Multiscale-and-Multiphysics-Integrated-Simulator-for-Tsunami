      MODULE MOD_MAP
C
      IMPLICIT NONE
C
      INTEGER :: JIKU         ! 座標系の番号
      REAL(8) :: GENTEN(2,19) ! 19座標系の原点[deg]
     &  = RESHAPE(SOURCE=(/ 33.D0 ,129.D0+30.D0/60.D0   ! I
     &                    , 33.D0 ,131.D0               ! II
     &                    , 36.D0 ,132.D0+10.D0/60.D0   ! III
     &                    , 33.D0 ,133.D0+30.D0/60.D0   ! IV
     &                    , 36.D0 ,134.D0+20.D0/60.D0   ! V
     &                    , 36.D0 ,136.D0               ! VI
     &                    , 36.D0 ,137.D0+10.D0/60.D0   ! VII
     &                    , 36.D0 ,138.D0+30.D0/60.D0   ! VIII
     &                    , 36.D0 ,139.D0+50.D0/60.D0   ! IX
     &                    , 40.D0 ,140.D0+50.D0/60.D0   ! X
     &                    , 44.D0 ,140.D0+15.D0/60.D0   ! XI
     &                    , 44.D0 ,142.D0+15.D0/60.D0   ! XII
     &                    , 44.D0 ,144.D0+15.D0/60.D0   ! XIII
     &                    , 26.D0 ,142.D0               ! XIV
     &                    , 26.D0 ,127.D0+30.D0/60.D0   ! XV
     &                    , 26.D0 ,124.D0               ! XVI
     &                    , 26.D0 ,131.D0               ! XVII
     &                    , 20.D0 ,136.D0               ! XVIII
     &                    , 26.D0 ,154.D0/)             ! XIX
     &  ,SHAPE=(/2,19/))
C
      REAL(8) :: X_BASE 
      REAL(8) :: Y_BASE       ! STOCの原点の19座標系における位置[m]

      REAL(8) :: XNS          ! 南北距離[m](北が正)
      REAL(8) :: YEW          ! 東西距離[m](東が正)
C
C-----------------------------------------------------------------------
      CONTAINS
C-----------------------------------------------------------------------
      SUBROUTINE MAP_INIT(LFILE,IERR)
C
      INTEGER,INTENT(IN) :: LFILE
      INTEGER,INTENT(OUT) :: IERR
C
      INTEGER,PARAMETER :: LP  = 16
C
C
C ファイルの読み込み
C
      READ(LFILE,*,IOSTAT=IERR) JIKU,X_BASE,Y_BASE 
C
      RETURN
C
 900  CONTINUE
      CALL ERRMSG('MAP_INIT',7090)
      WRITE(LP,*) 'ERROR IN MAP_INIT'
      CALL ABORT1('')
C
      END SUBROUTINE MAP_INIT
C-----------------------------------------------------------------------
      SUBROUTINE MAP_LLTOXY(RLAT,RLON,XCOORD,YCOORD)
      REAL(8),INTENT(IN)  :: RLAT,RLON     ! 緯度・経度[deg]
      REAL(8),INTENT(OUT) :: XCOORD,YCOORD ! STOCの座標[m]
      CALL MAP_BLTOXY(RLAT,RLON,XNS,YEW)
      XCOORD = YEW - X_BASE
      YCOORD = XNS - Y_BASE
      RETURN
      END SUBROUTINE MAP_LLTOXY
C-----------------------------------------------------------------------
      SUBROUTINE MAP_XYTOLL(RLAT,RLON,XCOORD,YCOORD)
      REAL(8),INTENT(OUT) :: RLAT,RLON     ! 緯度・経度[deg]
      REAL(8),INTENT(IN)  :: XCOORD,YCOORD ! STOCの座標[m]
      XNS = YCOORD + Y_BASE
      YEW = XCOORD + X_BASE
      CALL MAP_XYTOBL(RLAT,RLON,XNS,YEW)
      RETURN
      END SUBROUTINE MAP_XYTOLL
C-----------------------------------------------------------------------
      SUBROUTINE MAP_BLTOXY(RLAT,RLON,XCOORD,YCOORD)
C
      REAL(8),INTENT(IN)  :: RLAT,RLON     ! 緯度・経度[deg]
      REAL(8),INTENT(OUT) :: XCOORD,YCOORD ! 19座標系の座標[m]
C
      REAL(8) :: RAD                       ! [deg]-->[rad]の変換係数
      REAL(8) :: SINLAT,COSLAT
      REAL(8) :: RLAT0
      REAL(8) :: DLAT,DLON
      REAL(8) :: DC,DC2,DC4,DS
      REAL(8) :: TAU,TAU2,TAU4
      REAL(8) :: ETA,ETA2,ETA4
      REAL(8) :: N
C
      REAL(8) :: FUNC0
C
C [PARAMETER]
      REAL(8),PARAMETER :: XA  = 1.0050373060D0
      REAL(8),PARAMETER :: XB  = 0.0050478492D0
      REAL(8),PARAMETER :: XC  = 0.0000105638D0
      REAL(8),PARAMETER :: XD  = 0.0000000206D0
      REAL(8),PARAMETER :: RA  = 6377.39715D0
      REAL(8),PARAMETER :: RB  = 6356.07896D0
      REAL(8),PARAMETER :: EA2 = 0.00667437155D0 
      REAL(8),PARAMETER :: EB2 = 0.00671921811D0
      REAL(8),PARAMETER :: EA  = 0.08169682D0
      REAL(8),PARAMETER :: EB  = 0.08197083D0
      REAL(8),PARAMETER :: MO  = 0.9999D0
C-----------------------------------------------------------------------
C
      RAD = ATAN(1.0D0)/45.0D0
C
      COSLAT = COS(RLAT*RAD)
      SINLAT = SIN(RLAT*RAD)
C
      RLAT0 = GENTEN(1,JIKU)*RAD
      DLAT = (RLAT - GENTEN(1,JIKU))*RAD
      DLON = (RLON - GENTEN(2,JIKU))*RAD
C
      DC  = DLON*COSLAT
      DC2 = DC**2
      DC4 = DC2**2
      DS  = DLON*SINLAT
C
      TAU    = SINLAT/COSLAT
      TAU2   = TAU**2
      TAU4   = TAU2**2
C
      ETA  = EB*COSLAT
      ETA2 = ETA**2
      ETA4 = ETA2**2
C
      N    = RA/SQRT(1.0D0-EA2*SINLAT**2)
C
      FUNC0 = RA*(1.0D0-EA2)
     &          *( XA*DLAT
     &            -XB*SIN(      DLAT)*COS(2.0D0*RLAT0)
     &            +XC*SIN(2.0D0*DLAT)*COS(4.0D0*RLAT0)/2.0D0
     &            -XD*SIN(3.0D0*DLAT)*COS(6.0D0*RLAT0)/3.0D0 )
C
      XCOORD = FUNC0 + DC*DS*N*(
     &         0.5D0
     &       + DC2*( 5.0D0-TAU2+9.0D0*ETA2+4.0D0*ETA4)/24.0D0
     &       + DC4*(61.0D0-58.0D0*TAU2+TAU4)/720.0D0
     &                          )
      YCOORD = N*DC*(1.0D0 + DC2*(1.0D0-       TAU2+ETA2)/6.0D0
     &                     + DC4*(5.0D0-18.0D0*TAU2+TAU4)/120.0D0)
C
C [km]-->[m]
      XCOORD = XCOORD*MO*1000.D0
      YCOORD = YCOORD*MO*1000.D0
C
      RETURN
      END SUBROUTINE MAP_BLTOXY
C-----------------------------------------------------------------------
      SUBROUTINE MAP_XYTOBL(RLAT,RLON,XCOORD,YCOORD)
C
      REAL(8),INTENT(OUT) :: RLAT,RLON     ! 緯度・経度[deg]
      REAL(8),INTENT(IN)  :: XCOORD,YCOORD ! 19座標系の座標[m]
C
      REAL(8) :: X00,Y00
      REAL(8) :: XP0,YP0,XM0,YM0
      REAL(8) :: X0P,Y0P,X0M,Y0M
      REAL(8) :: A11,A12,A21,A22
      REAL(8) :: RDET
      REAL(8) :: DLAT,DLON
C
C [PARAMETER]
      REAL(8),PARAMETER :: EPS = 0.5D-4
      REAL(8),PARAMETER :: REPS = 1.0D0/(2.0D0*EPS)
C-----------------------------------------------------------------------
      RLAT = GENTEN(1,JIKU)
      RLON = GENTEN(2,JIKU) ! Newton法の初期値は19座標の原点
C
      DO
        CALL MAP_BLTOXY(RLAT    ,RLON    ,X00,Y00)
        CALL MAP_BLTOXY(RLAT+EPS,RLON    ,XP0,YP0)
        CALL MAP_BLTOXY(RLAT-EPS,RLON    ,XM0,YM0)
        CALL MAP_BLTOXY(RLAT    ,RLON+EPS,X0P,Y0P)
        CALL MAP_BLTOXY(RLAT    ,RLON-EPS,X0M,Y0M)
        A11 = (XP0 - XM0)*REPS
        A12 = (X0P - X0M)*REPS
        A21 = (YP0 - YM0)*REPS
        A22 = (Y0P - Y0M)*REPS
        RDET = 1.0D0/(A11*A22 - A12*A21)
        DLAT = ((X00-XCOORD)*A22 - A12*(Y00-YCOORD))*RDET
        DLON = (A11*(Y00-YCOORD) - (X00-XCOORD)*A21)*RDET
        RLAT = RLAT - DLAT
        RLON = RLON - DLON
        IF(DLAT**2+DLON**2.LE.EPS**2) EXIT
      END DO
C
      RETURN
      END SUBROUTINE MAP_XYTOBL
C-----------------------------------------------------------------------
      END

