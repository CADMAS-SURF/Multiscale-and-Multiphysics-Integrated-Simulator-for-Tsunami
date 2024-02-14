      MODULE MOD_WIND
C
      USE MOD_MAP
C
      IMPLICIT NONE
C
      CHARACTER(LEN=64),PARAMETER :: CFILE='wind.dat'
      INTEGER,PARAMETER :: LFILE=55
      INTEGER,PARAMETER :: LWFILE=56
C
      TYPE WIND_SYSTEM
        INTEGER :: MX
        INTEGER :: MY
C 20110907 for Hitachi FORTRAN
C        REAL(8),ALLOCATABLE :: WX_IN(:,:,:)
C        REAL(8),ALLOCATABLE :: WY_IN(:,:,:)
C 20110907 for Hitachi FORTRAN
        REAL(8),POINTER :: WX_IN(:,:,:)
        REAL(8),POINTER :: WY_IN(:,:,:)
      END TYPE
      TYPE(WIND_SYSTEM),ALLOCATABLE :: WIND_IN(:)
C
      INTEGER :: NFILE     ! 風データファイルの数
      INTEGER :: NXW,NYW   ! 風データの大きさ
      CHARACTER(LEN=256) :: CDIR  ! 風データがあるディレクトリ
      CHARACTER(LEN=64),ALLOCATABLE :: CFILE_WIND(:) ! 風データファイル名
      REAL(8),ALLOCATABLE :: DATE_FILE(:)  ! 各ファイルのJulius日
      REAL(8),ALLOCATABLE :: WINDX(:,:,:)
      REAL(8),ALLOCATABLE :: WINDY(:,:,:)
      INTEGER :: LON_START
      INTEGER :: LAT_START
      INTEGER :: IDLON
      INTEGER :: IDLAT
      INTEGER :: L0,L1     ! 現在の時刻は L0 と L1 を補間
      INTEGER :: IOLD,INEW
      INTEGER :: IOLD_SET,INEW_SET
C
      REAL(8) :: SDATE0    ! STOC の0時刻のJulius日
C
      REAL(8) :: TIME_FACT
C-----------------------------------------------------------------------
      CONTAINS
C-----------------------------------------------------------------------
      SUBROUTINE WIND_INIT(NGRID,IERR)
C
      INTEGER,INTENT(IN)  :: NGRID
      INTEGER,INTENT(OUT) :: IERR
C
      CHARACTER(LEN=10) :: CDATE0
      CHARACTER(LEN=80) :: CLINE
      INTEGER :: LON,LAT
      INTEGER :: IYEAR,IMONTH,IDAY,IHOUR
      INTEGER :: JDAY
      INTEGER :: I,J,N 
C
C
C
      NFILE = 0
C
C ファイルの読み込み
C
      OPEN(LFILE,FILE=TRIM(CFILE),FORM='FORMATTED',IOSTAT=IERR)
      IF(IERR.NE.0) RETURN
C
C [PASS 1]
C
      CALL MAP_INIT(LFILE,IERR)
      IF(IERR.NE.0) RETURN
C
      READ(LFILE,'(A10,A)',IOSTAT=IERR) CDATE0,CDIR
      IF(IERR.LT.0) THEN
        CLOSE(LFILE)
        IERR = 0
        RETURN
      ELSE IF(IERR.NE.0) THEN
        CALL ERRMSG('WIND_INIT',7100)
        WRITE(16,*) 'WIND_FILE ERROR'
        CALL ABORT1('')
      END IF
C
      READ(CDATE0,'(I4,3I2)') IYEAR,IMONTH,IDAY,IHOUR
      CALL JULIUS_DAY(JDAY,IYEAR,IMONTH,IDAY)
      SDATE0 = JDAY + IHOUR/24.D0
C
      L0 = -1
      L1 = -1
C
      DO
        READ(LFILE,'(A)',IOSTAT=IERR)
        IF(IERR.LT.0) EXIT
        NFILE = NFILE + 1
      END DO
C
      IF(NFILE.NE.0) THEN
        ALLOCATE(CFILE_WIND(NFILE))
        ALLOCATE(DATE_FILE(NFILE))
C
C [PASS 2]
C
        REWIND LFILE
        CALL MAP_INIT(LFILE,IERR)
        READ(LFILE,'(A10)') CDATE0
        DO N=1,NFILE
          READ(LFILE,'(A)') CFILE_WIND(N)
        END DO
      END IF
C
      CLOSE(LFILE)
C
C 風データファイルのチェック
C
      DO N=1,NFILE
        READ(CFILE_WIND(N)(1:10),'(I4,3I2)') IYEAR,IMONTH,IDAY,IHOUR
        CALL JULIUS_DAY(JDAY,IYEAR,IMONTH,IDAY)
        DATE_FILE(N) = JDAY + IHOUR/24.D0
        IF(N.GT.1) THEN
          IF(DATE_FILE(N).LE.DATE_FILE(N-1)) THEN
            CALL ERRMSG('WIND_INIT',7101)
            WRITE(16,*) 'INVALID ORDER OF WIND_FILES'
            CALL ABORT1('')
          END IF
        END IF
C
        OPEN(LFILE,FILE=TRIM(CDIR)//TRIM(CFILE_WIND(N))
     &            ,FORM='FORMATTED')
        IF(N.EQ.1) THEN
          READ(LFILE,'(A80)') CLINE
          READ(CLINE(2:),*) NXW,NYW
          READ(LFILE,*) LON_START,LAT_START
          BACKSPACE(LFILE)
          DO J=1,NYW
            DO I=1,NXW
              READ(LFILE,*) LON,LAT
              IF(I.EQ.2) IDLON = LON - LON_START
            END DO
            IF(J.EQ.2) THEN
              IDLAT = LAT - LAT_START
              EXIT
            END IF
          END DO
C        ELSE
C          READ(LFILE,*)
C          DO J=1,NYW
C            DO I=1,NXW
C              READ(LFILE,*)
C            END DO
C          END DO
        END IF
        CLOSE(LFILE)
C
      END DO
C
      ALLOCATE(WINDX(NXW,NYW,2),WINDY(NXW,NYW,2))
C
      ALLOCATE(WIND_IN(NGRID))
C
      IERR = 0
      RETURN
      END SUBROUTINE WIND_INIT
C-----------------------------------------------------------------------
      SUBROUTINE WIND_TIME(TIME)
C
C 風の時間方向の補間(格子は風データの格子のまま)
C
      REAL(8),INTENT(IN) :: TIME    ! 時刻[s]
      REAL(8) :: DATE_NOW
      INTEGER :: I,J
C
      IF(NFILE.EQ.0) RETURN
C
      DATE_NOW = TIME/86400.D0 + SDATE0
C
      IOLD_SET = 0
      INEW_SET = 0
C
C 新しい風データを読むかどうかのチェック
C
      IF(L0.EQ.-1) THEN
C
C 計算の初期はIOLDとINEWの２つの風データを設定
C
!!        IF(DATE_NOW.LT.DATE_FILE(1)) THEN
        IF(DATE_NOW.LE.DATE_FILE(1)) THEN
          L0 = 1
          L1 = 1
!!        ELSE IF(DATE_NOW.GT.DATE_FILE(NFILE)) THEN
        ELSE IF(DATE_NOW.GE.DATE_FILE(NFILE)) THEN
          L0 = NFILE
          L1 = NFILE
        ELSE
          DO L0=1,NFILE-1
            IF(DATE_NOW.GE.DATE_FILE(L0) .AND.
     &         DATE_NOW.LE.DATE_FILE(L0+1)) EXIT
          END DO
          L1 = L0+1
        END IF
        IOLD = 1
        INEW = 2
        IOLD_SET = 1
        INEW_SET = 1
        CALL WIND_SET(L0,IOLD)
        CALL WIND_SET(L1,INEW)
C
C すでにIOLD,INEWの２つの風データが設定されている
C
      ELSE
C
C まだ先の風データがある
C
 10     CONTINUE
        IF(L0.LT.NFILE) THEN
          IF(DATE_NOW.GT.DATE_FILE(L1)) THEN
            L0 = L0 + 1
            IF(L1.LT.NFILE) L1 = L1 + 1         ! データを一つ進める
            IOLD = 3 - IOLD
            INEW = 3 - INEW     ! (1,2)のSWAP
            INEW_SET = 1
            CALL WIND_SET(L1,INEW) ! NEWの風データを設定
            GO TO 10
          END IF
        END IF
C
      END IF
C
C 風データの時間方向の補間
C
      IF(L0.NE.L1) THEN
        TIME_FACT = (DATE_NOW      - DATE_FILE(L0))
     &             /(DATE_FILE(L1) - DATE_FILE(L0))
      ELSE
        TIME_FACT = 1.0D0
      END IF
C
      RETURN
      END SUBROUTINE WIND_TIME
C-----------------------------------------------------------------------
      SUBROUTINE WIND_SET(L,M)
C     
      INTEGER,INTENT(IN) :: L  ! ファイル番号
      INTEGER,INTENT(IN) :: M  ! 風データの格納領域
C
      INTEGER :: LON,LAT
      INTEGER :: I,J
C
      OPEN(LFILE,FILE=TRIM(CDIR)//TRIM(CFILE_WIND(L)),FORM='FORMATTED')
      WRITE(6,*) 'READ WIND FILE::',TRIM(CFILE_WIND(L))
      READ(LFILE,*)
      DO J=1,NYW
        DO I=1,NXW
          READ(LFILE,*) LON,LAT,WINDX(I,J,M),WINDY(I,J,M)
        END DO
      END DO
      CLOSE(LFILE)
C
      END SUBROUTINE
C-----------------------------------------------------------------------
      SUBROUTINE WIND_INTERP(WX,WY,XC,YC,MX,MY,L)
C
      INTEGER,INTENT(IN) :: MX,MY
      REAL(8),INTENT(IN) :: XC(8,MX,MY)
      REAL(8),INTENT(IN) :: YC(8,MY)
      REAL(8),INTENT(INOUT) :: WX(MX,MY)
      REAL(8),INTENT(INOUT) :: WY(MX,MY)
      INTEGER,INTENT(IN) :: L
      INTEGER :: I,J
C
      IF(NFILE.EQ.0) RETURN
C
C 20110907 for Hitachi FORTRAN
C      IF(.NOT.ALLOCATED(WIND_IN(L)%WX_IN)) THEN
      IF(.NOT.ASSOCIATED(WIND_IN(L)%WX_IN)) THEN
C 20110907 for Hitachi FORTRAN
        ALLOCATE(WIND_IN(L)%WX_IN(MX,MY,2))
        ALLOCATE(WIND_IN(L)%WY_IN(MX,MY,2))
      END IF
C
      IF(IOLD_SET.EQ.1) THEN
        CALL WIND_SPACE_INTERP(XC,YC,MX,MY,IOLD,L)
      END IF
      IF(INEW_SET.EQ.1) THEN
        CALL WIND_SPACE_INTERP(XC,YC,MX,MY,INEW,L)
      END IF
C
C 時間方向の補間
C
      DO J=1,MY
        DO I=1,MX
          WX(I,J) =          TIME_FACT *WIND_IN(L)%WX_IN(I,J,INEW)
     &            + (1.0D0 - TIME_FACT)*WIND_IN(L)%WX_IN(I,J,IOLD)
          WY(I,J) =          TIME_FACT *WIND_IN(L)%WY_IN(I,J,INEW)
     &            + (1.0D0 - TIME_FACT)*WIND_IN(L)%WY_IN(I,J,IOLD)
        END DO
      END DO
C      
      RETURN
C
      END SUBROUTINE WIND_INTERP
C-----------------------------------------------------------------------
      SUBROUTINE WIND_SPACE_INTERP(XC,YC,MX,MY,M,L)
C
      INTEGER,INTENT(IN) :: MX,MY
      REAL(8),INTENT(IN) :: XC(8,MX,MY)
      REAL(8),INTENT(IN) :: YC(8,MY)
      INTEGER,INTENT(IN) :: M
      INTEGER,INTENT(IN) :: L
C
      REAL(8) :: X,Y
      REAL(8) :: RLON,RLAT
      REAL(8) :: U,V
      REAL(8) :: FACTX,FACTY
      INTEGER :: I,J
      INTEGER :: II,JJ
C
      DO J=1,MY
        Y = YC(2,J) 
        DO I=1,MX
          X = XC(2,I,J)
          CALL MAP_XYTOLL(RLAT,RLON,X,Y)
          IF(RLON*1.D6.LT.MIN(LON_START,LON_START+IDLON*(NXW-1)) .OR.
     &       RLON*1.D6.GT.MAX(LON_START,LON_START+IDLON*(NXW-1)) .OR.
     &       RLAT*1.D6.LT.MIN(LAT_START,LAT_START+IDLAT*(NYW-1)) .OR.
     &       RLAT*1.D6.GT.MAX(LAT_START,LAT_START+IDLAT*(NYW-1))) THEN
            CALL ERRMSG('WIND_SPACE_INTERP',7102)
            WRITE(16,*) 'WIND DATA IS OUT OF BOUNDS'
            WRITE(16,*) 'I,J=',I,J
            WRITE(16,*) 'X,Y=',X,Y
            WRITE(16,*) 'LON,LAT=',RLON*1.D6,RLAT*1.D6
            CALL ABORT1('')
          END IF
          FACTX = (RLON*1.D6 - LON_START)/DBLE(IDLON)
          FACTY = (RLAT*1.D6 - LAT_START)/DBLE(IDLAT)
          II = MIN(INT(FACTX) + 1, NXW-1)
          JJ = MIN(INT(FACTY) + 1, NYW-1)   ! 整数部 + 1
          FACTX = FACTX - (II - 1)
          FACTY = FACTY - (JJ - 1)          ! 小数部
C         U =          FACTX *         FACTY *WINDX(II  ,JJ  ,M)
C    &      +          FACTX *(1.0D0 - FACTY)*WINDX(II  ,JJ+1,M)
C    &      + (1.0D0 - FACTX)*         FACTY *WINDX(II+1,JJ  ,M)
C    &      + (1.0D0 - FACTX)*(1.0D0 - FACTY)*WINDX(II+1,JJ+1,M)
C         V =          FACTX *         FACTY *WINDY(II  ,JJ  ,M)
C    &      +          FACTX *(1.0D0 - FACTY)*WINDY(II  ,JJ+1,M)
C    &      + (1.0D0 - FACTX)*         FACTY *WINDY(II+1,JJ  ,M)
C    &      + (1.0D0 - FACTX)*(1.0D0 - FACTY)*WINDY(II+1,JJ+1,M)
          U = (1.0D0 - FACTX)*(1.0D0 - FACTY)*WINDX(II  ,JJ  ,M)
     &      + (1.0D0 - FACTX)*         FACTY *WINDX(II  ,JJ+1,M)
     &      +          FACTX *(1.0D0 - FACTY)*WINDX(II+1,JJ  ,M)
     &      +          FACTX *         FACTY *WINDX(II+1,JJ+1,M)
          V = (1.0D0 - FACTX)*(1.0D0 - FACTY)*WINDY(II  ,JJ  ,M)
     &      + (1.0D0 - FACTX)*         FACTY *WINDY(II  ,JJ+1,M)
     &      +          FACTX *(1.0D0 - FACTY)*WINDY(II+1,JJ  ,M)
     &      +          FACTX *         FACTY *WINDY(II+1,JJ+1,M)
          WIND_IN(L)%WX_IN(I,J,M) = U
          WIND_IN(L)%WY_IN(I,J,M) = V
        END DO
      END DO
C
      END SUBROUTINE WIND_SPACE_INTERP
C-----------------------------------------------------------------------
      SUBROUTINE JULIUS_DAY(JDAY,YYYY,MM,DD)
C
      IMPLICIT NONE
C
      INTEGER,INTENT(OUT) :: JDAY
      INTEGER,INTENT(IN)  :: YYYY,MM,DD
      INTEGER :: MMMOD
!
      MMMOD = (MM - 14)/12
      JDAY = DD - 32075
     &  + 1461*(YYYY + 4800 + MMMOD)/4
     &  + 367*(MM - 2 - MMMOD*12)/12
     &  - 3*( (YYYY + 4900 + MMMOD)/100 )/4
     &  - 2415021  ! JDAY OF 1900/01/01
!
      RETURN
      END SUBROUTINE JULIUS_DAY
C-----------------------------------------------------------------------
      END
