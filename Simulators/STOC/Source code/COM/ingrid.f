      SUBROUTINE INGRID(IFLAG,IRTRN)
C======================================================================
C     格子データを読み込む
C     入力データの追加方法は README_INPUT を参照
C
C     IFLAG = 0 : 通常の入力処理
C     IFLAG= -1 : data.in読込み時にGRIDブロックの読込みのみを行う
C======================================================================
      use mod_comm,only: comm_mlicds_dm
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'GRID.h'
      INCLUDE 'DRIFT.h'
      INCLUDE 'mpif.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
      INCLUDE 'AUTODECOMP.h'
C
      INTEGER,INTENT(IN)::IFLAG
      INTEGER,INTENT(OUT)::IRTRN
C
      INTEGER::ITMP(3),ITAG,IREQ1,ISTAT(MPI_STATUS_SIZE)
C
      REAL(8)::RTMP(2)
C
      INTEGER::I,IE,IERR,IS,J,N,NDAT1
      INTEGER::IX,IY
      INTEGER:: I1,I2,J1,J2
C
C
C ... FOR AUTODECOMP
      IRTRN=0
      NDIVX=1        ! X方向の領域分割数
      NDIVY=1        ! Y方向の領域分割数
      IDIVX(:)=0     ! 部分領域のX方向の分割数
      JDIVY(:)=0     ! 部分領域のX方向の分割数
      IDIV1(:)=0     ! 部分領域のX方向の範囲(開始点)
      JDIV1(:)=0     ! 部分領域のX方向の範囲(終了点)
      IDIV2(:)=0     ! 部分領域のY方向の範囲(開始点)
      JDIV2(:)=0     ! 部分領域のY方向の範囲(終了点)
      XDIV1(:)=0.0D0    ! X座標の範囲(開始点)
      XDIV2(:)=0.0D0    ! X座標の範囲(終了点)
      YDIV1(:)=0.0D0    ! Y座標の範囲(開始点)
      YDIV2(:)=0.0D0    ! Y座標の範囲(終了点)
      ICRDC=0
      REGN(:)=0.0D0
C
      IX=0
      IY=0
      REGION(1)=0.0D0
      REGION(2)=0.0D0
      REGION(3)=0.0D0
      REGION(4)=0.0D0
C
      RTMP(:)=0.0D0
C
      DO 100 N=1,100000
         CALL GET1(IS,IE,IERR)
         IF( IERR.GT.0 ) GO TO 900
c         write(*,*) 'debug:ingrid:variable=',cline(is:ie)
C
         IF( CLINE(IS:IE) .EQ. '%END' ) THEN
            GO TO 200
C
         ELSE IF( CLINE(IS:IE) .EQ. 'X' ) THEN
            CALL MGETR(XGRID,MXM,NGRDSZ)
            IX=1
C
         ELSE IF( CLINE(IS:IE) .EQ. 'LONGITUDE' ) THEN
            CALL MGETR(XGRID,MXM,NGRDSZ)
            IX=2
C
         ELSE IF( CLINE(IS:IE) .EQ. 'Y' ) THEN
            CALL MGETR(YGRID,MYM,NGRDSZ)
            IY=1
C
         ELSE IF( CLINE(IS:IE) .EQ. 'LATITUDE' ) THEN
            CALL MGETR(YGRID,MYM,NGRDSZ)
            IY=2
C
         ELSE IF( CLINE(IS:IE) .EQ. 'Z' ) THEN
            CALL MGETR(ZGRID,MZM,NGRDSZ)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'ORIGIN' ) THEN
            CALL MGETR(RTMP,NDAT1,2)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'REGION' ) THEN
            CALL MGETR(REGION,NDAT1,4)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'HLIMIT' ) THEN
            CALL GETR(HLMT)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'I-DIV' ) THEN
            CALL MGETI(IDIVX,NDIVX,MAXPE)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'J-DIV' ) THEN
            CALL MGETI(JDIVY,NDIVY,MAXPE)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'CADMAS-XMIN' ) THEN
            CALL GETR(XCAD1)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'CADMAS-YMIN' ) THEN
            CALL GETR(YCAD1)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'CADMAS-XMAX' ) THEN
            CALL GETR(XCAD2)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'CADMAS-YMAX' ) THEN
            CALL GETR(YCAD2)
C
         ELSE
            CALL ERRMSG('INGRID',6570)
            WRITE(LP,*) 'UNKNOWN VARIABLE NAME: ',CLINE(IS:IE)
            CALL ABORT1('')
         END IF
  100 CONTINUE
  200 CONTINUE
C
      IF( IX.NE.IY.OR.IX*IY.EQ.0 ) GOTO 910
      ICORDTYPE=IX
C
C     座標値をシフト(XORG=RTMP(1),YORG=RTMP(2))
C
      XORG = RTMP(1)
      YORG = RTMP(2)
      DO 300 I=1,MXM
        XGRID(I) = XGRID(I)+XORG
  300 CONTINUE
      DO 400 J=1,MYM
        YGRID(J) = YGRID(J)+YORG
  400 CONTINUE
C
      IF( IFLAG.EQ.-1 ) THEN
         IRTRN=NDIVX*NDIVY
C
         IF( ICORDTYPE.EQ.2.AND.REGION(3)-REGION(1).GT.0.0D0 ) THEN
            ICRDC=1
            REGN(:)=REGION(:)
         ENDIF
C
         IF(IDIVX(1).GT.0)THEN
            I2=1
            DO I=1,NDIVX
               I1=I2
               I2=I1+IDIVX(I)
C
               IDIV1(I)=I1+1
               IDIV2(I)=I2
               XDIV1(I)=XGRID(I1)
               XDIV2(I)=XGRID(I2)
            ENDDO
C
            IF(I2.NE.MXM) THEN
               CALL ERRMSG('INGRID',6571)
               WRITE(LP,*) 'ERROR IN %GRID BLOCK'
               WRITE(LP,*) ' SUM OF I-DIV IS NOT EQUAL TO',
     $                     ' X-DIR. CELL NUMBER'
               CALL ABORT1('')
            ENDIF
         ELSE
            IDIV1(1)=2
            IDIV2(1)=MXM
            XDIV1(1)=XGRID(1)
            XDIV2(1)=XGRID(MXM)
         ENDIF
C
         IF(JDIVY(1).GT.0)THEN
            J2=1
            DO J=1,NDIVY
               J1=J2
               J2=J1+JDIVY(J)
C
               JDIV1(J)=J1+1
               JDIV2(J)=J2
               YDIV1(J)=YGRID(J1)
               YDIV2(J)=YGRID(J2)
            ENDDO
C
            IF(J2.NE.MYM) THEN
               CALL ERRMSG('INGRID',6572)
               WRITE(LP,*) 'ERROR IN %GRID BLOCK'
               WRITE(LP,*) ' SUM OF J-DIV IS NOT EQUAL TO',
     $                     ' Y-DIR. CELL NUMBER'
               CALL ABORT1('')
            ENDIF
         ELSE
            JDIV1(1)=2
            JDIV2(1)=MYM
            YDIV1(1)=YGRID(1)
            YDIV2(1)=YGRID(MYM)
         ENDIF
C
         RETURN
      ENDIF
C
      IF( IAUTOD.EQ.1 ) THEN
         I2 = MXM
         J2 = MXM
         MXM = MYIE - MYIS + 2
         MYM = MYJE - MYJS + 2
         I1 = MXM+1
         J1 = MYM+1
         DO I=1,MXM
            XGRID(I)=XGRID(I+MYIS-2)
         ENDDO
         DO J=1,MYM
            YGRID(J)=YGRID(J+MYJS-2)
         ENDDO
         XGRID(I1:I2)=0.0D0
         YGRID(J1:J2)=0.0D0
C
         IF( IPECON(3,NRANK+1).LT.0 ) THEN
            REGION(:)=0.0D0
         ENDIF
      ENDIF
C
C ... MX,MY,MZ,MXYZ,MXY,NXYZの値を設定
      MX   = MXM+1
      MY   = MYM+1
      MZ   = MZM+1
      MXYZ = MX*MY*MZ
      MXY  = MX*MY
      NXYZ = (MX-2)*(MY-2)*(MZ-2)
c      write(*,*) 'debug:ingrid:mx,my,mz=',mx,my,mz
C
C ... STOC-DMにメッシュ分割数を送信
      IF(NB_SD.GE.0) THEN
         ITMP(1)=MX-2
         ITMP(2)=MY-2
         ITMP(3)=MZ-2
         ITAG = 110
         CALL MPI_ISEND(ITMP,3,MPI_INTEGER,NB_SD,ITAG,
     $                  comm_mlicds_dm,IREQ1,IERR) !  NI の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
      ENDIF
C
      RETURN
C
C ... 読み込みエラー
  900 CONTINUE
      CALL ERRMSG('INGRID',6573)
      WRITE(LP,*) 'END OF FILE: INPUT DATA IS INCOMPLETE'
      CALL ABORT1('')
  910 CONTINUE
      CALL ERRMSG('INGRID',6574)
      WRITE(LP,*) 'ONE OF (X,Y) OR (LONGITUDE,LATITUDE) ',
     $   'MUST BE SPECIFIED'
      CALL ABORT1('')
      END
