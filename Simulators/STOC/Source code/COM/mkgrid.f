      SUBROUTINE MKGRID(XC,XCP,YC,ZC,YCOS,YCOSP,YSIN,YSINP,
     $                  XC_REF,YC_REF,ZCA,IXS,IXE,JYS,JYE)
C======================================================================
C     格子定数を設定する
C======================================================================
      use mod_comm,only: comm_model
      IMPLICIT NONE
C
      INCLUDE 'GRID.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
C
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
      INCLUDE 'mpif.h'
C
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),XCP(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(INOUT)::YCOS(MY),YCOSP(MY),YSIN(MY),YSINP(MY)
      REAL(8),INTENT(OUT)::XC_REF(8,MX),YC_REF(8,MY)
      REAL(8),INTENT(OUT)::ZCA(8,MZA)
      REAL(8),PARAMETER::PAI = 3.141592653897932D0
      INTEGER,INTENT(OUT)::IXS,IXE,JYS,JYE
C
      REAL(8)::YYY
      INTEGER::I,J,K
C
      REAL(8),PARAMETER::EPS=1.0D-3 ! 経緯度で3.6秒程度
      REAL(8)::RBUF(11),RBUF2(11,MAXPE),X0,Y0,X1,Y1,DX,DY
      REAL(8)::LON0,LAT0,LON1,LAT1,RTMP,RTMP2
      INTEGER::ICHILD,IPARNT,IREQ,IERR,ITYPC,N,NCH,I0,J0,I1,J1,IX,JX
      INTEGER::ISTAT(MPI_STATUS_SIZE)
CDEBUG      character(20)::filename
C
C----------------------------------------------------------------------
C ... 球面座標の場合のX,Y座標基準値の設定(最内側で基準点を決めて、その座標を親に向かって引き渡していく)
C     ITYPC = 0 最内側が球面：最内側のXCEN,YCENを基準にする
C     ITYPC = 1 最内側が直交：左下(X0,Y0)と右上(X1,Y1)の座標値を基準にする
C----------------------------------------------------------------------
      IPARNT = IPECON(2,NRANK+1)
      ICHILD = IPECON(3,NRANK+1)
      ITYPC  = 0
      IF( ICORDTYPE.EQ.1 ) ITYPC=1
C
      IF( ICHILD.GE.0 ) THEN ! 子供あり 子供から基準位置を受け取る
         CALL MPI_IRECV(RBUF,9,MPI_DOUBLE_PRECISION,ICHILD
     $                 ,MPI_ANY_TAG,comm_model,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
C
         IF( RBUF(1).EQ.1.0D0 ) ITYPC=1
C
         IF( ICORDTYPE.EQ.2 ) THEN ! 球面への接続のみ処理必要
            IF( ITYPC.EQ.1 ) THEN
               X0=RBUF(2)
               Y0=RBUF(3)
               X1=RBUF(4)
               Y1=RBUF(5)
               LON0=RBUF(6)
               LAT0=RBUF(7)
               LON1=RBUF(8)
               LAT1=RBUF(9)
C
               IF( REGION(3)-REGION(1).GT.0.0D0 .AND.
     $             REGION(4)-REGION(2).GT.0.0D0 ) THEN ! 直交と接続
                  I0=0
                  J0=0
                  I1=0
                  J1=0
                  DO I=1,MXM
                     IF( ABS(XGRID(I)-REGION(1)).LT.EPS ) I0=I
                     IF( ABS(XGRID(I)-REGION(3)).LT.EPS ) I1=I
                  ENDDO
                  DO J=1,MXM
                     IF( ABS(YGRID(J)-REGION(2)).LT.EPS ) J0=J
                     IF( ABS(YGRID(J)-REGION(4)).LT.EPS ) J1=J
                  ENDDO
                  IF(I0*I1*J0*J1.EQ.0) THEN
                     WRITE(*,*) 'GRID DATA AND REGION DATA IS NOT MATCH'
                     WRITE(*,*) '   NRANK =',NRANK
                     WRITE(*,*) '   REGION(1)=',REGION(1)
                     WRITE(*,*) '   REGION(2)=',REGION(2)
                     WRITE(*,*) '   REGION(3)=',REGION(3)
                     WRITE(*,*) '   REGION(4)=',REGION(4)
                  ENDIF
C
                  DX=(X1-X0)/(I1-I0)
                  DY=(Y1-Y0)/(J1-J0)
C
                  X0=X0+DX*(1-I0)
                  Y0=Y0+DY*(1-J0)
                  X1=X1+DX*(MXM-I1)
                  Y1=Y1+DY*(MYM-J1)
C
                  IXS=I0+1
                  IXE=I1
                  JYS=J0+1
                  JYE=J1
               ELSE ! 球面と接続
                  DX=(X1-X0)/(LON1-LON0) ! 1度あたりの距離
                  DY=(Y1-Y0)/(LAT1-LAT0)
C
                  X0=X0+(XGRID(1)-LON0)*DX
                  Y0=Y0+(YGRID(1)-LAT0)*DY
                  X1=X1+(XGRID(MXM)-LON1)*DX
                  Y1=Y1+(YGRID(MYM)-LAT1)*DY
               ENDIF
            ELSE
               XCEN=RBUF(2)
               YCEN=RBUF(3)
            ENDIF
         ELSE                      ! 直交の場合、子供は考慮しない
            X0=XGRID(1)
            Y0=YGRID(1)
            X1=XGRID(MXM)
            Y1=YGRID(MYM)
         ENDIF
C
      ELSE ! 子供なし
         IF( ICORDTYPE.EQ.1 ) THEN ! 直交の場合
            X0=XGRID(1)
            Y0=YGRID(1)
            X1=XGRID(MXM)
            Y1=YGRID(MYM)
         ELSE                      ! 球面の場合、中央値を基準にする
            XCEN=0.5D0*(XGRID(1)+XGRID(MXM))
            YCEN=0.5D0*(YGRID(1)+YGRID(MYM))
         ENDIF
      ENDIF
C
      IF( NPROC.GT.1 ) THEN ! 領域分割している兄弟あり
C                           ! 優先順位 1.子供のある番号の大きいプロセッサ
         RBUF(1)=DBLE(ICHILD)
         RBUF(2)=DBLE(NRANK)
         IF( ITYPC.EQ.1 ) THEN
            RBUF(3)=1.0D0
            RBUF(4)=X0
            RBUF(5)=Y0
            RBUF(6)=X1
            RBUF(7)=Y1
            RBUF(8)=XGRID(1)
            RBUF(9)=YGRID(1)
            RBUF(10)=XGRID(MXM)
            RBUF(11)=YGRID(MYM)
         ELSE
            RBUF(3)=0.0D0
            RBUF(4)=XCEN
            RBUF(5)=YCEN
            RBUF(6)=0.0D0
            RBUF(7)=0.0D0
            RBUF(8)=XGRID(1)
            RBUF(9)=YGRID(1)
            RBUF(10)=XGRID(MXM)
            RBUF(11)=YGRID(MYM)
         ENDIF
C
         CALL MPI_ALLGATHER(RBUF,11,MPI_DOUBLE_PRECISION,
     $                   RBUF2,11,MPI_DOUBLE_PRECISION,
     $                   CHILDCOMM,IERR)
C
         RTMP=1.0D10
         RTMP2=-1.0D10
         NCH=0
         DO N=1,NPROC
            RTMP=MIN(RTMP,RBUF2(2,N))
            IF( RBUF2(1,N).GT.0.0D0.AND.RTMP2.LT.RBUF2(1,N) ) THEN
               RTMP2=RBUF2(1,N)
               NCH=N
            ENDIF
         ENDDO
         IF( DBLE(NRANK).NE.RTMP ) IPARNT=-1 ! 最小ランク以外は親フラグをクリアする
C
         IF( ICORDTYPE.EQ.1 ) THEN ! 直交の場合
            X0=RBUF2(4,1)
            Y0=RBUF2(5,1)
            X1=RBUF2(6,1)
            Y1=RBUF2(7,1)
            DO N=2,NPROC
               X0=MIN(X0,RBUF2(4,N))
               Y0=MIN(Y0,RBUF2(5,N))
               X1=MAX(X1,RBUF2(6,N))
               Y1=MAX(Y1,RBUF2(7,N))
            ENDDO
         ELSE                      ! 球面の場合
            IF( NCH.GT.0 ) THEN    ! 子がいるとき
               IF( RBUF2(3,NCH).EQ.1.0D0 ) THEN
                  ITYPC=1
                  X0=RBUF2(4,NCH)
                  Y0=RBUF2(5,NCH)
                  X1=RBUF2(6,NCH)
                  Y1=RBUF2(7,NCH)
                  LON0=RBUF2(8,NCH)
                  LAT0=RBUF2(9,NCH)
                  LON1=RBUF2(10,NCH)
                  LAT1=RBUF2(11,NCH)
               ELSE
                  XCEN=RBUF2(4,NCH)
                  YCEN=RBUF2(5,NCH)
               ENDIF
            ELSE ! 子がいないとき(平均)
               XCEN=0.0D0
               YCEN=0.0D0
               DO N=1,NPROC
                  IF( RBUF2(3,N).EQ.1.0D0 ) THEN
                     ITYPC=1
                     X0=RBUF2(4,N)
                     Y0=RBUF2(5,N)
                     X1=RBUF2(6,N)
                     Y1=RBUF2(7,N)
                     LON0=RBUF2(8,N)
                     LAT0=RBUF2(9,N)
                     LON1=RBUF2(10,N)
                     LAT1=RBUF2(11,N)
                  ELSE
                     XCEN=XCEN+RBUF2(4,N)
                     YCEN=YCEN+RBUF2(5,N)
                  ENDIF
               ENDDO
            ENDIF
C
            IF( ITYPC.EQ.1 ) THEN  ! 子供に直交がある場合
               DX=(X1-X0)/(LON1-LON0) ! 1度あたりの距離
               DY=(Y1-Y0)/(LAT1-LAT0)
C
               X0=X0+(XGRID(1)-LON0)*DX
               Y0=Y0+(YGRID(1)-LAT0)*DY
               X1=X1+(XGRID(MXM)-LON1)*DX
               Y1=Y1+(YGRID(MYM)-LAT1)*DY
            ELSE IF( NCH.EQ.0 ) THEN
               XCEN=XCEN/DBLE(NPROC)
               YCEN=YCEN/DBLE(NPROC)
            ENDIF
         ENDIF
      ENDIF
C
      IF( IPARNT.GE.0 ) THEN ! 親あり
         IF( ITYPC.EQ.1 ) THEN
            RBUF(1)=1.0D0
            RBUF(2)=X0
            RBUF(3)=Y0
            RBUF(4)=X1
            RBUF(5)=Y1
            RBUF(6)=XGRID(1)
            RBUF(7)=YGRID(1)
            RBUF(8)=XGRID(MXM)
            RBUF(9)=YGRID(MYM)
         ELSE
            RBUF(1)=0.0D0
            RBUF(2)=XCEN
            RBUF(3)=YCEN
            RBUF(4)=0.0D0
            RBUF(5)=0.0D0
            RBUF(6)=0.0D0
            RBUF(7)=0.0D0
            RBUF(8)=0.0D0
            RBUF(9)=0.0D0
         ENDIF
C
         CALL MPI_ISEND(RBUF,9,MPI_DOUBLE_PRECISION,IPARNT
     $                  ,IPARNT,comm_model,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR) 
      ENDIF
C
C----------------------------------------------------------------------
C     以下、格子定数の設定
C----------------------------------------------------------------------
      DO I=1,MXM
         IF( ICORDTYPE.EQ.1 ) THEN
            XC_REF(1,I)=XGRID(I)
         ELSE
            IF( ITYPC.EQ.0 ) THEN
               XC_REF(1,I) = REARTH*(XGRID(I)-XCEN)/180.0D0*PAI
     $            *COS(YCEN/180.0D0*PAI)
            ELSE
               DX=(X1-X0)/(XGRID(MXM)-XGRID(1))
               XC_REF(1,I) = X0+DX*(XGRID(I)-XGRID(1))
            ENDIF
         ENDIF
      ENDDO
      DO J=1,MYM
         IF( ICORDTYPE.EQ.1 ) THEN
            YC_REF(1,J)=YGRID(J)
         ELSE
            IF( ITYPC.EQ.0 ) THEN
               YC_REF(1,J) = REARTH*(YGRID(J)-YCEN)/180.0D0*PAI
            ELSE
               DY=(Y1-Y0)/(YGRID(MYM)-YGRID(1))
               YC_REF(1,J) = Y0+DY*(YGRID(J)-YGRID(1))
            ENDIF
         ENDIF
      ENDDO
C
C
C ... [XYZ]C(1,*) 格子点座標の設定(入力データから読み込み済み)
      DO 10 J=1,MY
      DO 10 I=1,MXM
         IF( ICORDTYPE.EQ.1 ) THEN
            XC(1,I,J) = XGRID(I)
         ELSE
            XC(1,I,J) = REARTH*(XGRID(I)-XCEN)/180.0D0*PAI
         ENDIF
   10 CONTINUE
      DO 20 J=1,MYM
         IF( ICORDTYPE.EQ.1 ) THEN
            YC(1,J) = YGRID(J)
         ELSE
            YC(1,J) = REARTH*(YGRID(J)-YCEN)/180.0D0*PAI
         ENDIF
   20 CONTINUE
      DO 30 K=1,MZM
         ZC(1,K) = ZGRID(K)
   30 CONTINUE
C
C     次の3点は、仮想セルの外側のため計算では参照しないが、
C     等間隔を仮定して値だけは設定しておく。
      DO J=1,MY
         XC(1,MX,J) = XC(1,MXM,J) + ( XC(1,MXM,J) - XC(1,MXM-1,J) )
      ENDDO
      YC(1,MY) = YC(1,MYM) + ( YC(1,MYM) - YC(1,MYM-1) )
      ZC(1,MZ) = ZC(1,MZM) + ( ZC(1,MZM) - ZC(1,MZM-1) )
C
      XC_REF(1,MX) = XC_REF(1,MXM) + ( XC_REF(1,MXM) - XC_REF(1,MXM-1) )
      YC_REF(1,MY) = YC_REF(1,MYM) + ( YC_REF(1,MYM) - YC_REF(1,MYM-1) )
C
      DO J=1,MY
      DO I=1,MX
         XCP(1,I,J)=XC(1,I,J)
      ENDDO
      ENDDO
C
      IF( ICORDTYPE.EQ.1 ) THEN
         YCOS=1.0D0
         YCOSP=1.0D0
         YSIN=0.5D0*CORI/max(CEARTH,1.0d-20)     ! CORI=2.0D0*CEARTH*SIN
         YSINP=0.5D0*CORI/max(CEARTH,1.0d-20)
      ELSE
         DO J=1,MYM
            YCOSP(J) = COS(YGRID(J)/180.0D0*PAI)
            YSINP(J) = SIN(YGRID(J)/180.0D0*PAI)
         ENDDO
         DO J=2,MYM
            YCOS(J) = COS(0.5D0*(YGRID(J-1)+YGRID(J))/180.0D0*PAI)
            YSIN(J) = SIN(0.5D0*(YGRID(J-1)+YGRID(J))/180.0D0*PAI)
         ENDDO
C
         YYY=YGRID(MYM)+(YGRID(MYM)-YGRID(MYM-1))
         YCOSP(MY)=COS(YYY/180.0D0*PAI)
         YSINP(MY)=SIN(YYY/180.0D0*PAI)
C
         YYY=YGRID(1)-0.5D0*(YGRID(2)-YGRID(1))
         YCOS(1)=COS(YYY/180.0D0*PAI)
         YSIN(1)=SIN(YYY/180.0D0*PAI)
C
         YYY=YGRID(MYM)+0.5D0*(YGRID(MYM)-YGRID(MYM-1))
         YCOS(MY)=COS(YYY/180.0D0*PAI)
         YSIN(MY)=SIN(YYY/180.0D0*PAI)
C
         DO J=1,MY
         DO I=1,MX
            XC(1,I,J)  = XC(1,I,J)*YCOS(J)
            XCP(1,I,J) = XCP(1,I,J)*YCOSP(J)
         ENDDO
         ENDDO
      ENDIF
C
C
C ... [XYZ]C(2,*) セル中心座標の設定
      DO 100 J=1,MY
      DO 100 I=2,MX
         XC(2,I,J) = 0.5D0*( XC(1,I-1,J) + XC(1,I,J)  )
         XCP(2,I,J) = 0.5D0*( XCP(1,I-1,J) + XCP(1,I,J)  )
         IF(J.EQ.1)
     $   XC_REF(2,I) = 0.5D0*( XC_REF(1,I-1) + XC_REF(1,I)  )
  100 CONTINUE
      DO 110 J=2,MY
         YC(2,J) = 0.5D0*( YC(1,J-1) + YC(1,J)  )
         YC_REF(2,J) = 0.5D0*( YC_REF(1,J-1) + YC_REF(1,J)  )
  110 CONTINUE
      DO 120 K=2,MZ
         ZC(2,K) = 0.5D0*( ZC(1,K-1) + ZC(1,K)  )
  120 CONTINUE
      DO J=1,MY
         XC(2,1,J)  = XC(1,1,J)  - 0.5D0*( XC(1,2,J)  - XC(1,1,J)  )
         XCP(2,1,J) = XCP(1,1,J) - 0.5D0*( XCP(1,2,J) - XCP(1,1,J)  )
      ENDDO
      YC(2,1) = YC(1,1) - 0.5D0*( YC(1,2) - YC(1,1) )
      ZC(2,1) = ZC(1,1) - 0.5D0*( ZC(1,2) - ZC(1,1) )
C
      XC_REF(2,1) = XC_REF(1,1) - 0.5D0*( XC_REF(1,2) - XC_REF(1,1)  )
      YC_REF(2,1) = YC_REF(1,1) - 0.5D0*( YC_REF(1,2) - YC_REF(1,1) )
C
C
C ... [XYZ]C(3,*) セル中心間隔の設定
      DO 200 J=1,MY
      DO 200 I=1,MXM
         XC(3,I,J)  = XC(2,I+1,J)  - XC(2,I,J)
         XCP(3,I,J) = XCP(2,I+1,J) - XCP(2,I,J)
         IF(J.EQ.1)
     $   XC_REF(3,I)  = XC_REF(2,I+1) - XC_REF(2,I)
  200 CONTINUE
      DO 210 J=1,MYM
         YC(3,J) = YC(2,J+1) - YC(2,J)
         YC_REF(3,J) = YC_REF(2,J+1) - YC_REF(2,J)
  210 CONTINUE
      DO 220 K=1,MZM
         ZC(3,K) = ZC(2,K+1) - ZC(2,K)
  220 CONTINUE
      DO J=1,MY
         XC(3,MX,J)  = XC(3,MXM,J)
         XCP(3,MX,J) = XCP(3,MXM,J)
      ENDDO
      YC(3,MY) = YC(3,MYM)
      ZC(3,MZ) = ZC(3,MZM)
C
      XC_REF(3,MX) = XC_REF(3,MXM)
      YC_REF(3,MY) = YC_REF(3,MYM)
C
C
C ... [XYZ]C(4,*) 格子点間隔の設定
      DO 300 J=1,MY
      DO 300 I=2,MX
         XC(4,I,J)  = XC(1,I,J)  - XC(1,I-1,J)
         XCP(4,I,J) = XCP(1,I,J) - XCP(1,I-1,J)
         IF(J.EQ.1)
     $   XC_REF(4,I) = XC_REF(1,I) - XC_REF(1,I-1)
  300 CONTINUE
      DO 310 J=2,MY
         YC(4,J) = YC(1,J) - YC(1,J-1)
         YC_REF(4,J) = YC_REF(1,J) - YC_REF(1,J-1)
  310 CONTINUE
      DO 320 K=2,MZ
         ZC(4,K) = ZC(1,K) - ZC(1,K-1)
  320 CONTINUE
      DO J=1,MY
         XC(4,1,J)  = XC(4,2,J)
         XCP(4,1,J) = XCP(4,2,J)
      ENDDO
      YC(4,1) = YC(4,2)
      ZC(4,1) = ZC(4,2)
C
      XC_REF(4,1) = XC_REF(4,2)
      YC_REF(4,1) = YC_REF(4,2)
C
C
C ... [XYZ]C(5,*) セル中心間隔の逆数の設定
C ... [XYZ]C(6,*) 格子点間隔の逆数の設定
      DO 400 J=1,MY
      DO 400 I=1,MX
         XC(5,I,J) = 1.0D0/XC(3,I,J)
         XC(6,I,J) = 1.0D0/XC(4,I,J)
         XCP(5,I,J) = 1.0D0/XCP(3,I,J)
         XCP(6,I,J) = 1.0D0/XCP(4,I,J)
         IF(J.EQ.1)THEN
            XC_REF(5,I) = 1.0D0/XC_REF(3,I)
            XC_REF(6,I) = 1.0D0/XC_REF(4,I)
         ENDIF
  400 CONTINUE
      DO 410 J=1,MY
         YC(5,J) = 1.0D0/YC(3,J)
         YC(6,J) = 1.0D0/YC(4,J)
         YC_REF(5,J) = 1.0D0/YC_REF(3,J)
         YC_REF(6,J) = 1.0D0/YC_REF(4,J)
  410 CONTINUE
      DO 420 K=1,MZ
         ZC(5,K) = 1.0D0/ZC(3,K)
         ZC(6,K) = 1.0D0/ZC(4,K)
  420 CONTINUE
C
C
C ... [XYZ]C(7,*) 線形補間時のI側係数
C ... [XYZ]C(8,*) 線形補間時のI+1側係数
      DO 500 J=1,MY
      DO 500 I=1,MXM
         XC(7,I,J) = XC(4,I+1,J) / ( XC(4,I,J)+XC(4,I+1,J) )
         XC(8,I,J) = XC(4,I  ,J) / ( XC(4,I,J)+XC(4,I+1,J) )
         XCP(7,I,J) = XCP(4,I+1,J) / ( XCP(4,I,J)+XCP(4,I+1,J) )
         XCP(8,I,J) = XCP(4,I  ,J) / ( XCP(4,I,J)+XCP(4,I+1,J) )
         IF(J.EQ.1)THEN
            XC_REF(7,I) = XC_REF(4,I+1) / ( XC_REF(4,I)+XC_REF(4,I+1) )
            XC_REF(8,I) = XC_REF(4,I  ) / ( XC_REF(4,I)+XC_REF(4,I+1) )
         ENDIF
  500 CONTINUE
      DO 510 J=1,MYM
         YC(7,J) = YC(4,J+1) / ( YC(4,J)+YC(4,J+1) )
         YC(8,J) = YC(4,J  ) / ( YC(4,J)+YC(4,J+1) )
         YC_REF(7,J) = YC_REF(4,J+1) / ( YC_REF(4,J)+YC_REF(4,J+1) )
         YC_REF(8,J) = YC_REF(4,J  ) / ( YC_REF(4,J)+YC_REF(4,J+1) )
  510 CONTINUE
      DO 520 K=1,MZM
         ZC(7,K) = ZC(4,K+1) / ( ZC(4,K)+ZC(4,K+1) )
         ZC(8,K) = ZC(4,K  ) / ( ZC(4,K)+ZC(4,K+1) )
  520 CONTINUE
      DO J=1,MY
         XC(7,MX,J) = 0.5D0
         XC(8,MX,J) = 0.5D0
         XCP(7,MX,J) = 0.5D0
         XCP(8,MX,J) = 0.5D0
      ENDDO
      YC(7,MY) = 0.5D0
      YC(8,MY) = 0.5D0
      ZC(7,MZ) = 0.5D0
      ZC(8,MZ) = 0.5D0
C
      XC_REF(7,MX) = 0.5D0
      XC_REF(8,MX) = 0.5D0
      YC_REF(7,MY) = 0.5D0
      YC_REF(8,MY) = 0.5D0
C
      IF(LAIR.EQ.1) THEN
C (1,*)
         DO 40 K=1,MZMA
            ZCA(1,K) = ZGRIDA(K)
   40    CONTINUE
         ZCA(1,MZA) = ZCA(1,MZMA) + ( ZCA(1,MZMA) - ZCA(1,MZMA-1) )
C
C (2,*)
         DO 130 K=2,MZA
            ZCA(2,K) = 0.5D0*( ZCA(1,K-1) + ZCA(1,K)  )
  130    CONTINUE
         ZCA(2,1) = ZCA(1,1) - 0.5D0*( ZCA(1,2) - ZCA(1,1) )
C
C (3,*)
         DO 230 K=1,MZMA
            ZCA(3,K) = ZCA(2,K+1) - ZCA(2,K)
  230    CONTINUE
         ZCA(3,MZA) = ZCA(3,MZMA)
C
C (4,*)
         DO 330 K=2,MZA
            ZCA(4,K) = ZCA(1,K) - ZCA(1,K-1)
  330    CONTINUE
         ZCA(4,1) = ZCA(4,2)
C
C (5,*),(6,*)
         DO 430 K=1,MZA
            ZCA(5,K) = 1.0D0/ZCA(3,K)
            ZCA(6,K) = 1.0D0/ZCA(4,K)
  430    CONTINUE
C
C (7,*),(8,*)
         DO 530 K=1,MZMA
            ZCA(7,K) = ZCA(4,K+1) / ( ZCA(4,K)+ZCA(4,K+1) )
            ZCA(8,K) = ZCA(4,K  ) / ( ZCA(4,K)+ZCA(4,K+1) )
  530    CONTINUE
         ZCA(7,MZA) = 0.5D0
         ZCA(8,MZA) = 0.5D0
      ENDIF
C
      RETURN
      END
