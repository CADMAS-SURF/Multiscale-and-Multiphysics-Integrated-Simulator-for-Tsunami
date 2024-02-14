      SUBROUTINE SETDT
C----------------------------------------
C     時間刻みを設定する
C----------------------------------------
      USE M_GRID
      USE M_TIME
      USE M_DRIFT
      USE M_OUTPUT,ONLY:IFL
C
      IMPLICIT NONE
C
      REAL(8)::U1,U2,U3,U4,U5,U6,V1,V2,V3,V4,V5,V6,UMAX,VMAX
      INTEGER N,NA
      REAL(8)::DXMIN,DYMIN
C
C
C ... 固定の場合は何もしない
      IF( IDT.EQ.0 ) RETURN
C
      UMAX = 0.0D0
      VMAX = 0.0D0
      DO N=1,ND
         IF( LD(N).NE.1 ) CYCLE
         U1 = ABS(UD(N) - OD(N)*(YVERT(1,N)-YD(N)))
         U2 = ABS(UD(N) - OD(N)*(YVERT(2,N)-YD(N)))
         U3 = ABS(UD(N) - OD(N)*(YVERT(3,N)-YD(N)))
         U4 = ABS(UD(N) - OD(N)*(YVERT(4,N)-YD(N)))
         U5 = ABS(UD(N) - OD(N)*(YCOLLIS(1,N)-YD(N)))
         U6 = ABS(UD(N) - OD(N)*(YCOLLIS(2,N)-YD(N)))
         V1 = ABS(VD(N) + OD(N)*(XVERT(1,N)-XD(N)))
         V2 = ABS(VD(N) + OD(N)*(XVERT(2,N)-XD(N)))
         V3 = ABS(VD(N) + OD(N)*(XVERT(3,N)-XD(N)))
         V4 = ABS(VD(N) + OD(N)*(XVERT(4,N)-XD(N)))
         V5 = ABS(VD(N) + OD(N)*(XCOLLIS(1,N)-XD(N)))
         V6 = ABS(VD(N) + OD(N)*(XCOLLIS(2,N)-XD(N)))
         UMAX = MAX(UMAX,U1,U2,U3,U4,U5,U6)
         VMAX = MAX(VMAX,V1,V2,V3,V4,V5,V6)
      ENDDO
C
      DXMIN=1.0D10
      DYMIN=1.0D10
      DO NA=1,MXAREA
      DXMIN = MIN(DX(NA),DXMIN)
      DYMIN = MIN(DY(NA),DYMIN)
      ENDDO
      DT = CSAFE*MIN(DXMIN/MAX(UMAX,1.0D-10),
     $               DYMIN/MAX(VMAX,1.0D-10))
      DT = MIN(MAX(DT,DTMIN),DTMAX)
C
      if( mod(ns,500).eq.1 )
     $   write(IFL,90) umax,vmax
   90 format('u,v-max=',F6.1,F6.1)
C
      RETURN
      END
