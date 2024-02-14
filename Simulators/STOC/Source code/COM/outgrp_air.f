      SUBROUTINE OUTGRP_AIR(UUA,VVA,WWA,PPA,AKA,EPA,TMUA,FFA,GVA,
     $                  WRK1,WRK2,WRK3,IWRK1,IWRK2,IWRK3,IWRK4,INDPA)
C======================================================================
C     グラフィック出力を行う
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
      INCLUDE 'OUTPUT.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'GRID.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
C
      REAL(8),INTENT(INOUT)::UUA(MX,MY,MZA),VVA(MX,MY,MZA)
      REAL(8),INTENT(INOUT)::WWA(MX,MY,MZA),PPA(MX,MY,MZA)
      REAL(8),INTENT(INOUT)::AKA(MX,MY,MZA),EPA(MX,MY,MZA)
      REAL(8),INTENT(INOUT)::TMUA(MX,MY,MZA),FFA(MX,MY,MZA)
      REAL(8),INTENT(INOUT)::GVA(MX,MY,MZA)
      REAL(8),INTENT(INOUT)::WRK1(MX,MY,MZA),WRK2(MX,MY,MZA)
      REAL(8),INTENT(INOUT)::WRK3(MX,MY,MZA)
C
      INTEGER,INTENT(INOUT)::IWRK1(MX,MY,MZA),IWRK2(MX,MY,MZA)
      INTEGER,INTENT(INOUT)::IWRK3(MX,MY,MZA),IWRK4(MX,MY,MZA)
      INTEGER,INTENT(INOUT)::INDPA(MX,MY,MZA)
C
      REAL(8),ALLOCATABLE::BND1(:)
      REAL(8)::BND2(2)
      CHARACTER(16)::CPHYS1,CPHYS2
      INTEGER::IFLAG=0
C
      INTEGER::I,J,K,N,IERR
      INTEGER,SAVE::NM1,NM2
C
      INTEGER::NOBSS=0,IFLGRA=39
      INTEGER::IOBSS(6)=0
      INTEGER::NXYZA
C
C
      NXYZA=(MX-2)*(MY-2)*(MZA-2)
C ... グラフィックファイルのヘッダ(格子データ、形状データ等)の出力
      IF( IFLAG.EQ.0 ) THEN
         IFLAG = 1
         CALL PLCOD(XGRID,YGRID,ZGRIDA,MXM,MYM,MZMA,IFLGRA)
         CALL PLIND(INDPA,IWRK1,IWRK2,IWRK3,IWRK4,MXM,MYM,MZMA,
     $              NOBSS,IOBSS,NM1,NM2,IFLGRA)
         ALLOCATE(BND1(NM1),STAT=IERR)
         IF( IERR.NE.0 ) THEN
            CALL ERRMSG('OUTGRP_AIR',7150)
            WRITE(LP,*) 'CANNOT ALLOCATE AREA OF BND1(NM1),',
     $                 ' NM1=',NM1
            CALL ABORT1('')
         END IF
         BND2(1) = 0.0D0
         BND2(2) = 0.0D0
      ELSE
         call mkindx(IWRK1,IWRK2,IWRK3,IWRK4,indpa,mxm,mym,mzma,nm1,nm2,
     $               iobss)
         ALLOCATE(BND1(NM1),STAT=IERR)
         bnd2(1) = 0.0d0
         bnd2(2) = 0.0d0
      END IF
C
C
C ... 出力指定時刻(ステップ)毎の出力
C
      DO 100 N=1,MGRPH
C
         IF( CGRPH(N).EQ.'UA      ' .OR.
     $       CGRPH(N).EQ.'VA      ' .OR.
     $       CGRPH(N).EQ.'WA      ' ) THEN
            CPHYS1 = 'V               '
            CPHYS2 = 'VELOCITY        '
            CALL PLVEC(TIME,NXYZA,CPHYS1,CPHYS2,IFLGRA)
C
            DO 110 K=2,MZMA
            DO 110 J=2,MYM
            DO 110 I=2,MXM
               WRK1(I,J,K) = 0.5D0*(UUA(I-1,J,K)+UUA(I,J,K))
               WRK2(I,J,K) = 0.5D0*(VVA(I,J-1,K)+VVA(I,J,K))
               WRK3(I,J,K) = 0.5D0*(WWA(I,J,K-1)+WWA(I,J,K))
  110       CONTINUE
C
            WRITE(IFLGRA) (((WRK1(I,J,K),I=2,MXM),J=2,MYM ),K=2,MZMA)
            WRITE(IFLGRA) (((WRK2(I,J,K),I=2,MXM),J=2,MYM ),K=2,MZMA)
            WRITE(IFLGRA) (((WRK3(I,J,K),I=2,MXM),J=2,MYM ),K=2,MZMA)
C
         ELSE IF( CGRPH(N).EQ.'PA      ' ) THEN
            CPHYS1 = 'P               '
            CPHYS2 = 'PRESSURE        '
            CALL PLSCA(PPA,BND1,BND2,IWRK1,IWRK2,IWRK3,IWRK4,
     $                 TIME,MXM,MYM,MZMA,NM1,NM2,CPHYS1,CPHYS2,IFLGRA)
C
         ELSE IF( CGRPH(N).EQ.'KA      ' ) THEN
            CPHYS1 = 'K               '
            CPHYS2 = 'TURBULENT-ENERGY'
            CALL PLSCA(AKA,BND1,BND2,IWRK1,IWRK2,IWRK3,IWRK4,
     $                 TIME,MXM,MYM,MZMA,NM1,NM2,CPHYS1,CPHYS2,IFLGRA)
C
         ELSE IF( CGRPH(N).EQ.'EA      ' ) THEN
            CPHYS1 = 'E               '
            CPHYS2 = 'T.E.DIMINISHING '
            CALL PLSCA(EPA,BND1,BND2,IWRK1,IWRK2,IWRK3,IWRK4,
     $                 TIME,MXM,MYM,MZMA,NM1,NM2,CPHYS1,CPHYS2,IFLGRA)
C
         ELSE IF( CGRPH(N).EQ.'TMUA    ' ) THEN
            CPHYS1 = 'TMU             '
            CPHYS2 = 'TURB.VISCOSITY  '
            CALL PLSCA(TMUA,BND1,BND2,IWRK1,IWRK2,IWRK3,IWRK4,
     $                 TIME,MXM,MYM,MZMA,NM1,NM2,CPHYS1,CPHYS2,IFLGRA)
C
         ELSE IF( CGRPH(N).EQ.'FA      ' ) THEN
            CPHYS1 = 'F               '
            CPHYS2 = 'WATER-RATE      '
            CALL PLSCA(FFA,BND1,BND2,IWRK1,IWRK2,IWRK3,IWRK4,
     $                 TIME,MXM,MYM,MZMA,NM1,NM2,CPHYS1,CPHYS2,IFLGRA)
C
         ELSE IF( CGRPH(N).EQ.'GVA     ' ) THEN
            CPHYS1 = 'GV              '
            CPHYS2 = 'OBST-RATE       '
            CALL PLSCA(GVA,BND1,BND2,IWRK1,IWRK2,IWRK3,IWRK4,
     $                 TIME,MXM,MYM,MZMA,NM1,NM2,CPHYS1,CPHYS2,IFLGRA)
C
         ELSE
            CALL ERRMS2('OUTGRP_AIR',7151)
            WRITE(LP,*) '   VARIABLE ',CGRPH(N),' IS NOT DEFINED'
         END IF
C
  100 CONTINUE
C
      RETURN
      END
