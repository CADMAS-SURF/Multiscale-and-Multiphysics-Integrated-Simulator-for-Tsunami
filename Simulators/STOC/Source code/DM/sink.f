      SUBROUTINE SINK
C----------------------------------------
C     漂流物の沈降計算を行う(H21モデル)
C----------------------------------------
      USE M_TIME,ONLY:DT
      USE M_DRIFT,ONLY:ND,AM,AI,AD,AL,AB,BD,AZ1,AM0,AI0,AD0,BD0,
     $                 ALPD,CKD,WSD,TIME_S,TIME_D,IFLAG_S,IFLAG_D,
     $                 HFD,HTD,HZD,LD
c    $                 HFD,HTD,LD
cmod 100428 1(upper)
      USE M_FLUID,ONLY:RHO
C
      IMPLICIT NONE
C
      REAL(8),PARAMETER::GRAV=9.807D0
      REAL(8)::TS,AMMAX,AMADD,COEF,AIMAX,AIADD,VOL,AC
      INTEGER::N
C
C
C----------------------------------------
C     (A) 増水による沈降開始の判定
C----------------------------------------
      DO N=1,ND
cmod 100428 1
         IF( LD(N).EQ.0 ) CYCLE ! スキップする
         IF( IFLAG_S(N).EQ.0.AND.TIME_S(N).GT.0.0D0 ) THEN
            IF( HFD(N)-AD0(N).GE.HTD(N) ) THEN ! 水位-喫水が標高値よりも大きくなったら沈降開始
               IFLAG_S(N) = 1
            ENDIF
         ENDIF
      ENDDO
C
C
C----------------------------------------
C     (B) 沈降速度と重量と慣性モーメントの増加計算
C----------------------------------------
      DO N=1,ND
         IF( IFLAG_S(N).LE.0 .AND. IFLAG_D(N).LE.0 ) CYCLE
C
         IF( IFLAG_S(N).GE.1 .AND. IFLAG_D(N).GE.1 ) THEN
            TS=MIN(TIME_S(N),TIME_D(N))
         ELSE IF( IFLAG_S(N).GE.1 ) THEN
            TS=TIME_S(N)
         ELSE
            TS=TIME_D(N)
         ENDIF
C
         IF( LD(N).EQ.-2 ) THEN ! 領域外
C             ! 計算対象外のため、何もしない
C
cmod 100428 1
c        ELSE IF( LD(N).EQ.-1 ) THEN ! 地面乗り上げ時
         ELSE IF( LD(N).EQ.-1 .or. HZD(N).EQ.HTD(N) ) THEN ! 地面乗り上げ時
            IF( IFLAG_S(N).EQ.1 ) IFLAG_S(N)=4
            IF( IFLAG_D(N).EQ.1 ) IFLAG_D(N)=4
            IF( IFLAG_S(N).EQ.2 ) IFLAG_S(N)=3
            IF( IFLAG_D(N).EQ.2 ) IFLAG_D(N)=3
            WSD(N)=0.0D0
C
         ELSE
            IF( IFLAG_S(N).EQ.4 ) IFLAG_S(N)=1
            IF( IFLAG_D(N).EQ.4 ) IFLAG_D(N)=1
            IF( IFLAG_S(N).EQ.3 ) IFLAG_S(N)=2
            IF( IFLAG_D(N).EQ.3 ) IFLAG_D(N)=2
C
            VOL=AL(N)*AB(N)*AZ1(N)
            AMMAX=AM0(N)+RHO*ALPD(N)*VOL
            IF( AM(N).LT.AMMAX ) THEN
               AMADD=(AMMAX-AM0(N))/TS*DT
               AM(N)=MIN(AM(N)+AMADD,AMMAX)
            ENDIF
C
cmod 101121 1
            COEF=(AL(N)**2+AB(N)**2)/12.0D0
            AIMAX=AI0(N)+(RHO*ALPD(N)*VOL)*COEF
            IF( AI(N).LT.AIMAX.AND.LD(N).GE.0 ) THEN
               AIADD=(AIMAX-AI0(N))/TS*DT
               AI(N)=MIN(AI(N)+AIADD,AIMAX)
            ENDIF
C
            IF( AD(N).LT.AZ1(N) ) THEN
               WSD(N)=(AZ1(N)-AD0(N))/TS
               AD(N)=MIN(AD(N)+WSD(N)*DT,AZ1(N))
               BD(N)=MIN(BD(N)+WSD(N)*DT,AZ1(N)+BD0(N)-AD0(N))
            ELSE
               IF( IFLAG_S(N).GE.1 ) IFLAG_S(N)=2
               IF( IFLAG_D(N).GE.1 ) IFLAG_D(N)=2
               AC=(1.0D0-RHO*VOL/AMMAX)*GRAV*CKD(N)
               AC=MAX(AC,0.0D0)
               WSD(N)=WSD(N)+AC*DT
            ENDIF
C
         ENDIF
C
      ENDDO
C
      RETURN
      END
