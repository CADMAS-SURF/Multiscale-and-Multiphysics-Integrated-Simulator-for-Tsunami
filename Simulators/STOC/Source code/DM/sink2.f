      SUBROUTINE SINK2
C----------------------------------------
C     漂流物の沈降計算を行う(H22モデル)
C----------------------------------------
      USE M_TIME,ONLY:DT
      USE M_DRIFT,ONLY:ND,AM,AI,AD,AL,AB,BD,AZ1,AM0,AI0,AD0,BD0,
     $                 ALPD,CKD,WSD,IFLAG_D,HFD,HTD,HZD,HAD,
     $                 LD,ABHOLE,ATHOLE,ABHOLED,ATHOLED,
     $                 QD,AHIN,CQD,CDD
      USE M_FLUID,ONLY:RHO
C
      IMPLICIT NONE
C
      REAL(8),PARAMETER::GRAV=9.807D0
      REAL(8)::COEF,VOL,DF,AD1,ABH,ATH
      INTEGER::N
C
C
C----------------------------------------
C     (A) 浸水速度の計算
C----------------------------------------
      COEF=SQRT(2.0D0*GRAV)
      DO N=1,ND
         IF( IFLAG_D(N).EQ.0 ) THEN
            ABH=ABHOLE(N)
            ATH=ATHOLE(N)
         ELSE
            ABH=ABHOLED(N)
            ATH=ATHOLED(N)
         ENDIF
C
         IF( AD(N).GT.AZ1(N) ) THEN
            IF( AHIN(N).EQ.AZ1(N) ) THEN    ! パターン(e)
               QD(N)=0.0D0
            ELSE                            ! パターン(d)
               QD(N)=ABH*CQD(N)*COEF*SQRT(AD(N)-AHIN(N))
     $              +ATH*CQD(N)*COEF*SQRT(AD(N)-AZ1(N))
            ENDIF
         ELSE
            IF( AHIN(N).EQ.AZ1(N) ) THEN    ! パターン(c)
               QD(N)=-ABH*CQD(N)*COEF*SQRT(AHIN(N)-AD(N))
            ELSEIF( AD(N).GT.AHIN(N) ) THEN ! パターン(a)
               QD(N)= ABH*CQD(N)*COEF*SQRT(AD(N)-AHIN(N))
            ELSE                            ! パターン(b)
               QD(N)=-ABH*CQD(N)*COEF*SQRT(AHIN(N)-AD(N))
            ENDIF
         ENDIF
      ENDDO
C
C
C----------------------------------------
C     (B) 内部水位と質量と慣性モーメントの計算
C         および沈降速度と喫水の計算
C----------------------------------------
      DO N=1,ND
         IF( LD(N).EQ.-2 ) CYCLE ! 領域外
C
C ...... 質量保存式
         AHIN(N)=MIN( MAX( AHIN(N)+QD(N)*DT/(ALPD(N)*AL(N)*AB(N))
     $                    ,0.0D0), AZ1(N))
         VOL=ALPD(N)*AL(N)*AB(N)*AHIN(N)
         COEF=(AL(N)**2+AB(N)**2)/12.0D0
         AM(N)=AM0(N)+RHO*VOL
         AI(N)=AI0(N)+RHO*VOL*COEF
C
C ...... 運動方程式
         AD1=MIN(AZ1(N),AD(N)+WSD(N)*DT)
         WSD(N)=(AM(N)*WSD(N)
     $         + AM(N)*GRAV*DT -RHO*GRAV*AL(N)*AB(N)*AD1*DT)
     $         /(AM(N)+0.5D0*CDD(N)*RHO*AL(N)*AB(N)*ABS(WSD(N))*DT)
         AD(N)=AD(N)+WSD(N)*DT
         BD(N)=BD(N)+WSD(N)*DT
         HZD(N)=HZD(N)-WSD(N)*DT
         HAD(N)=HAD(N)-WSD(N)*DT
C
C        着底
         DF=HFD(N)-HTD(N) ! 重心位置での全水深
         IF( BD(N).GE.DF ) THEN
cccc            LD(N)=-1
            IF( LD(N).NE.0 ) LD(N)=-1
            BD(N)=DF
            AD(N)=BD(N)-BD0(N)+AD0(N)
            WSD(N)=0.0D0
C
            HZD(N)=HTD(N)
            HAD(N)=HTD(N)+BD0(N)-AD0(N)
         ENDIF
      ENDDO
C
      RETURN
      END
