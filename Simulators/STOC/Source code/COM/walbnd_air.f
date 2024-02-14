      SUBROUTINE WALBND_AIR(AKA,EPA,UUA,VVA,WWA,XC,YC,ZCA,
     $                      INDPA,INDUA,INDVA,INDWA,NC,AKX,EPX)
C======================================================================
C     壁面に接するkとεの値を計算する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
      INCLUDE 'TURBR.h'
      include 'FILE.h'
      include 'TIMEI.h'
C
      REAL(8),INTENT(INOUT)::AKA(MX,MY,MZA),EPA(MX,MY,MZA)
      REAL(8),INTENT(IN)::UUA(MX,MY,MZA),VVA(MX,MY,MZA),WWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::XC(8,MX,MY),YC(8,MY),ZCA(8,MZA)
      INTEGER,INTENT(IN)::INDPA(MX,MY,MZA),INDUA(MX,MY,MZA)
      INTEGER,INTENT(IN)::INDVA(MX,MY,MZA),INDWA(MX,MY,MZA)
      INTEGER::NC(MX,MY,MZA)
      REAL(8)::AKX(MX,MY,MZA),EPX(MX,MY,MZA)
C
      REAL(8)::ANU,DCMU,UU1,VV1,WW1,DD1,VEL1,VTAU,RNC
      INTEGER::I,J,K,INB1,JNB1,KNB1
C
C
      ANU=AMUAIR/RHOAIR
      DCMU = 0.0D0
      IF(CMU.GT.0.0D0) DCMU=1.0D0/SQRT(CMU)
C
      CALL ZERCLI(NC,MXY*MZA,0)
      CALL ZERCLR(AKX,MXY*MZA,0.0D0)
      CALL ZERCLR(EPX,MXY*MZA,0.0D0)
C
C----------------------------------------------------------------------
C     壁面境界
C----------------------------------------------------------------------
C ... X-DIR.
      DO J=2,MYM
      DO I=2,MXM-1
         DO K=2,MZMA
            IF( INDUA(I,J,K).EQ.-2 ) THEN
               IF( INDPA(I  ,J,K).GT.0 ) THEN
                  INB1 = I
               ELSEIF( INDPA(I+1,J,K).GT.0 ) THEN
                  INB1 = I+1
               ELSE
                  WRITE(*,*) 'ERROR1: WALBND_AIR',I,J,K
               ENDIF
               VV1  = 0.5D0*(VVA(INB1,J-1,K)+VVA(INB1,J,K))
               WW1  = 0.5D0*(WWA(INB1,J,K-1)+WWA(INB1,J,K))
               VEL1 = SQRT(VV1*VV1+WW1*WW1)
               DD1  = 0.5D0*XC(4,I,J)
               CALL LOGLAW(VTAU,VEL1,DD1,ANU)
               NC(INB1,J,K)  = NC(INB1,J,K)+1
               AKX(INB1,J,K) = AKX(INB1,J,K)+VTAU*VTAU*DCMU
               EPX(INB1,J,K) = EPX(INB1,J,K)+VTAU**3/(AKAR*DD1)
            ELSEIF( INDUA(I,J,K).GT.0 ) THEN
               EXIT
            ENDIF
         ENDDO
      ENDDO
      ENDDO
C
C ... Y-DIR.
      DO J=2,MYM-1
      DO I=2,MXM
         DO K=2,MZMA
            IF( INDVA(I,J,K).EQ.-2 ) THEN
               IF( INDPA(I,J  ,K).GT.0 ) THEN
                  JNB1 = J
               ELSEIF( INDPA(I,J+1,K).GT.0 ) THEN
                  JNB1 = J+1
               ELSE
                  WRITE(*,*) 'ERROR2: WALBND_AIR',I,J,K
               ENDIF
               UU1  = 0.5D0*(UUA(I-1,JNB1,K)+UUA(I,JNB1,K))
               WW1  = 0.5D0*(WWA(I,JNB1,K-1)+WWA(I,JNB1,K))
               VEL1 = SQRT(UU1*UU1+WW1*WW1)
               DD1  = 0.5D0*YC(4,J)
               CALL LOGLAW(VTAU,VEL1,DD1,ANU)
               NC(I,JNB1,K)  = NC(I,JNB1,K)+1
               AKX(I,JNB1,K) = AKX(I,JNB1,K)+VTAU*VTAU*DCMU
               EPX(I,JNB1,K) = EPX(I,JNB1,K)+VTAU**3/(AKAR*DD1)
            ELSEIF( INDUA(I,J,K).GT.0 ) THEN
               EXIT
            ENDIF
         ENDDO
      ENDDO
      ENDDO
C
C
C ... Z-DIR.
      DO J=2,MYM
      DO I=2,MXM
         DO K=1,MZMA
            IF( INDWA(I,J,K).EQ.-2 ) THEN
               IF( INDPA(I,J,K  ).GT.0 ) THEN
                  KNB1 = K
               ELSEIF( INDPA(I,J,K+1).GT.0 ) THEN
                  KNB1 = K+1
               ELSE
                  WRITE(*,*) 'ERROR3: WALBND_AIR',I,J,K
               ENDIF
               UU1  = 0.5D0*(UUA(I-1,J,KNB1)+UUA(I,J,KNB1))
               VV1  = 0.5D0*(VVA(I,J-1,KNB1)+VVA(I,J,KNB1))
               VEL1 = SQRT(UU1*UU1+VV1*VV1)
               DD1  = 0.5D0*ZCA(4,K)
               CALL LOGLAW(VTAU,VEL1,DD1,ANU)
               NC(I,J,KNB1)  = NC(I,J,KNB1)+1
               AKX(I,J,KNB1) = AKX(I,J,KNB1)+VTAU*VTAU*DCMU
               EPX(I,J,KNB1) = EPX(I,J,KNB1)+VTAU**3/(AKAR*DD1)
c               if( k.eq.1.and.i.eq.2 )
c     $            write(lp,*) 'vtau=',vtau,knb1,
c     $            NC(I,J,KNB1),VTAU*VTAU*DCMU,VTAU**3/(AKAR*DD1)
            ELSEIF( INDWA(I,J,K).GT.0 ) THEN
               EXIT
            ENDIF
         ENDDO
      ENDDO
      ENDDO
C
C ... 平均化処理
      DO K=2,MZMA
      DO J=2,MYM
      DO I=2,MXM
         IF( NC(I,J,K).GT.0 ) THEN
            RNC=1.D0/NC(I,J,K)
            AKA(I,J,K)=AKX(I,J,K)*RNC
            EPA(I,J,K)=EPX(I,J,K)*RNC
         ENDIF
      ENDDO
      ENDDO
      ENDDO
C
Cdbg      if( debug_air11.eq.1 ) then
Cdbg         do j=1,my
Cdbg            write(lp,*) 'aka2 j=',j,' istep=',istep
Cdbg            write(lp,'(a5,<mx>i10)') ' k%i|',(i,i=1,mx)
Cdbg            do k=mza,1,-1
Cdbg               write(lp,'(i4,a1,<mx>e10.3)') k,'|',(aka(i,j,k),i=1,mx)
Cdbg            enddo
Cdbg            write(lp,*) ''
Cdbg         enddo
Cdbg         do j=1,my
Cdbg            write(lp,*) 'epa2 j=',j,' istep=',istep
Cdbg            write(lp,'(a5,<mx>i10)') ' k%i|',(i,i=1,mx)
Cdbg            do k=mza,1,-1
Cdbg               write(lp,'(i4,a1,<mx>e10.3)') k,'|',(epa(i,j,k),i=1,mx)
Cdbg            enddo
Cdbg            write(lp,*) ''
Cdbg         enddo
Cdbg      endif
      RETURN
      END
