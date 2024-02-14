      SUBROUTINE BCINLF(FF,ZC,INDP,HH,HDEP,HHBCN,IFL)
C======================================================================
C     流速固定境界および自由流出境界のF値を設定する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'AREA.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'BOUNDR.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'TABLER.h'
      INCLUDE 'FILE.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
C// 2006.06.23
      INCLUDE 'CONNEC.h'
C END//
      INCLUDE 'CADMAS.h'
C// 2004.01.16
C      COMMON  / ADD0116 / AMP,TTT,ALL,HHH,AXX
      INCLUDE 'RGWAVE.h'
      REAL(8),INTENT(INOUT)::HH(MX,MY),HDEP(MX,MY)
C END//
      REAL(8),INTENT(INOUT)::FF(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::ZC(8,MZ)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HHBCN(MX,MY)
      INTEGER,INTENT(INOUT)::IFL
      INTEGER::IDB=0
C
      REAL(8)::AKK,AKX,DDD,FF1,HH1,HH2,HH3,HH4,PAI,SIGMA
      INTEGER::I,IDIR,IE,IH1,IH2,IS,J,JE,JS,K,KE,KS,L,M,N
C
      IF( NESTFL.GT.0 ) THEN
C----------------------------------------------------------------------(START)
C     親領域の境界水位をFFに設定
C
C// 2006.06.23
      IF( IPECON(5,NRANK+1).LT.0 ) THEN
        I = 1
        DO 50 K=2,MZM
        DO 50 J=2,MYM
          HH1 = HHBCN(I,J)
          FF(I,J,K) = MIN(1.0D0,MAX(0.0D0,(HH1-ZC(1,K-1))*ZC(6,K)))
c          if(idb.ne.0) write(*,1) i,j,k,hh1,ff(i,j,k)
   50   CONTINUE
      END IF
C
      IF( IPECON(6,NRANK+1).LT.0 ) THEN
        I = MX
        DO 60 K=2,MZM
        DO 60 J=2,MYM
          HH2 = HHBCN(I,J)
          FF(I,J,K) = MIN(1.0D0,MAX(0.0D0,(HH2-ZC(1,K-1))*ZC(6,K)))
c          if(idb.ne.0) write(*,1) i,j,k,hh2,ff(i,j,k)
   60   CONTINUE
      END IF
C
      IF( IPECON(4,NRANK+1).LT.0 ) THEN
        J = 1
        DO 70 K=2,MZM
        DO 70 I=2,MXM
          HH3 = HHBCN(I,J)
          FF(I,J,K) = MIN(1.0D0,MAX(0.0D0,(HH3-ZC(1,K-1))*ZC(6,K)))
c          if(idb.ne.0) write(*,1) i,j,k,hh3,ff(i,j,k)
   70   CONTINUE
      END IF
C
      IF( IPECON(7,NRANK+1).LT.0 ) THEN
        J = MY
        DO 80 K=2,MZM
        DO 80 I=2,MXM
          HH4 = HHBCN(I,J)
          FF(I,J,K) = MIN(1.0D0,MAX(0.0D0,(HH4-ZC(1,K-1))*ZC(6,K)))
c          if(idb.ne.0) write(*,1) i,j,k,hh4,ff(i,j,k)
   80   CONTINUE
      END IF
C END//
 1        format('cp_bcinlf i,j,hh1,ff(i,j,k)=',3i5,1p,2d13.5)
      END IF
CC      IF(IFL.NE.0) RETURN
      IF(IFL.GT.0) RETURN
C 
C----------------------------------------------------------------------(END)
C
C----------------------------------------------------------------------
C     (1) 流速固定境界のF値を設定する(一定値または時系列値)
C----------------------------------------------------------------------
      DO 100 N=1,NINLT
C ...... CADMASとの連成時、時間積分前は連成境界に関しては何もしない
         IF( NB_SC.GT.0 .AND. N.LE.4 .AND. IFL.EQ.-1 ) GOTO 100
         M  = MINLT(N)
         IS = IAREA(1,M)
         IE = IAREA(2,M)
         JS = IAREA(3,M)
         JE = IAREA(4,M)
         KS = IAREA(5,M)
         KE = IAREA(6,M)
         IDIR = IAREA(7,M)
         IH1 = IINLT(4,N)
C
         IF( IH1.EQ. 0 ) THEN
            HH1 = RINLT(4,N)
         ELSE IF(IH1.GT.0) THEN
            HH1 = TABLE(IH1)
         ELSE
            IH2 = -IH1
            HH1 = HTIDE(1,IH2)
            DO 105 L=1,NTIDE
              HH1 = HH1+RTIDE(1,L,IH2)*COS(ROMEG(L)*TIME-RTIDE(2,L,IH2))
  105       CONTINUE
            HTIDE(2,IH2) = HH1
         END IF
         IF(LSURF.EQ.0) HH1=ZC(1,MZM)
C
C// 2004.01.16
C (模型実験対応)......................................................
C
C        潮位を時間の関数にする場合、上のinclude文を追加しHH1を計算する
C           AMP:片振幅(m) , TTT :周期(s)  , TIME:時刻(s)
C           SIGMA:角周波数(1/s) , DDD:位相差(rad.)
C        境界条件としては、便宜的に一定値入力としておく
C
ckt       IF(AMP.GT.0.0D0) THEN
       IF(AMP.GT.0.0D0.or.AMP.LT.0.0D0) THEN
         PAI = 3.141592653897932D0
C         AMP = 0.005D0
C         TTT = 40.D0
C         ALL = 125.21
C         HHH = 1.0D0
         SIGMA=2.0D0*PAI/TTT
         DDD = PAI*0.5D0
         HH1 = AMP*COS(-SIGMA*TIME+DDD)
         AKK = 2.0D0*PAI/ALL
C         AXX = 0.5D0
         AKX = AKK*AXX*0.5D0
         HH2 = AMP*COS(AKX-SIGMA*TIME+DDD)
C         HH(2,2) = HH2
C         write(16,161) TIME,HH1,HH2
 161     FORMAT('# BCINLF TIME,HH1,HH2=',1P3D15.7)
       END IF
C END//.....................................................
C
C ...... 法線方向がX方向の面
         IF( IDIR.EQ.1 ) THEN
            I = IS
            DO 110 K=KS,KE
            DO 110 J=JS,JE
               IF( NB_SC.GT.0.AND.N.EQ.1 ) HH1=UWCAD(J-JS+1,1,5)
               IF( NB_SC.GT.0.AND.N.EQ.2 ) HH1=UECAD(J-JS+1,1,5)
               FF1 = MIN(1.0D0,MAX(0.0D0,(HH1-ZC(1,K-1))*ZC(6,K)))
C// 2004.01.15
CC            IF(AMP.GT.0.0D0) THEN
CC               FF2 = MIN(MAX( HH2-ZC(1,K-1), 0.0D0 ), ZC(4,K)) * ZC(6,K)
CC               IF(IS.EQ.  1) FF(I+1,J,K)=FF2
CC               IF(IS.EQ.MXM) FF(I  ,J,K)=FF2
CC            END IF
C END//
               IF( INDP(I,J,K).GT.0 ) THEN
                  FF(I+1,J,K) = FF1
cmod130829(s)
                  if(nb_sc.gt.0.and.n.eq.1) then
                     if( k.eq.ks ) then
                        hh1=MAX(UWCAD(J-JS+1,1,6),HDEP(i,j)+EPSH)
                        hh(i,j) = hh1
c                       hh(i,j) = 0.5d0*(hh1+hh(i,j))
                     endif
                  endif
cmod130829(e)
               ELSE
                  FF(I,J,K) = FF1
cmod130829(s)
                  if(nb_sc.gt.0.and.n.eq.2) then
                     if( k.eq.ks ) then
                        hh1=MAX(UECAD(J-JS+1,1,6),HDEP(i+1,j)+EPSH)
                        hh(i+1,j) = hh1
c                       hh(i+1,j) = 0.5d0*(UECAD(J-JS+1,1,6)+hh(i+1,j))
                     endif
                  endif
cmod130829(e)
               END IF
  110       CONTINUE
C
C ......
         ELSE IF( IDIR.EQ.2 ) THEN
            J = JS
            DO 120 K=KS,KE
            DO 120 I=IS,IE
               IF( NB_SC.GT.0.AND.N.EQ.3 ) HH1=VSCAD(I-IS+1,1,5)
               IF( NB_SC.GT.0.AND.N.EQ.4 ) HH1=VNCAD(I-IS+1,1,5)
               FF1 = MIN(1.0D0,MAX(0.0D0,(HH1-ZC(1,K-1))*ZC(6,K)))
C// 2004.01.15
CC            IF(AMP.GT.0.0D0) THEN
CC               FF2 = MIN(MAX( HH2-ZC(1,K-1), 0.0D0 ), ZC(4,K)) * ZC(6,K)
CC               IF(JS.EQ.  1) FF(I,J+1,K)=FF2
CC               IF(JS.EQ.MYM) FF(I,J  ,K)=FF2
CC            END IF
C END//
               IF( INDP(I,J,K).GT.0 ) THEN
                  FF(I,J+1,K) = FF1
cmod130829(s)
                  if(nb_sc.gt.0.and.n.eq.3) then
                     if( k.eq.ks ) then
                        hh1=MAX(VSCAD(I-IS+1,1,6),HDEP(i,j)+EPSH)
                        hh(i,j) = hh1
c                       hh(i,j) = 0.5d0*(VSCAD(I-IS+1,1,6)+hh(i,j))
                     endif
                  endif
cmod130829(e)
               ELSE
                  FF(I,J,K) = FF1
cmod130829(s)
                  if(nb_sc.gt.0.and.n.eq.4) then
                     if( k.eq.ks ) then
                        hh1=MAX(VNCAD(I-IS+1,1,6),HDEP(i,j+1)+EPSH)
                        hh(i,j+1) = hh1
c                       hh(i,j+1) = 0.5d0*(VNCAD(I-IS+1,1,6)+hh(i,j+1))
                     endif
                  endif
cmod130829(e)
               END IF
  120       CONTINUE
         END IF
  100 CONTINUE
C
C
C----------------------------------------------------------------------
C     (2) 自由流入出境界のF値を設定する(勾配0)
C----------------------------------------------------------------------
      DO 200 N=1,NOUTLT
         M  = MOUTLT(N)
         IS = IAREA(1,M)
         IE = IAREA(2,M)
         JS = IAREA(3,M)
         JE = IAREA(4,M)
         KS = IAREA(5,M)
         KE = IAREA(6,M)
         IDIR = IAREA(7,M)
C
C ......
         IF( IDIR.EQ.1 ) THEN
            I = IS
            DO 210 K=KS,KE
            DO 210 J=JS,JE
               IF( INDP(I,J,K).GT.0 ) THEN
                  FF(I+1,J,K) = FF(I,J,K)
               ELSE
                  FF(I,J,K) = FF(I+1,J,K)
               END IF
  210       CONTINUE
C
C ......
         ELSE IF( IDIR.EQ.2 ) THEN
            J = JS
            DO 220 K=KS,KE
            DO 220 I=IS,IE
               IF( INDP(I,J,K).GT.0 ) THEN
                  FF(I,J+1,K) = FF(I,J,K)
               ELSE
                  FF(I,J,K) = FF(I,J+1,K)
               END IF
  220       CONTINUE
         END IF
  200 CONTINUE
C
C
      RETURN
      END
