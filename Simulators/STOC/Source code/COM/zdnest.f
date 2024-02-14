      SUBROUTINE ZDNEST(UU,FF,GX0,XY,ZC,INDU,I,J,INB,JNB,MXY1,
     $                  INDU_ML,II,JJ,K_NS,IEAS_ML,IWES_ML,JSOU_ML,
     $                  JNOR_ML,KBOT_ML,KTOP_ML)
C======================================================================
C     ネスティング境界において法線方向流速成分の鉛直方向分布を補正する
C
C     I  : 境界部のI
C     J  : 境界部のJ
C     INB: 隣接する参照位置のI (東西境界ではJ==JNB)
C     JNB: 隣接する参照位置のJ (南北境界ではI==INB)
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'FILE.h'
C
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ)
      REAL(8),INTENT(IN)::FF(MX,MY,MZ)
      REAL(8),INTENT(IN)::GX0(MX,MY,MZ)
      REAL(8),INTENT(IN)::XY(8,MXY1),ZC(8,MZ)
      INTEGER,INTENT(IN)::INDU(MX,MY,MZ)
      INTEGER,INTENT(IN)::I,J
      INTEGER,INTENT(IN)::INB,JNB,MXY1
      INTEGER,INTENT(IN)::
     $   INDU_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $           KBOT_ML-1:KTOP_ML+1)
      INTEGER,INTENT(IN)::II,JJ
      INTEGER,INTENT(IN)::K_NS(2,MZ)
      INTEGER,INTENT(IN)::IEAS_ML,IWES_ML,JSOU_ML,
     $                    JNOR_ML,KBOT_ML,KTOP_ML
C
C
C ... LOCAL ARRAY
      REAL(8):: DZBND(MZM) ! 境界部の層厚
      REAL(8):: DZNAB(MZM) ! 隣接する参照位置での層厚
      REAL(8):: DZSCL(MZM) ! DZNABを境界部に合せてスケーリングした層厚
      REAL(8):: VZBND(MZM) ! 境界部の流速の法線方向成分
      REAL(8):: VZNAB(MZM) ! 隣接する参照位置での流速の法線方向成分
      REAL(8):: VZSCL(MZM) ! VZNABをDZBNDに合せて設定しなおした値
      REAL(8):: ZZBND(MZM) ! DZBNDで地面を0として積み上げたZ座標
      REAL(8):: ZZSCL(MZM) ! DZSCLで地面を0として積み上げたZ座標
C
      INTEGER::KGBND,KFBND ! 境界部の地面高さKと水面高さK
      INTEGER::KGNAB,KFNAB ! 隣接する参照位置の地面高さKと水面高さK
      REAL(8)::SUMDZB,SUMDZN,SUMVZB,SUMVZN
      REAL(8)::VAVEBND,VAVENAB
      INTEGER::IJ  ,IP  ,JP
      INTEGER::IJNB,INBP,JNBP
      REAL(8)::FFI,FF0,GX1,DZZ,UZZ,Z1,Z2,DBGZ,DBGU
      INTEGER::K,K2
C
C
      IF( J.EQ.JNB ) THEN      ! 東西境界
         IJ=I
         IP=I+1
         JP=J
         IJNB=INB
         INBP=INB+1
         JNBP=J
      ELSEIF( I.EQ.INB ) THEN  ! 南北境界
         IJ=J
         IP=I
         JP=J+1
         IJNB=JNB
         INBP=I
         JNBP=JNB+1
      ELSE
         CALL ERRMSG('ZDNEST',7230)
         WRITE(LP,*) 'UNEXPCTED ERROR',I,INB,J,JNB
         CALL ABORT1('')
      ENDIF
C
C----------------------------------------
C     (0) 初期化
C----------------------------------------
      DZBND(:)=0.0D0
      DZNAB(:)=0.0D0
      DZSCL(:)=0.0D0
      VZBND(:)=0.0D0
      VZNAB(:)=0.0D0
      VZSCL(:)=0.0D0
      ZZBND(:)=0.0D0
      ZZSCL(:)=0.0D0
      SUMDZB=0.0D0
      SUMDZN=0.0D0
      SUMVZB=0.0D0
      SUMVZN=0.0D0
      KGBND=0
      KFBND=0
      KGNAB=0
      KFNAB=0
C
C----------------------------------------
C     (1) 鉛直方向の積分計算
C----------------------------------------
      DO K=2,MZM
C ...... 境界位置
         IF( INDU(I,J,K).EQ.-1 ) THEN
            FFI = (FF(I,J,K)*XY(7,IJ)+FF(IP,JP,K)*XY(8,IJ))
            GX1 = 1.0D0-GX0(I,J,K)
            FF0 = MAX(FFI-GX1,0.0D0)
            IF(FF0.GT.0.0D0.AND.INDU_ML(II,JJ,K_NS(1,K)).GT.-2) THEN
               DZBND(K) = FF0*ZC(4,K)
               VZBND(K) = UU(I,J,K)
               ZZBND(K) = ZZBND(K-1) + DZBND(K)
C
               IF(KGBND.EQ.0) KGBND=K
               KFBND=K
               SUMDZB=SUMDZB+DZBND(K)
               SUMVZB=SUMVZB+DZBND(K)*VZBND(K)
            ENDIF
         ENDIF
C
C ...... 参照位置
         IF( INDU(INB,JNB,K).GE.0 ) THEN
            FFI = (FF(INB,JNB,K)*XY(7,IJNB)+FF(INBP,JNBP,K)*XY(8,IJNB))
            GX1 = 1.0D0-GX0(INB,JNB,K)
            FF0 = MAX(FFI-GX1,0.0D0)
            IF(FF0.GT.0.0D0) THEN
               DZNAB(K) = FF0*ZC(4,K)
               VZNAB(K) = UU(INB,JNB,K)
C
               IF(KGNAB.EQ.0) KGNAB=K
               KFNAB=K
               SUMDZN=SUMDZN+DZNAB(K)
               SUMVZN=SUMVZN+DZNAB(K)*VZNAB(K)
            ENDIF
         ENDIF
      ENDDO
C
C ... 平均流速
      VAVEBND = SUMVZB/MAX(SUMDZB,EPSH)
      VAVENAB = SUMVZN/MAX(SUMDZN,EPSH)
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     隣接位置で鉛直分布が定義できないとき(1層もしくは水がないとき)
C     境界位置で鉛直分布が定義できないとき(1層もしくは水がないとき)は変更しない
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF( KFNAB-KGNAB.LT.1.OR.
     $   (KFNAB-KGNAB.EQ.1.AND.ISW(3).EQ.1) ) RETURN
      IF( KFBND-KGBND.LT.1.OR.
     $   (KFBND-KGBND.EQ.1.AND.ISW(3).EQ.1) ) RETURN
C
C----------------------------------------
C     (2) 参照位置のメッシュのスケーリングと
C         スケーリング後のメッシュへの流速の設定
C----------------------------------------
      DO K=KGNAB,KFNAB
         DZSCL(K) = DZNAB(K)*SUMDZB/MAX(SUMDZN,EPSH)
         ZZSCL(K) = ZZSCL(K-1) + DZSCL(K)
      ENDDO
C ... スケーリングによって平均流速VAVENABは変化しない
C
      IF( ABS(ZZSCL(KFNAB)-ZZBND(KFBND)).GT.1.0D-5 ) THEN
         CALL ERRMSG('ZDNEST',7231)
         WRITE(LP,*) 'UNEXPECTED CONVERSION ERROR'
         CALL ABORT1('')
      ENDIF
C
C
c ... デバッグ用処理
      dbgz=0.0d0
      dbgu=0.0d0
C
      DO K=KGBND,KFBND
         DZZ=0.0D0
         UZZ=0.0D0
         DO K2=KGNAB,KFNAB
            Z1=MAX(ZZSCL(K2-1),ZZBND(K-1))
            Z2=MIN(ZZSCL(K2),ZZBND(K))
            DZZ=DZZ+MAX(Z2-Z1,0.0D0)
            UZZ=UZZ+MAX(Z2-Z1,0.0D0)*VZNAB(K2)
         ENDDO
         VZSCL(K) = UZZ/MAX(DZZ,EPSH)
c ...... デバッグ用処理
         dbgz=dbgz+DZZ
         dbgu=dbgu+UZZ
      ENDDO
C
c ... デバッグ用処理
      dbgu=dbgu/max(dbgz,EPSH)
      if( abs(dbgu-VAVENAB).GT.1.0D-5 ) then
         CALL ERRMSG('ZDNEST',7232)
         WRITE(LP,*) 'UNEXPECTED V-AVERAGE ERROR'
         WRITE(LP,*) 'dbgu=',dbgu
         WRITE(LP,*) 'VAVENAB=',VAVENAB
         CALL ABORT1('')
      endif
C
C----------------------------------------
C     (3) 境界流速の補正
C----------------------------------------
      DO K=KGBND,KFBND
         UU(I,J,K) = VAVEBND + ( VZSCL(K) - VAVENAB )
      ENDDO
C
      RETURN
      END
