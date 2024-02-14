      SUBROUTINE STFGL(ESTF,ST,T,N,NDIM)
C
C     ESTF : IN   : 要素剛性行列(local座標系)
C            OUT  : 要素剛性行列(global座標系)
C     ST   : WORK : ワーク
C     T    : IN   : global -> local 座標変換行列
C     N    : IN   : ESTFの行(列)サイズ
C     NDIM : IN   : 解析次元(2 or 3)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION  ESTF(1),ST(N,N),T(NDIM,NDIM)
C
C
C--- 0 CLEAR ---
      NNN = N * N
      CALL CLEAR1( ST,NNN )
C
C
C--- ESTF X T ---
      IR   = N
      LAST = 0
      DO 4000 I=1,N
         IIC  = 0
         LP   = 0
C
      DO 3000 J=1,N,NDIM
C
         DO 2000 K=1,NDIM
            IC       = IIC
            JK       = J + K - 1
C
         DO 1000 L=1,NDIM
            JL = J + L - 1
            IF( I .LE. JL )  THEN
                IAD = LAST + JL - I + 1
             ELSE
                IAD = I  + IC
                IC  = IC + N - L - LP
            ENDIF
C
            ST(I,JK) = ST(I,JK) + ESTF(IAD)*T(L,K)
C
 1000    CONTINUE
 2000    CONTINUE
            IIC = IC
            LP  = LP + NDIM
C
 3000 CONTINUE
         LAST = LAST + IR
         IR   = IR - 1
 4000 CONTINUE
C
C
C--- TT X ESTF X T ---
      IAD = 0
      DO 8000 I=1,N
         I3 = I - (I-1)/NDIM * NDIM
         II = (I-1)/NDIM * NDIM
C
      DO 7000 J=I,N
         IAD       = IAD + 1
         ESTF(IAD) = 0.0D0
C
         DO 6000 K=1,NDIM
            ESTF(IAD) = ESTF(IAD) + T(K,I3)*ST(II+K,J)
 6000    CONTINUE
C
 7000 CONTINUE
 8000 CONTINUE
C
C
      RETURN
      END
