      DOUBLE PRECISION FUNCTION ABRYDON(S,T)
C
C ... Brydon et al.(1999)の式により密度を計算する。
C     ただし、圧力は考慮しない。
C     適用範囲(0<S<42, -2<T<40)
C        S : 塩分 , T : 温度(°C)
C
      IMPLICIT NONE
C
      REAL(8),INTENT(IN)::s,T
      REAL(8),PARAMETER::c1=-9.20601D-2
      REAL(8),PARAMETER::c2=+5.10768D-2
      REAL(8),PARAMETER::c3=+8.05999D-1
      REAL(8),PARAMETER::c4=-7.40849D-3
      REAL(8),PARAMETER::c5=-3.01036D-3
      REAL(8),PARAMETER::c6=+3.32267D-5
      REAL(8),PARAMETER::c7=+3.21931D-5

      real(8) :: sigma
      
C      sigma = c1 + c2*t + c3*s + c4*t*t + c5*s*t + c6*t*t*t + c7*s*t*t
      sigma = c1 + s*c3 + t*(c2 + s*c5 + t*(c4 + s*c7 + t*c6))

      abrydon = 1.0D3 + sigma
      
C
      RETURN
      END
