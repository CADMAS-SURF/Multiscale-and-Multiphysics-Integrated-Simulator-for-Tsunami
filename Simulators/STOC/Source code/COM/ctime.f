C-----------------------------------------------------------
      SUBROUTINE CTIME1(N1,N2,N3,N4,M1,M2,M3,M4,KHUR)
C-----------------------------------------------------------
C     オリジナルの処理のまま
C
      IMPLICIT NONE
C
      INTEGER,INTENT(INOUT)::N1,N2,N3,N4,M1,M2,M3,M4,KHUR
C
      INTEGER::ND(12),ND1(12),ND2(12)
C
      DATA ND1 /  0, 31, 59, 90,120,151,
     &          181,212,243,273,304,334/
      DATA ND2 /  0, 31, 60, 91,121,152,
     &          182,213,244,274,305,335/
C
      INTEGER::KDAY,KYAR,MDAY,MHUR,N,NDAY,NHUR
C
C-----------------------------------------------------------
      IF(MOD(N1,4).NE.0)THEN
        DO 110 N=1,12
          ND(N)=ND1(N)
  110   CONTINUE
      ELSE
        DO 120 N=1,12
          ND(N)=ND2(N)
  120   CONTINUE
      ENDIF
C
      NDAY=ND(N2)+N3
      NHUR=24*NDAY+N4
C
      IF(MOD(M1,4).NE.0)THEN
        DO 1110 N=1,12
          ND(N)=ND1(N)
 1110   CONTINUE
      ELSE
        DO 1120 N=1,12
          ND(N)=ND2(N)
 1120   CONTINUE
      ENDIF
C
      MDAY=ND(M2)+M3
      MHUR=24*MDAY+M4
C
      KDAY=0
      KYAR=M1-1
  130 CONTINUE
      KYAR=KYAR+1
      IF(KYAR.EQ.N1)GOTO 140
C
      IF(MOD(KYAR,4).NE.0)THEN
        KDAY=KDAY+365
      ELSE
        KDAY=KDAY+366
      ENDIF
      GOTO 130
C
  140 CONTINUE
      KHUR=KDAY*24+(NHUR-MHUR)
C----------------------------------------------------------
      RETURN
      END
