      SUBROUTINE VF_PMGFLX(FLF,UDT,FF,BCF,XX,GGV,FLOWER,NF,INDX
     &                    ,I,J,K,NUMI,NUMJ,NUMK,MAXG1,NUMB)
C
      IMPLICIT NONE
C
      INTEGER,INTENT(IN) :: NUMI,NUMJ,NUMK,MAXG1,NUMB
      REAL(8),INTENT(OUT) :: FLF
      REAL(8),INTENT(IN) :: UDT
      REAL(8),INTENT(IN) :: FF(NUMI,NUMJ,NUMK)
      REAL(8),INTENT(IN) :: BCF(NUMB)
      REAL(8),INTENT(IN) :: XX(MAXG1,NUMI)
      REAL(8),INTENT(IN) :: GGV(NUMI,NUMJ,NUMK)
      REAL(8),INTENT(IN) :: FLOWER
      INTEGER,INTENT(IN) :: NF(NUMI,NUMJ,NUMK)
      INTEGER,INTENT(IN) :: INDX(NUMI,NUMJ,NUMK)
      INTEGER,INTENT(IN) :: I,J,K
C
      REAL(8) :: FA,FD,FDMW,FAD,FDM
      REAL(8) :: V
      REAL(8) :: CFX,FVX,FVM
      INTEGER :: LSN,LA,LD,LDM
      INTEGER :: N
C
      IF(UDT.GE.0.0D0) THEN
        LSN = +1
        LA  = I
        LD  = I-1
        LDM = I-2
      ELSE
        LSN = -1
        LA  = I-1
        LD  = I
        LDM = I+1
      END IF
C
      N = NF(LD,J,K)
      IF(N.NE.-1) THEN
        FD = FF(LD,J,K)
        FDMW = 1.0D0
        IF(NF(LDM,J,K).NE.-1) FDMW = FF(LDM,J,K)
        V = XX(2,LD)*GGV(LD,J,K)
      ELSE
        FD = BCF(INDX(I,J,K))
        FDMW = 1.0D0
        V = XX(2,LD)
      END IF
      IF(NF(LA,J,K).NE.-1) THEN
        FA = FF(LA,J,K)
      ELSE
        FA = FD
      END IF
      FAD = FA
      IF(N.EQ.-1 .OR. N.EQ.3 .OR. N.EQ.4
     &           .OR. N.EQ.5 .OR. N.EQ.6) THEN
        IF(FDMW.NE.0.0D0 .AND. FA.NE.0.0D0) FAD = FD
      END IF
      FDM = MAX(FDMW,FD)
      IF(FDMW.LT.FLOWER .AND. FA.LT.FLOWER) FDM=MAX(FDM,0.1D0)
      CFX = MAX((FDM-FAD)*ABS(UDT)-(FDM-FD)*V,0.0D0)
      FVX = FAD*ABS(UDT) + CFX
      FVM = FD*V
      FLF = DBLE(LSN)*MIN(FVX,FVM)/UDT
C
      RETURN
      END
