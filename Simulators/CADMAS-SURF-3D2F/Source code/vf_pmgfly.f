      SUBROUTINE VF_PMGFLY(FLF,VDT,FF,BCF,YY,GGV,FLOWER,NF,INDY
     &                    ,I,J,K,NUMI,NUMJ,NUMK,MAXG1,NUMB)
C
      IMPLICIT NONE
C
      INTEGER,INTENT(IN) :: NUMI,NUMJ,NUMK,MAXG1,NUMB
      REAL(8),INTENT(OUT) :: FLF
      REAL(8),INTENT(IN) :: VDT
      REAL(8),INTENT(IN) :: FF(NUMI,NUMJ,NUMK)
      REAL(8),INTENT(IN) :: BCF(NUMB)
      REAL(8),INTENT(IN) :: YY(MAXG1,NUMJ)
      REAL(8),INTENT(IN) :: GGV(NUMI,NUMJ,NUMK)
      REAL(8),INTENT(IN) :: FLOWER
      INTEGER,INTENT(IN) :: NF(NUMI,NUMJ,NUMK)
      INTEGER,INTENT(IN) :: INDY(NUMI,NUMJ,NUMK)
      INTEGER,INTENT(IN) :: I,J,K
C
      REAL(8) :: FA,FD,FDMW,FAD,FDM
      REAL(8) :: V
      REAL(8) :: CFX,FVX,FVM
      INTEGER :: LSN,LA,LD,LDM
      INTEGER :: N
C
      IF(VDT.GE.0.0D0) THEN
        LSN = +1
        LA  = J
        LD  = J-1
        LDM = J-2
      ELSE
        LSN = -1
        LA  = J-1
        LD  = J
        LDM = J+1
      END IF
C
      N = NF(I,LD,K)
      IF(N.NE.-1) THEN
        FD = FF(I,LD,K)
        FDMW = 1.0D0
        IF(NF(I,LDM,K).NE.-1) FDMW = FF(I,LDM,K)
        V = YY(2,LD)*GGV(I,LD,K)
      ELSE
        FD = BCF(INDY(I,J,K))
        FDMW = 1.0D0
        V = YY(2,LD)
      END IF
      IF(NF(I,LA,K).NE.-1) THEN
        FA = FF(I,LA,K)
      ELSE
        FA = FD
      END IF
      FAD = FA
      IF(N.EQ.-1 .OR. N.EQ.1 .OR. N.EQ.2
     &           .OR. N.EQ.5 .OR. N.EQ.6) THEN
        IF(FDMW.NE.0.0D0 .AND. FA.NE.0.0D0) FAD = FD
      END IF
      FDM = MAX(FDMW,FD)
      IF(FDMW.LT.FLOWER .AND. FA.LT.FLOWER) FDM=MAX(FDM,0.1D0)
      CFX = MAX((FDM-FAD)*ABS(VDT)-(FDM-FD)*V,0.0D0)
      FVX = FAD*ABS(VDT) + CFX
      FVM = FD*V
      FLF = DBLE(LSN)*MIN(FVX,FVM)/VDT
C
      RETURN
      END
