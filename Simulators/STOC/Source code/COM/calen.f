C-----------------------------------------------------------
      SUBROUTINE CALEN(IEND)
C-----------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'TYPHOI.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'FILE.h'
C
      INTEGER,INTENT(INOUT)::IEND
C
      INTEGER::ND1(12),ND2(12)
C
      DATA ND1 /31,28,31,30,31,30,31,31,30,31,30,31/
      DATA ND2 /31,29,31,30,31,30,31,31,30,31,30,31/
C
      REAL(8)::RESI
      INTEGER::ND
C
C-----------------------------------------------------------
C
      IEND = 1
      RESI = MOD(TIME,60.0D0)
cccccccccccccccccccccc
      write(6,*) 'calen istep,time,resi=',istep,time,resi
cccccccccccccccccccccc
      IF(RESI.LT.0.5D0*DT.OR.60.0D0-RESI.LT.0.5D0*DT) THEN
        NS5 = NS5+1
        IF(NS5.EQ.60) THEN
          NS5=0
          NS4=NS4+1
          IF(NS4.LT.24) THEN
            NS4=0
            NS3=NS3+1
            ND=ND1(NS2)
            IF(MOD(NS1,4).EQ.0) ND=ND2(NS2)
            IF(NS3.LE.ND) THEN
              NS3=1
              NS2=NS2+1
              IF(NS2.LE.12) THEN
                NS2=1
                NS1=NS1+1
              END IF
            END IF
          END IF
        END IF
C
        WRITE(LP,600) ISTEP,NS1,NS2,NS3,NS4,NS5
  600   FORMAT('# ISTEP =',I10,'     NS1,NS2,NS3,NS4,NS5 =',5I10)
      END IF
C
      IF(NS1.NE.NE1) IEND=0
      IF(NS2.NE.NE2) IEND=0
      IF(NS3.NE.NE3) IEND=0
      IF(NS4.NE.NE4) IEND=0
      IF(NS5.NE.NE5) IEND=0
cccccccccccccccccccccc
      write(6,1) ns1,ns2,ns3,ns4,ns5
 1    format('ns1,ns2,ns3,ns4,ns5=',5i5)
      write(6,2) ne1,ne2,ne3,ne4,ne5
 2    format('ne1,ne2,ne3,ne4,ne5=',5i5)
      write(6,*) 'iend',iend
cccccccccccccccccccccc
C
C-----------------------------------------------------------
      RETURN
      END
