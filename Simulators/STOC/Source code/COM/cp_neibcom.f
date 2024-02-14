      SUBROUTINE CP_NEIBCOM(UU,VV,WW,PP,RHOW,TT,CC,CSEDI,ZBED,
     1                      AK,EP,TMU,WX,WY,CD,PATM,HH,KF,KP,IFLAG)
C----------------------------------------------------------------------
C     XXXX
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
C
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::PP(MX,MY,MZ),RHOW(MX,MY,MZ),TT(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::CC(MX,MY,MZ),CSEDI(MX,MY,MZ),ZBED(MX,MY)
      REAL(8),INTENT(INOUT)::AK(MX,MY,MZ),EP(MX,MY,MZ),TMU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::WX(MX,MY),WY(MX,MY),CD(MX,MY)
      REAL(8),INTENT(INOUT)::PATM(MX,MY),HH(MX,MY)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KP(MX,MY)
      INTEGER,INTENT(IN)::IFLAG
C
      REAL(8)::BF2(MX,MY)
      INTEGER::L,N=1
C----------------------------------------------------------------------
C     (1) IFLAG = 1.OR.0.OR.4
C----------------------------------------------------------------------
      IF(IFLAG.EQ.1.OR.IFLAG.EQ.0.OR.IFLAG.EQ.4) THEN
C
      L=1
      CALL CP_DSR_DC2(MX,MY,MZ,L,N,UU)
C
      L=2
      CALL CP_DSR_DC2(MX,MY,MZ,L,N,VV)
C
      L=3
      CALL CP_DSR_DC2(MX,MY,MZ,L,N,WW)
C
      END IF
C----------------------------------------------------------------------
C     (2) IFLAG = 2.OR.0
C----------------------------------------------------------------------
      IF(IFLAG.EQ.2.OR.IFLAG.EQ.0) THEN
C
      L=3
      CALL CP_DSR_DC2(MX,MY,MZ,L,N,WW)
C
      L=0
      CALL CP_DSR_DC2(MX,MY,N,L,N,WX)
      CALL CP_DSR_DC2(MX,MY,N,L,N,WY)
      CALL CP_DSR_DC2(MX,MY,N,L,N,CD)
      CALL CP_DSR_DC2(MX,MY,N,L,N,PATM)
      CALL CP_DSR_DC2(MX,MY,N,L,N,HH)
C
      BF2=DBLE(KF)
      CALL CP_DSR_DC2(MX,MY,N,L,N,BF2)
      KF=NINT(BF2)
C
      BF2=DBLE(KP)
      CALL CP_DSR_DC2(MX,MY,N,L,N,BF2)
      KP=NINT(BF2)
C
      END IF
C----------------------------------------------------------------------
C     (3) IFLAG = 3.OR.0
C----------------------------------------------------------------------
      IF(IFLAG.EQ.3.OR.IFLAG.EQ.0) THEN
C
      L=0
      CALL CP_DSR_DC2(MX,MY,MZ,L,N,PP)
      CALL CP_DSR_DC2(MX,MY,MZ,L,N,RHOW)
      CALL CP_DSR_DC2(MX,MY,MZ,L,N,TMU)
      IF(LTEMP.EQ.1) CALL CP_DSR_DC2(MX,MY,MZ,L,N,TT)
      IF(LCONC.EQ.1) CALL CP_DSR_DC2(MX,MY,MZ,L,N,CC)
      IF(LSEDI.EQ.1)THEN
        CALL CP_DSR_DC2(MX,MY,MZ,L,N,CSEDI)
        CALL CP_DSR_DC2(MX,MY,N,L,N,ZBED)
      ENDIF
C ... LTURB=2(AK=AK,EP=EP),LTURB=3(AK=Q2,EP=QL),LTURB=4(AK=AK)
      IF(LTURB.EQ.2.OR.LTURB.EQ.3) THEN
        CALL CP_DSR_DC2(MX,MY,MZ,L,N,AK)
        CALL CP_DSR_DC2(MX,MY,MZ,L,N,EP)
      END IF
      IF(LTURB.EQ.4) THEN
        CALL CP_DSR_DC2(MX,MY,MZ,L,N,AK)
      END IF
C
      END IF
C
      RETURN
      END
