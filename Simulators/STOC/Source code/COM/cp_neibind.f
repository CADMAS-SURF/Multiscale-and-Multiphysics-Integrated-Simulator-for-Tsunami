      SUBROUTINE CP_NEIBIND(INDP,INDU,INDV,INDW,KF,KP,KG,GV,GX,GY,GZ,
     1                      FRIC,HDEP,AMNG)
C----------------------------------------------------------------------
C     時間ループ中に変化する変数を領域間で通信する
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
C
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),INDU(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDV(MX,MY,MZ),INDW(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KP(MX,MY),KG(MX,MY)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GX(MX,MY,MZ),GY(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GZ(MX,MY,MZ),FRIC(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HDEP(MX,MY),AMNG(MX,MY)
C
      REAL(8),ALLOCATABLE::BF1(:,:,:),BF2(:,:)
      INTEGER::L,IERR,N=1
C
C
      ALLOCATE(BF1(MX,MY,MZ),BF2(MX,MY),STAT=IERR)
      IF(IERR.NE.0) THEN
         CALL ERRMSG('CP_NEIBIND',6280)
        WRITE(LP,*) 'CANNOT ALLOCATE BF1,BF2'
        CALL ABORT1('')
      ENDIF
C----------------------------------------------------------------------
C     (1) INTEGER
C----------------------------------------------------------------------
      L=0
      BF1=DBLE(INDP)
      CALL CP_DSR_DC2(MX,MY,MZ,L,N,BF1)
      INDP=NINT(BF1)
C
      L=1
      BF1=DBLE(INDU)
      CALL CP_DSR_DC2(MX,MY,MZ,L,N,BF1)
      INDU=NINT(BF1)
C
      L=2
      BF1=DBLE(INDV)
      CALL CP_DSR_DC2(MX,MY,MZ,L,N,BF1)
      INDV=NINT(BF1)
C
      L=3
      BF1=DBLE(INDW)
      CALL CP_DSR_DC2(MX,MY,MZ,L,N,BF1)
      INDW=NINT(BF1)
C
      L=0
      BF2=DBLE(KG)
      CALL CP_DSR_DC2(MX,MY,N,L,N,BF2)
      KG=NINT(BF2)
C
      L=0
      BF2=DBLE(KF)
      CALL CP_DSR_DC2(MX,MY,N,L,N,BF2)
      KF=NINT(BF2)
C
      L=0
      BF2=DBLE(KP)
      CALL CP_DSR_DC2(MX,MY,N,L,N,BF2)
      KP=NINT(BF2)
C----------------------------------------------------------------------
C     (2) REAL
C----------------------------------------------------------------------
      L=0
      CALL CP_DSR_DC2(MX,MY,MZ,L,N,GV)
C
      L=1
      CALL CP_DSR_DC2(MX,MY,MZ,L,N,GX)
C
      L=2
      CALL CP_DSR_DC2(MX,MY,MZ,L,N,GY)
C
      L=3
      CALL CP_DSR_DC2(MX,MY,MZ,L,N,GZ)
C
      L=0
      CALL CP_DSR_DC2(MX,MY,MZ,L,N,FRIC)
C
      L=0
      CALL CP_DSR_DC2(MX,MY,N,L,N,HDEP)
C
      L=0
      CALL CP_DSR_DC2(MX,MY,N,L,N,AMNG)
C
      DEALLOCATE(BF1,BF2)
      RETURN
      END
