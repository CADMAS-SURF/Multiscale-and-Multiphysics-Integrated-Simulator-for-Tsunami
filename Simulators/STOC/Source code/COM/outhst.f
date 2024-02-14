      SUBROUTINE OUTHST(UU,VV,WW,PP,TT,CC,AK,EP,TMU,FF,RHOW,HH,HDEP,
     $   SHLSD,CSEDI,CSDAVE,ZBED,ZBED0,
     $   WEXSD,QBX,QBY,EXSDE,EXSDD,INDP,WX,WY,PATM)
C======================================================================
C     リスト出力を行う
C======================================================================
      use mod_gather,only: rhbuff,lhorder,gatherh
      IMPLICIT NONE
C     
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
      INCLUDE 'OUTPUT.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
C     
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::PP(MX,MY,MZ),TT(MX,MY,MZ),CC(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::AK(MX,MY,MZ),EP(MX,MY,MZ),TMU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::FF(MX,MY,MZ),RHOW(MX,MY,MZ)
      REAL(8),INTENT(IN)::HH(MX,MY),HDEP(MX,MY)
      REAL(8),INTENT(IN)::CSEDI(MX,MY,MZ),CSDAVE(MX,MY)
      REAL(8),INTENT(IN)::ZBED(MX,MY),ZBED0(MX,MY)
      REAL(8),INTENT(IN)::SHLSD(MX,MY),WEXSD(MX,MY)
      REAL(8),INTENT(IN)::EXSDE(MX,MY),EXSDD(MX,MY)
      REAL(8),INTENT(IN)::QBX(MX,MY),QBY(MX,MY)
      INTEGER,INTENT(IN)::INDP(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::WX(MX,MY),WY(MX,MY),PATM(MX,MY)
C     
      REAL(8),ALLOCATABLE::OUTWRK(:)
      INTEGER::II,JJ,KK,KPNT,L,L2,M,N,ISIZ,IERR
      INTEGER(8):: IDUM
      CHARACTER(80):: FORM1
C     
C     
      ISIZ=MAX(MHIST*NHCELLSUM,1)
      ALLOCATE(OUTWRK(ISIZ),STAT=IERR)
      IF(IERR.NE.0)THEN
         CALL ERRMSG('OUTHST',7120)
         WRITE(LP,*) 'CANNOT ALLOCATE OUTWRK'
         CALL ABORT1('')
      ENDIF      
C
      KPNT = 0
C     
      DO N=1,MHIST
C     
         DO M=1,NHCELL
            KPNT = KPNT+1
            II = IHCELL(1,M)
            JJ = IHCELL(2,M)
            KK = IHCELL(3,M)
C     
            IF( CHIST(N).EQ.'U       ' ) THEN
c     tmp            OUTWRK(KPNT) = 0.5D0*(UU(II-1,JJ,KK)+UU(II,JJ,KK))
               OUTWRK(KPNT) = UU(II,JJ,KK)
               IF(DABS(OUTWRK(KPNT)).LE.1.0D-50) OUTWRK(KPNT)=0.0D0
C     
            ELSE IF( CHIST(N).EQ.'V       ' ) THEN
c     tmp            OUTWRK(KPNT) = 0.5D0*(VV(II,JJ-1,KK)+VV(II,JJ,KK))
               OUTWRK(KPNT) = VV(II,JJ,KK)
               IF(DABS(OUTWRK(KPNT)).LE.1.0D-50) OUTWRK(KPNT)=0.0D0
C     
            ELSE IF( CHIST(N).EQ.'W       ' ) THEN
               OUTWRK(KPNT) = 0.5D0*(WW(II,JJ,KK-1)+WW(II,JJ,KK))
               IF(DABS(OUTWRK(KPNT)).LE.1.0D-50) OUTWRK(KPNT)=0.0D0
C     
            ELSE IF( CHIST(N).EQ.'P       ' ) THEN
               OUTWRK(KPNT) = PP(II,JJ,KK)
               IF(DABS(OUTWRK(KPNT)).LE.1.0D-50) OUTWRK(KPNT)=0.0D0
C     
            ELSE IF( CHIST(N).EQ.'T       ' ) THEN
               OUTWRK(KPNT) = TT(II,JJ,KK)
               IF(DABS(OUTWRK(KPNT)).LE.1.0D-50) OUTWRK(KPNT)=0.0D0
C     
            ELSE IF( CHIST(N).EQ.'C       ' ) THEN
               OUTWRK(KPNT) = CC(II,JJ,KK)
               IF(DABS(OUTWRK(KPNT)).LE.1.0D-50) OUTWRK(KPNT)=0.0D0
C     
            ELSE IF( CHIST(N).EQ.'K       '.OR.
     $            CHIST(N).EQ.'Q2      ' ) THEN
               OUTWRK(KPNT) = AK(II,JJ,KK)
               IF(DABS(OUTWRK(KPNT)).LE.1.0D-50) OUTWRK(KPNT)=0.0D0
C     
            ELSE IF( CHIST(N).EQ.'E       '.AND.LTURB.EQ.2 ) THEN
               OUTWRK(KPNT) = EP(II,JJ,KK)
               IF(DABS(OUTWRK(KPNT)).LE.1.0D-50) OUTWRK(KPNT)=0.0D0
C     
            ELSE IF( CHIST(N).EQ.'L       '.AND.LTURB.EQ.3 ) THEN
               IF(AK(II,JJ,KK).NE.0.0D0) THEN
                  OUTWRK(KPNT) = 0.5D0*EP(II,JJ,KK)/AK(II,JJ,KK)
               ELSE
                  OUTWRK(KPNT)= 0.0D0
               END IF
C     
            ELSE IF( CHIST(N).EQ.'TMU     ' ) THEN
               OUTWRK(KPNT) = TMU(II,JJ,KK)
               IF(DABS(OUTWRK(KPNT)).LE.1.0D-50) OUTWRK(KPNT)=0.0D0
C     
            ELSE IF( CHIST(N).EQ.'F       ' ) THEN
               OUTWRK(KPNT) = FF(II,JJ,KK)
               IF(DABS(OUTWRK(KPNT)).LE.1.0D-50) OUTWRK(KPNT)=0.0D0
C     
            ELSE IF( CHIST(N).EQ.'RHO     ' ) THEN
               OUTWRK(KPNT) = RHOW(II,JJ,KK)
               IF(DABS(OUTWRK(KPNT)).LE.1.0D-50) OUTWRK(KPNT)=0.0D0
C     
            ELSE IF( CHIST(N).EQ.'H       ' ) THEN
               OUTWRK(KPNT) = HH(II,JJ)
               IF(DABS(OUTWRK(KPNT)).LE.1.0D-50) OUTWRK(KPNT)=0.0D0
C     
            ELSE IF( CHIST(N).EQ.'DEP     ' ) THEN
               OUTWRK(KPNT) = HDEP(II,JJ)
               IF(DABS(OUTWRK(KPNT)).LE.1.0D-50) OUTWRK(KPNT)=0.0D0
C     
            ELSE IF( CHIST(N).EQ.'WX      ' ) THEN
               OUTWRK(KPNT) = WX(II,JJ)
               IF(DABS(OUTWRK(KPNT)).LE.1.0D-50) OUTWRK(KPNT)=0.0D0
C     
            ELSE IF( CHIST(N).EQ.'WY      ' ) THEN
               OUTWRK(KPNT) = WY(II,JJ)
               IF(DABS(OUTWRK(KPNT)).LE.1.0D-50) OUTWRK(KPNT)=0.0D0
C     
            ELSE IF( CHIST(N).EQ.'PS      ' ) THEN
               OUTWRK(KPNT) = PATM(II,JJ)
               IF(DABS(OUTWRK(KPNT)).LE.1.0D-50) OUTWRK(KPNT)=0.0D0
C     
            ELSE IF( CHIST(N).EQ.'SHLSD   ' ) THEN
               OUTWRK(KPNT) = SHLSD(II,JJ)
               IF(DABS(OUTWRK(KPNT)).LE.1.0D-50) OUTWRK(KPNT)=0.0D0
C     
            ELSE IF( CHIST(N).EQ.'WEXSD   ' ) THEN
               OUTWRK(KPNT) = WEXSD(II,JJ)
               IF(DABS(OUTWRK(KPNT)).LE.1.0D-50) OUTWRK(KPNT)=0.0D0
C     
            ELSE IF( CHIST(N).EQ.'CSEDI   ' ) THEN
               OUTWRK(KPNT) = CSEDI(II,JJ,KK)
               IF(DABS(OUTWRK(KPNT)).LE.1.0D-50) OUTWRK(KPNT)=0.0D0
C     
            ELSE IF( CHIST(N).EQ.'CSDAVE  ' ) THEN
               OUTWRK(KPNT) = CSDAVE(II,JJ)
               IF(DABS(OUTWRK(KPNT)).LE.1.0D-50) OUTWRK(KPNT)=0.0D0
C     
            ELSE IF( CHIST(N).EQ.'ZBED    ' ) THEN
               OUTWRK(KPNT) = ZBED(II,JJ)
               IF(DABS(OUTWRK(KPNT)).LE.1.0D-50) OUTWRK(KPNT)=0.0D0
C     
            ELSE IF( CHIST(N).EQ.'DZBED   ' ) THEN
               OUTWRK(KPNT) = ZBED(II,JJ)-ZBED0(II,JJ)
               IF(DABS(OUTWRK(KPNT)).LE.1.0D-50) OUTWRK(KPNT)=0.0D0
C     
            ELSE IF( CHIST(N).EQ.'EXSD_ERO' ) THEN
               OUTWRK(KPNT) = EXSDE(II,JJ)
               IF(DABS(OUTWRK(KPNT)).LE.1.0D-50) OUTWRK(KPNT)=0.0D0
C     
            ELSE IF( CHIST(N).EQ.'EXSD_DEP' ) THEN
               OUTWRK(KPNT) = EXSDD(II,JJ)
               IF(DABS(OUTWRK(KPNT)).LE.1.0D-50) OUTWRK(KPNT)=0.0D0
C     
            ELSE IF( CHIST(N).EQ.'QBX     ' ) THEN
               OUTWRK(KPNT) = QBX(II,JJ)
               IF(DABS(OUTWRK(KPNT)).LE.1.0D-50) OUTWRK(KPNT)=0.0D0
C     
            ELSE IF( CHIST(N).EQ.'QBY     ' ) THEN
               OUTWRK(KPNT) = QBY(II,JJ)
               IF(DABS(OUTWRK(KPNT)).LE.1.0D-50) OUTWRK(KPNT)=0.0D0
C     
            ELSE
               CALL ERRMS2('OUTHST',7121)
               WRITE(LP,*) '   VARIABLE ',CHIST(N),' IS NOT DEFINED'
            END IF
C     
         ENDDO
C     
         IF(IAUTOD.NE.0) THEN
            KPNT = KPNT-NHCELL
            CALL GATHERH(OUTWRK(KPNT+1),NHCELL,CHILDCOMM)
            IF(MYPROC.EQ.1) THEN
               DO L=1,NHCELLSUM
                  KPNT = KPNT+1
                  OUTWRK(KPNT)=RHBUFF(LHORDER(L))
               ENDDO
            ENDIF
         ENDIF
C
      ENDDO
C
      FORM1='(1P,E13.5,        E13.5)'
C
      IF( KENSAMODE.EQ.1 ) THEN
      FORM1='(1P,E13.5,0P,     F13.5)'
      ENDIF
C
      WRITE(FORM1(14:18),'(I5)') KPNT
      IF((IAUTOD.EQ.0.OR.MYPROC.EQ.1).AND.KPNT.GT.0) THEN
         WRITE(IFLHS,FORM1) TIME,(OUTWRK(L),L=1,KPNT)
      ENDIF
C
  100 CONTINUE
C
      DEALLOCATE(OUTWRK)
      RETURN
      END
