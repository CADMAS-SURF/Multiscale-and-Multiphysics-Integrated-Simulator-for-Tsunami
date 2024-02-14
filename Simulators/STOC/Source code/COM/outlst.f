      SUBROUTINE OUTLST(UU,VV,WW,PP,TT,CC,AK,EP,TMU,FF,RHOW,HH,HDEP,
     $                  SHLSD,CSEDI,CSDAVE,ZBED,ZBED0,
     $                  WEXSD,QBX,QBY,EXSDE,EXSDD,INDP,KF,KP,KG)
C======================================================================
C     リスト出力を行う
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
      INCLUDE 'OUTPUT.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'MODELI.h'
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
      INTEGER,INTENT(INOUT)::KF(MX,MY),KP(MX,MY),KG(MX,MY)
C
      INTEGER::IDIR1,IFIX1,M,N,I,J,K,L,LPX,NLSECTX,IERR
      REAL(8),ALLOCATABLE::RL(:,:,:)
      REAL(8),ALLOCATABLE::DZBED(:,:),CSEDIOUT(:,:,:)
C
C
      ALLOCATE(RL(MX,MY,MZ),DZBED(MX,MY),CSEDIOUT(MX,MY,MZ),STAT=IERR)
      IF(IERR.NE.0)THEN
         CALL ERRMSG('OUTLST',7130)
         WRITE(LP,*) 'CANNOT ALLOCATE RL,...'
         CALL ABORT1('')
      ENDIF
C
C
      LPX=LP
      NLSECTX=NLSECT
      IF(LISTT.EQ.1) THEN
         LPX=IFLLP
         NLSECTX=1
      ENDIF
C
      DO 100 N=1,MLIST
C
         IF( CLIST(N).EQ.'L       '.AND.LTURB.EQ.3 ) THEN
            DO 184 K=1,MZ
            DO 184 J=1,MY
            DO 184 I=1,MX
              IF(AK(I,J,K).NE.0.0D0) THEN
                RL(I,J,K) = 0.5D0*EP(I,J,K)/AK(I,J,K)
              ELSE
                RL(I,J,K) = 0.0D0
              END IF
  184       CONTINUE          
         ELSE IF( CLIST(N).EQ.'CSEDI   ' ) THEN
            DO 605 K=1,MZ
            DO 605 J=1,MY
            DO 605 I=1,MX
               IF(INDP(I,J,K).EQ.1)THEN
                 IF(K.LE.KF(I,J))THEN
                   CSEDIOUT(I,J,K)=CSEDI(I,J,K)
                 ELSE
                   CSEDIOUT(I,J,K)=0.0D0
                 ENDIF
               ELSE
                 CSEDIOUT(I,J,K)=-1.0D0
               ENDIF
  605       CONTINUE
         ELSE IF( CLIST(N).EQ.'DZBED   ' ) THEN
            DO 610 J=1,MY
            DO 610 I=1,MX
               DZBED(I,J)=ZBED(I,J)-ZBED0(I,J)
  610       CONTINUE
         ENDIF
C
C
         WRITE(LP,1000) CLIST(N),ISTEP,TIME
         IF(LISTT.EQ.1) WRITE(LPX) CLIST(N),ISTEP,REAL(TIME)
C
         DO 110 M=1,NLSECTX
            IDIR1 = ILSECT(1,M)
            IFIX1 = ILSECT(2,M)
C
         IF( CLIST(N).EQ.'U       ' ) THEN
            CALL DBWR2D(UU,IDIR1,IFIX1,MX,MY,MZ,LPX)
C
         ELSE IF( CLIST(N).EQ.'V       ' ) THEN
            CALL DBWR2D(VV,IDIR1,IFIX1,MX,MY,MZ,LPX)
C
         ELSE IF( CLIST(N).EQ.'W       ' ) THEN
            CALL DBWR2D(WW,IDIR1,IFIX1,MX,MY,MZ,LPX)
C
         ELSE IF( CLIST(N).EQ.'P       ' ) THEN
            CALL DBWR2D(PP,IDIR1,IFIX1,MX,MY,MZ,LPX)
C
         ELSE IF( CLIST(N).EQ.'T       ' ) THEN
            CALL DBWR2D(TT,IDIR1,IFIX1,MX,MY,MZ,LPX)
C
         ELSE IF( CLIST(N).EQ.'C       ' ) THEN
            CALL DBWR2D(CC,IDIR1,IFIX1,MX,MY,MZ,LPX)
C
         ELSE IF( CLIST(N).EQ.'K       '.AND.LTURB.EQ.2 ) THEN
            CALL DBWR2D(AK,IDIR1,IFIX1,MX,MY,MZ,LPX)
C
         ELSE IF( CLIST(N).EQ.'K       '.AND.LTURB.EQ.3 .OR.
     $            CLIST(N).EQ.'K       '.AND.LTURB.EQ.4 ) THEN
            CALL DBWR2D(AK,IDIR1,IFIX1,MX,MY,MZ,LPX)
C
         ELSE IF( CLIST(N).EQ.'Q2       '.AND.LTURB.EQ.3 ) THEN
            CALL DBWR2D(AK,IDIR1,IFIX1,MX,MY,MZ,LPX)
C
         ELSE IF( CLIST(N).EQ.'E       '.AND.LTURB.EQ.4 ) THEN
            CALL DBWR2D(EP,IDIR1,IFIX1,MX,MY,MZ,LPX)
C
         ELSE IF( CLIST(N).EQ.'L       '.AND.LTURB.EQ.3 ) THEN
            CALL DBWR2D(RL,IDIR1,IFIX1,MX,MY,MZ,LPX)
C
         ELSE IF( CLIST(N).EQ.'TMU     ' ) THEN
            CALL DBWR2D(TMU,IDIR1,IFIX1,MX,MY,MZ,LPX)
C
         ELSE IF( CLIST(N).EQ.'F       ' ) THEN
            CALL DBWR2D(FF,IDIR1,IFIX1,MX,MY,MZ,LPX)
C
         ELSE IF( CLIST(N).EQ.'RHO     ' ) THEN
            CALL DBWR2D(RHOW,IDIR1,IFIX1,MX,MY,MZ,LPX)
C
         ELSE IF( CLIST(N).EQ.'H       ' ) THEN
            CALL DBWR2D(HH,3,1,MX,MY,1,LPX)
            EXIT
C
         ELSE IF( CLIST(N).EQ.'DEP     ' ) THEN
            CALL DBWR2D(HDEP,3,1,MX,MY,1,LPX)
            EXIT
C
         ELSE IF( CLIST(N).EQ.'KF      ' ) THEN
            CALL DBWIXY(KF)
            EXIT
C
         ELSE IF( CLIST(N).EQ.'KP      ' ) THEN
            CALL DBWIXY(KP)
            EXIT
C
         ELSE IF( CLIST(N).EQ.'KG      ' ) THEN
            CALL DBWIXY(KG)
            EXIT
C
         ELSE IF( CLIST(N).EQ.'SHLSD   ' ) THEN
            CALL DBWR2D(SHLSD,3,1,MX,MY,1,LPX)
            EXIT
C
         ELSE IF( CLIST(N).EQ.'WEXSD   ' ) THEN
            CALL DBWR2D(WEXSD,3,1,MX,MY,1,LPX)
            EXIT
C
         ELSE IF( CLIST(N).EQ.'CSEDI   ' ) THEN
            CALL DBWR2D(CSEDIOUT,IDIR1,IFIX1,MX,MY,MZ,LPX)
C
         ELSE IF( CLIST(N).EQ.'CSDAVE  ' ) THEN
            CALL DBWR2D(CSDAVE,3,1,MX,MY,1,LPX)
            EXIT
C
         ELSE IF( CLIST(N).EQ.'ZBED    ' ) THEN
            CALL DBWR2D(ZBED,3,1,MX,MY,1,LPX)
            EXIT
C
         ELSE IF( CLIST(N).EQ.'DZBED   ' ) THEN
            CALL DBWR2D(DZBED,3,1,MX,MY,1,LPX)
            EXIT
C
         ELSE IF( CLIST(N).EQ.'EXSD_ERO' ) THEN
            CALL DBWR2D(EXSDE,3,1,MX,MY,1,LPX)
            EXIT
C
         ELSE IF( CLIST(N).EQ.'EXSD_DEP' ) THEN
            CALL DBWR2D(EXSDD,3,1,MX,MY,1,LPX)
            EXIT
C
         ELSE IF( CLIST(N).EQ.'QBX     ' ) THEN
            CALL DBWR2D(QBX,3,1,MX,MY,1,LPX)
            EXIT
C
         ELSE IF( CLIST(N).EQ.'QBY     ' ) THEN
            CALL DBWR2D(QBY,3,1,MX,MY,1,LPX)
            EXIT
C
         ELSE
            CALL ERRMS2('OUTLST',7131)
            WRITE(LP,*) '   VARIABLE ',CLIST(N),' IS NOT DEFINED'
         END IF
C
         IF( LISTT.EQ.1 ) EXIT
  110 CONTINUE
  100 CONTINUE
C
      DEALLOCATE(RL,DZBED,CSEDIOUT,STAT=IERR)
      RETURN
 1000 FORMAT(/,'# LIST PHS=',A8,' STEP=',I7,' TIME=',1P,E12.5,/)
      END
