      SUBROUTINE SF_ALLOC1()

      USE VF_A2ARRAY

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'

      IERR = 0
      DEALLOCATE(BCU,STAT=IERR)
      DEALLOCATE(BCV,STAT=IERR)
      DEALLOCATE(BCW,STAT=IERR)
      DEALLOCATE(BCP,STAT=IERR)
      DEALLOCATE(BCF,STAT=IERR)
      DEALLOCATE(BCVI,STAT=IERR)
      IF (LEQK.NE.0) THEN
        DEALLOCATE(BCK,STAT=IERR)
        DEALLOCATE(BCE,STAT=IERR)
      ENDIF
      IF (LEQT.NE.0) THEN
        DEALLOCATE(BCT,STAT=IERR)
        DEALLOCATE(BCTI,STAT=IERR)
      ENDIF
      IF (LEQC.GT.0) THEN
        DEALLOCATE(BCC,STAT=IERR)
        DEALLOCATE(BCCI,STAT=IERR)
      ENDIF
      DEALLOCATE(WKBC,STAT=IERR)
      DEALLOCATE(INDB,STAT=IERR)
      IF (LEQK.NE.0) THEN
        DEALLOCATE(INDBK,STAT=IERR)
        DEALLOCATE(INDBE,STAT=IERR)
      ENDIF
      IF (LEQT.NE.0) THEN
        DEALLOCATE(INDBT,STAT=IERR)
      ENDIF
      IF (LEQC.GT.0) THEN
        DEALLOCATE(INDBC,STAT=IERR)
      ENDIF
      DEALLOCATE(NWKBC,STAT=IERR)
      IF (IERR.NE.0) CALL VF_A2ERR('SF_ALLOC1','CAN NOT DEALLOC.')

      IERR = 0
      ALLOCATE(BCU  (NUMB,3    ),STAT=IERR)
      ALLOCATE(BCV  (NUMB,3    ),STAT=IERR)
      ALLOCATE(BCW  (NUMB,3    ),STAT=IERR)
      ALLOCATE(BCP  (NUMB,3    ),STAT=IERR)
      ALLOCATE(BCF  (NUMB      ),STAT=IERR)
      ALLOCATE(BCVI (NUMB      ),STAT=IERR)
      IF (LEQK.NE.0) THEN
        ALLOCATE(BCK (  NUMB,3 ),STAT=IERR)
        ALLOCATE(BCE (  NUMB,3 ),STAT=IERR)
      ENDIF
      IF (LEQT.NE.0) THEN
        ALLOCATE(BCT (  NUMB   ),STAT=IERR)
        ALLOCATE(BCTI(2,NUMB   ),STAT=IERR)
      ENDIF
      IF (LEQC.GT.0) THEN
        ALLOCATE(BCC (  NUMB,LEQC),STAT=IERR)
        ALLOCATE(BCCI(2,NUMB,LEQC),STAT=IERR)
      ENDIF
      ALLOCATE(WKBC (NUMB*3    ),STAT=IERR)
      ALLOCATE(INDB (MAXB1,NUMB),STAT=IERR)
      IF (LEQK.NE.0) THEN
        ALLOCATE(INDBK(MAXBK1,NUMB),STAT=IERR)
        ALLOCATE(INDBE(MAXBE1,NUMB),STAT=IERR)
      ENDIF
      IF (LEQT.NE.0) THEN
        ALLOCATE(INDBT(NUMB     ),STAT=IERR)
      ENDIF
      IF (LEQC.GT.0) THEN
        ALLOCATE(INDBC(NUMB,LEQC),STAT=IERR)
      ENDIF
      ALLOCATE(NWKBC(NUMB      ),STAT=IERR)
      IF (IERR.NE.0) CALL VF_A2ERR('SF_ALLOC1','CAN NOT ALLOC.')
      CALL VF_ZSETR2(BCU  ,0.0D0,NUMB,3)
      CALL VF_ZSETR2(BCV  ,0.0D0,NUMB,3)
      CALL VF_ZSETR2(BCW  ,0.0D0,NUMB,3)
      CALL VF_ZSETR2(BCP  ,0.0D0,NUMB,3)
      CALL VF_ZSETR1(BCF  ,0.0D0,NUMB)
      CALL VF_ZSETR1(BCVI ,0.0D0,NUMB)
      IF (LEQK.NE.0) THEN
        CALL VF_ZSETR2(BCK ,0.0D0,NUMB,3)
        CALL VF_ZSETR2(BCE ,0.0D0,NUMB,3)
      ENDIF
      IF (LEQT.NE.0) THEN
        CALL VF_ZSETR1(BCT ,0.0D0,  NUMB)
        CALL VF_ZSETR2(BCTI,0.0D0,2,NUMB)
      ENDIF
      DO 110 LC=1,LEQC
        CALL VF_ZSETR1(BCC (  1,LC),0.0D0,  NUMB)
        CALL VF_ZSETR2(BCCI(1,1,LC),0.0D0,2,NUMB)
 110  CONTINUE
      CALL VF_ZSETR1(WKBC ,0.0D0,NUMB*3)
      CALL VF_ZSETI2(INDB ,    0,MAXB1,NUMB)
      IF (LEQK.NE.0) THEN
        CALL VF_ZSETI2(INDBK,  0,MAXBK1,NUMB)
        CALL VF_ZSETI2(INDBE,  0,MAXBE1,NUMB)
      ENDIF
      IF (LEQT.NE.0) THEN
        CALL VF_ZSETI1(INDBT,  0,NUMB)
      ENDIF
      DO 120 LC=1,LEQC
        CALL VF_ZSETI1(INDBC(1,LC),0,NUMB)
 120  CONTINUE
      CALL VF_ZSETI1(NWKBC,    0,NUMB)

      END