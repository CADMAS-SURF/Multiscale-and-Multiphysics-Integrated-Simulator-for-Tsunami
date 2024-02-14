      MODULE MOD_LIST
      IMPLICIT NONE
      include 'mpif.h'
C
      INTEGER,ALLOCATABLE:: LLWALL(:,:),LLWALP(:,:)
      INTEGER,ALLOCATABLE:: LLWALB(:,:),LLOFL(:,:)
      REAL(8),ALLOCATABLE:: HHOFL(:)
C
C
C-----------------------------------------------------------------------
      CONTAINS
C-----------------------------------------------------------------------
      SUBROUTINE ALLOC_LIST(MLWALL,MLWALP,LP)
      INTEGER,INTENT(IN):: MLWALL,MLWALP,LP
C
      INTEGER:: MLWAL1,MLWAL2,IERR,ICODE
C
      MLWAL1 = MAX(MLWALL,1)
      MLWAL2 = MAX(MLWALP,1)

      ALLOCATE(LLWALL(8,MLWAL1),LLWALP(8,MLWAL2),STAT=IERR)
C
      IF(IERR.NE.0) THEN
         CALL ERRMSG('ALLOC_LIST',7080)
         WRITE(LP,*) 'CANNOT ALLOCATE LLWALL,...'
         CALL ABORT1('')
      ENDIF
C
c      write(LP,*) 'mlwall=',mlwall
c      write(LP,*) 'mlwalp=',mlwalp
C
      LLWALL(:,:)=0
      LLWALP(:,:)=0
C
      RETURN
      END SUBROUTINE ALLOC_LIST
C
C
      SUBROUTINE ALLOC_LIST2(MLWALB,MLOFL,LP)
      INTEGER,INTENT(IN):: MLWALB,MLOFL,LP
C
      INTEGER:: MLWAL3,MLOFL1,IERR,ICODE
C
      MLWAL3 = MAX(MLWALB,1)
      MLOFL1 = MAX(MLOFL ,1)
C
      ALLOCATE(LLWALB(3,MLWAL3),LLOFL(3,MLOFL1),
     $         HHOFL(MLOFL1),STAT=IERR)
C
      IF(IERR.NE.0) THEN
         CALL ERRMSG('ALLOC_LIST2',7081)
         WRITE(LP,*) 'CANNOT ALLOCATE LLWALB,...'
         CALL ABORT1('')
      END IF
C
c      write(LP,*) 'mlwalb=',mlwalb
c      write(LP,*) 'mlofl =',mlofl
C
      LLWALB(:,:)=0
      LLOFL(:,:)=0
      HHOFL(:)=0.0D0
C
      RETURN
      END SUBROUTINE ALLOC_LIST2
C
C
      SUBROUTINE DEALLOC_LIST(I)
      INTEGER,INTENT(IN)::I
C
      IF(I.EQ.0) THEN
      DEALLOCATE(LLWALL)
      DEALLOCATE(LLWALP)
      ELSEIF(I.EQ.1) THEN
      DEALLOCATE(LLWALB)
      DEALLOCATE( LLOFL)
      DEALLOCATE( HHOFL)
      ENDIF
C
      RETURN
      END SUBROUTINE DEALLOC_LIST
C
C
      SUBROUTINE COUNT_MLWAL(MLWALL,MLWALL1,MLWALP,MX,MY,MZ,
     $                       INDU,INDV,INDW,LSTOCDS)
C
      INTEGER,INTENT(OUT)::MLWALL,MLWALL1,MLWALP
      INTEGER,INTENT(IN)::MX,MY,MZ,LSTOCDS
      INTEGER,INTENT(IN)::INDU(MX,MY,MZ)
      INTEGER,INTENT(IN)::INDV(MX,MY,MZ)
      INTEGER,INTENT(IN)::INDW(MX,MY,MZ)
C
      INTEGER:: I,J,K
C
      MLWALL=0
      MLWALP=0
      DO K=2,MZ-1
      DO J=2,MY-1
      DO I=1,MX-1
         IF( INDU(I,J,K).EQ.-2 ) MLWALL = MLWALL + 1
         IF( INDU(I,J,K).EQ.-3 ) MLWALP = MLWALP + 1
      ENDDO
      ENDDO
      ENDDO
C
      DO K=2,MZ-1
      DO J=1,MY-1
      DO I=2,MX-1
         IF( INDV(I,J,K).EQ.-2 ) MLWALL = MLWALL + 1
         IF( INDV(I,J,K).EQ.-3 ) MLWALP = MLWALP + 1
      ENDDO
      ENDDO
      ENDDO
C
      DO K=1,MZ-1
      DO J=2,MY-1
      DO I=2,MX-1
         IF( INDW(I,J,K).EQ.-2 ) MLWALL = MLWALL + 1
         IF( INDW(I,J,K).EQ.-3 ) MLWALP = MLWALP + 1
      ENDDO
      ENDDO
      ENDDO
C
      MLWALL1=MLWALL
      IF( LSTOCDS.EQ.1 ) THEN
         MLWALL =MLWALL*10
      ENDIF
C
      RETURN
      END SUBROUTINE COUNT_MLWAL
C
C
      END MODULE MOD_LIST
