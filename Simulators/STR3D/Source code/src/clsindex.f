      SUBROUTINE CLSINDEX(KK,IFMTX1,IFMTX2)
C
      USE M_VAL
      USE MPC_WORK
      USE M_MUMPS
C
      REAL*8 DDUM
      DIMENSION KK(*)
C----&------------------------------------------------------------------
      IF( KK(87) > 0 ) THEN
        DEALLOCATE(IDEP)
        DEALLOCATE(CIDP)
        DEALLOCATE(IDX)
        DEALLOCATE(IRH)
        DEALLOCATE(IFRH)
        DEALLOCATE(CRH)
        DEALLOCATE(IDL)
        DEALLOCATE(ROWL)
        DEALLOCATE(COLL)
        DEALLOCATE(ROWR)
        DEALLOCATE(COLR)
        DEALLOCATE(B)
        DEALLOCATE(MPCF)
        DEALLOCATE(RMPC)
      ENDIF
C
      DEALLOCATE(RHV)
      DEALLOCATE(X)
C
      DEALLOCATE(IDSK)
      DEALLOCATE(IDCG)
C
      SELECT CASE( KK(21) )
      CASE( 1, 11 )
C
        DEALLOCATE(CGWK)
        DEALLOCATE(STF)
        IF( KK(21) == 1 ) DEALLOCATE(LOW)
C
      CASE( 2 )
C
        DEALLOCATE(IASEWK)
        DEALLOCATE(ASEWK)
        DEALLOCATE(STF)
        DEALLOCATE(SYM)
        DEALLOCATE(NUM)
C
        CLOSE(IFMTX1,STATUS='DELETE')
        CLOSE(IFMTX2,STATUS='DELETE')
C
      CASE( 3, 13 )
C
        DEALLOCATE( STF )
C
        IF( KK(21) == 3 ) THEN
          MTYPE = -2
        ELSE
          MTYPE = 11
        ENDIF
C
!        CALL PARDISO(PT,1,1,MTYPE,-1,KK(19),DDUM,IDUM,IDUM,IDUM,1,IPARM
!     &              ,0,DDUM,DDUM,IERR)
C
      CASE( 4, 14 )
C
        DEALLOCATE( MUMPS_PAR%IRN )
        DEALLOCATE( STF )
C
        MUMPS_PAR%JOB = -2
C
        CALL M_MPI_BCAST_I(MUMPS_PAR%JOB,1)
C
        CALL DMUMPS(MUMPS_PAR)
C
      END SELECT
C
      END
