      SUBROUTINE ST_FORCE( IFC, NFC, FC, NIFC, NNFC, J_FRC, R_FRC,
     &                     N_FRC, ICRDR, INDGR )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION IFC(2,NIFC), NFC(2,NNFC), FC(6,NNFC), J_FRC(3,N_FRC),
     &          R_FRC(6,N_FRC), INDGR(*), ICRDR(*)
C
      IP = 0
C
      DO I = 1, NIFC
        DO J = 1, N_FRC
          IF( J_FRC(1,J) == IFC(1,I) ) THEN
            IP = IP + 1
            NFC(1,IP) = J_FRC(2,J)
            NFC(2,IP) = J_FRC(3,J)
            FC(:,IP) = R_FRC(:,J)
          ENDIF
        ENDDO
        IFC(2,I) = IP
      ENDDO
C
      DO I = 1, NNFC
        NFC(1,I) = INDGR( NFC(1,I) )
        IF( NFC(2,I) > 0 ) NFC(2,I) = ICRDR( NFC(2,I) )
      ENDDO
C
      END