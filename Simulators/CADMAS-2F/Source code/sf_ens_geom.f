      SUBROUTINE SF_ENS_GEOM(XX,YY,ZZ)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)

      CHARACTER*1 IP
      CHARACTER*12 FLD
!-----------------------------------------------------------------------
      IF( NPROCS == 1 ) THEN
        FLD = 'cadmas_out'
        N = 10
      ELSE
        WRITE(IP,'(I1)') MYRANK
        FLD = 'cadmas_'//IP//'_out'
        N = 12
      ENDIF

 !    CALL MK_DIR(FLD)

      OPEN(40, FILE = FLD(1:N)//'/data.geom' )

      WRITE(40,'(A)') 'CADMAS-SURF/3D-2F'
      WRITE(40,'(A)') 'EnSight Gold geometry file'

      WRITE(40,'(A)') 'part'
      WRITE(40,'(I10)') 1
      WRITE(40,'(A)') '3D strucured mesh'
      WRITE(40,'(A)') 'block rectilinear'

      WRITE(40,'(3I10)') MYIE-MYIS+2, MYJE-MYJS+2, NUMK-1
      WRITE(40,'(1PE12.5)') XX(1,MYIS:MYIE+1)
      WRITE(40,'(1PE12.5)') YY(1,MYJS:MYJE+1)
      WRITE(40,'(1PE12.5)') ZZ(1,2:NUMK)

      CLOSE(40)

      END
