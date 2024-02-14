      SUBROUTINE ENS_CASE( KK, IOUT, D_T, IFCAS, FLNAME, NLEN, ICHK )
C
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*256 FLNAME
      CHARACTER*256 FLN
C
      DIMENSION KK(*), IOUT(*), D_T(*)
C
      FLN = 'str_out/'//FLNAME(1:NLEN)
C
      N = 8 + NLEN
C
      OPEN( IFCAS, FILE = 'str.case' )
C
      WRITE(IFCAS,'(A)') 'FORMAT'
C
      WRITE(IFCAS,'(A)') 'type: ensight gold'
C
      WRITE(IFCAS,'(A)') 'GEOMETRY'
C
      WRITE(IFCAS,'(2A)') 'model: ', FLN(1:N)//'.geom'
C
      WRITE(IFCAS,'(A)') 'VARIABLE'
C
      WRITE(IFCAS,'(2A)') 'vector per node: displacement ', 
     &                    FLN(1:N)//'.disp_****'
C
      WRITE(IFCAS,'(2A)') 'vector per node: velocity ', 
     &                    FLN(1:N)//'.vel_****'
C
      WRITE(IFCAS,'(2A)') 'vector per node: acceleration ', 
     &                    FLN(1:N)//'.acc_****'
C
      WRITE(IFCAS,'(2A)') 'vector per node: reactive_force ', 
     &                    FLN(1:N)//'.rfc_****'
C
      IF( KK(25) > 0 )
     &  WRITE(IFCAS,'(2A)') 'scalar per node: pore_water_pressure ', 
     &                      FLN(1:N)//'.pprs_****'
C
      WRITE(IFCAS,'(2A)') 'tensor symm per node: solid_stress ', 
     &                    FLN(1:N)//'.sols_****'
C
      WRITE(IFCAS,'(2A)') 'tensor symm per node: solid_strain ', 
     &                    FLN(1:N)//'.sole_****'
C
      WRITE(IFCAS,'(2A)') 'vector per node: solid_principal_stress ', 
     &                    FLN(1:N)//'.solps_****'
C
      IF( KK(25) > 0 )
     &  WRITE(IFCAS,'(2A)') 'vector per node: pore_water_flux ', 
     &                      FLN(1:N)//'.flux_****'
C
      IF( ICHK == 2 ) THEN
C
        IF( KK(81) > 0 .OR. KK(92) > 0 )
     &    WRITE(IFCAS,'(2A)') 'vector per node: displacement_w ', 
     &                        FLN(1:N)//'.dispw_****'
C
        IF( KK(81) > 0 )
     &    WRITE(IFCAS,'(2A)') 'scalar per node: surface_pressure ', 
     &                        FLN(1:N)//'.surfp_****'
C
        IF( KK(81) > 0 )
     &    WRITE(IFCAS,'(2A)') 'scalar per element: surface_area ', 
     &                        FLN(1:N)//'.surfa_****'
C
      ENDIF
C
      WRITE(IFCAS,'(A)') 'TIME'
C
      WRITE(IFCAS,'(A)') 'time set: 1'
C
      NOUT = 1
      DO I = 1, KK(7)
        IF( IOUT(I) == 1 ) NOUT = NOUT + 1
      ENDDO
C
      WRITE(IFCAS,'(A,I3)') 'number of steps: ', NOUT
C
      WRITE(IFCAS,'(A)') 'filename start number: 0000'
C
      WRITE(IFCAS,'(A)') 'filename increment: 1'
C
      WRITE(IFCAS,'(A)') 'time values:'
C
      TIME = 0.
      WRITE(IFCAS,'(1PE15.7)') TIME
      DO I = 1, KK(7)
        TIME = TIME + D_T(I)
        IF( IOUT(I) == 1 ) WRITE(IFCAS,'(1PE15.7)') TIME
      ENDDO
C
      CLOSE( IFCAS )
C
      END
