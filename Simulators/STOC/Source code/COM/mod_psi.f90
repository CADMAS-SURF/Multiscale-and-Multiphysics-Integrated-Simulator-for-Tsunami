Module mod_psi
      Implicit None
      Real (8), Allocatable :: PSI (:,:)
      Real (8), Allocatable :: DHDX2 (:, :)
      Integer, Allocatable :: INDEXPSI (:, :)
      Real (8) :: ALPHA,BETA

!
Contains
      Subroutine init_psi(MX,MY,DISPBETA)
      integer,intent(in):: MX,MY
      real(8),intent(in):: DISPBETA
      INTEGER :: I
!
         Allocate (PSI(MX,MY))
         Allocate (INDEXPSI(MX, MY))
         Allocate (DHDX2(MX, MY))
         PSI    = 1.d0
         INDEXPSI    = 0
         BETA = DISPBETA
         ALPHA       = 1.d0/3.d0  + BETA
         DHDX2 = 0.d0
!
      End Subroutine
!
      Subroutine fin_psi
!
         Deallocate (PSI)
         Deallocate (INDEXPSI)
         Deallocate (DHDX2)
!
      End Subroutine
!
End Module mod_psi
