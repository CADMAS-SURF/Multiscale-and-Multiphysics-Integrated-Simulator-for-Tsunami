      SUBROUTINE CONT_TBL_P(KK)

      USE M_VAL

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KK(*),XX(3,3),GX(3)
      INTEGER, POINTER :: INDX(:),NT(:),ICEL2(:,:),ICFC2(:,:),ICED2(:,:)
      INTEGER, POINTER :: INDCR(:),NEL(:),INDCE2(:,:)

      NNOD  = KK( 8)
      NELM  = KK(12)
      NNODI = KK(26)
      NNODC = KK(28)
      NICRG = KK(92)
      NIGSF = KK(94)
      NINDC = KK(95)
      NIELC = KK(97)
      NIEDG = KK(99)
      NIBTE = KK(100)
      NINDC0= KK(102)
      NIGSFC= KK(108)

!     --- IELA ---

      ALLOCATE( IELA(3,NIELC) )

      DO I = 1, NIEDG

        IE1 = IEDG(3,I)
        K1  = IEDG(4,I)
        IE2 = IEDG(5,I)
        K2  = IEDG(6,I)

        IF( IE1 > 0 ) IELA(K1,IE1) = I
        IF( IE2 > 0 ) IELA(K2,IE2) = I

      ENDDO

!     --- ICELA, ICEL ---

      ALLOCATE( INDX(NNOD+NNODC+NIGSF+NIGSFC) )

      INDX(:) = 0

      DO I = 1, NIELC
        INDX(IELC(:,I)) = 1
      ENDDO

      IP = 0

      DO I = 1, NNOD + NNODC + NIGSF
        IF( INDX(I) == 1 ) THEN
          IP = IP + 1
          INDX(I) = IP
        ENDIF
      ENDDO

      NINDX = IP

      ALLOCATE( NT(NINDX) )

      NT(:) = 0

      DO I = 1, NIBTE
        DO J = 1, 4
          K = INDX(IBTE(J,I))
          IF( K > 0 ) NT(K) = NT(K) + 1
        ENDDO
      ENDDO

      ALLOCATE( ICEL2(MAXVAL(NT),NINDX) )

      NT(:) = 0

      DO I = 1, NIBTE
        DO J = 1, 4
          K = INDX(IBTE(J,I))
          IF( K > 0 ) THEN
            NT(K) = NT(K) + 1
            ICEL2(NT(K),K) = I
          ENDIF
        ENDDO
      ENDDO

      ALLOCATE( ICELA(2,NNOD+NNODC+NIGSF) )
      ALLOCATE( ICEL(SUM(NT)) )

      IE = 0

      DO I = 1, NNOD + NNODC + NIGSF
        J = INDX(I)
        IF( J > 0 ) THEN
          IS = IE + 1
          IE = IE + NT(J)
          ICELA(1,I) = IS
          ICELA(2,I) = IE
          ICEL(IS:IE) = ICEL2(1:NT(J),J)
        ENDIF
      ENDDO
      
      DEALLOCATE( ICEL2 )

!     --- ICFCA, ICFC ---

      NT(:) = 0

      DO I = 1, NIELC
        DO J = 1, 3
          K = INDX(IELC(J,I))
          NT(K) = NT(K) + 1
        ENDDO
      ENDDO

      ALLOCATE( ICFC2(MAXVAL(NT),NINDX) )

      NT(:) = 0

      DO I = 1, NIELC
        DO J = 1, 3
          K = INDX(IELC(J,I))
          NT(K) = NT(K) + 1
          ICFC2(NT(K),K) = I
        ENDDO
      ENDDO

      ALLOCATE( ICFCA(2,NNOD+NNODC+NIGSF) )
      ALLOCATE( ICFC(SUM(NT)) )

      IE = 0

      DO I = 1, NNOD + NNODC + NIGSF
        J = INDX(I)
        IF( J > 0 ) THEN
          IS = IE + 1
          IE = IE + NT(J)
          ICFCA(1,I) = IS
          ICFCA(2,I) = IE
          ICFC(IS:IE) = ICFC2(1:NT(J),J)
        ENDIF
      ENDDO

      DEALLOCATE( ICFC2 )

!     --- ICEDA, ICED ---

      NT(:) = 0

      DO I = 1, NIEDG
        DO J = 1, 2
          K = INDX(IEDG(J,I))
          IF( K > 0 ) NT(K) = NT(K) + 1
        ENDDO
      ENDDO

      ALLOCATE( ICED2(MAXVAL(NT),NINDX) )

      NT(:) = 0

      DO I = 1, NIEDG
        DO J = 1, 2
          K = INDX(IEDG(J,I))
          IF( K > 0 ) THEN
            NT(K) = NT(K) + 1
            ICED2(NT(K),K) = I
          ENDIF
        ENDDO
      ENDDO

      ALLOCATE( ICEDA(2,NNOD+NNODC+NIGSF) )
      ALLOCATE( ICED(SUM(NT)) )

      IE = 0

      DO I = 1, NNOD + NNODC + NIGSF
        J = INDX(I)
        IF( J > 0 ) THEN
          IS = IE + 1
          IE = IE + NT(J)
          ICEDA(1,I) = IS
          ICEDA(2,I) = IE
          ICED(IS:IE) = ICED2(1:NT(J),J)
        ENDIF
      ENDDO
      
      DEALLOCATE( INDX )
      DEALLOCATE( NT )

      DEALLOCATE( ICED2 )

!     --- INDCEA, INDCE ---

      ALLOCATE( INDCR(NNOD) )

      INDCR(:) = 0

      DO I = 1, NINDC
        J = INDC(I)
        IF( J <= NNODI ) INDCR(J) = I
      ENDDO

      ALLOCATE( NEL(NINDC) )

      NEL(:) = 0

      DO I = 1, NELM
        N = IELM(3,I)
        DO J = 1, N
          K = INDCR( IELM(7+J,I) )
          IF( K > 0 ) NEL(K) = NEL(K) + 1
        ENDDO
      ENDDO

      ALLOCATE( INDCE2(MAXVAL(NEL),NINDC) )

      NEL(:) = 0

      DO I = 1, NELM
        N = IELM(3,I)
        DO J = 1, N
          K = INDCR( IELM(7+J,I) )
          IF( K > 0 ) THEN
            NEL(K) = NEL(K) + 1
            INDCE2(NEL(K),K) = I
          ENDIF
        ENDDO
      ENDDO

      DEALLOCATE( INDCR )

      ALLOCATE( INDCEA(2,NINDC) )
      ALLOCATE( INDCE(SUM(NEL)) )

      IE = 0

      DO I = 1, NINDC
        N = NEL(I)
        IF( N > 0 ) THEN
          IS = IE + 1
          IE = IE + N
          INDCEA(1,I) = IS
          INDCEA(2,I) = IE
          INDCE(IS:IE) = INDCE2(1:N,I)
        ENDIF
      ENDDO
      
      DEALLOCATE( NEL )

      DEALLOCATE( INDCE2 )

!     --- GELC ---

      ALLOCATE( GELC(NIELC) )

      DO I = 1, NIELC

        XX(:,1:2) = GRID(:,IELC(1:2,I))

        IG3 = IELC(3,I)
        IQU = IVRQ(IG3)
        IF( IQU == 0 ) THEN
          XX(:,3) = GRID(:,IG3)
        ELSE
          CALL MEAN4(XX(1,3),GRID,3,IELQ(1,IQU),4)
        ENDIF

        GX(:) = ( XX(:,1) + XX(:,2) + XX(:,3) ) / 3.D0

        DIST_MAX = 0.
        DO J = 1, 3
          CALL LENGTH(DIST,XX(1,J),GX,3)
          DIST_MAX = DMAX1( DIST_MAX, DIST )
        ENDDO

        GELC(I) = DIST_MAX

      ENDDO

!     --- ICTB ---

      ALLOCATE( ICTB(NICRG,NICRG) )

!     --- ISLV0, ISLVP, RSLV0, PSLV, ISLV, RSLV, IRANK ---

      ALLOCATE( ISLV0(2,NINDC0) )
      ALLOCATE( ISLVP(NINDC0) )
      ALLOCATE( RSLV0(3,NINDC0) )
      ALLOCATE( PSLV(3,4,NINDC0) )

      ALLOCATE( ISLV(2,NINDC) )
      ALLOCATE( RSLV(3,NINDC) )
      ALLOCATE( IRANK(NINDC) )

!     --- FRTB, U0, RL0, IFRIC, FRIC, ISTICK ---

      ALLOCATE( FRTB(3,NICRG,NICRG) )
      ALLOCATE( U0(3,4,NINDC) )
      ALLOCATE( RL0(3,NINDC) )
      ALLOCATE( IFRIC(10,NINDC) )
      ALLOCATE( FRIC(10,NINDC) )
      ALLOCATE( ISTICK(NINDC0) )

      END
