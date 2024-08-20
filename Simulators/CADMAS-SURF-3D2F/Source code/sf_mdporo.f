      SUBROUTINE SF_MDPORO(GGV,GGX,GGY,GGZ,GGW,GGR,SPC,LNDC0,LNDC,KST,XX
     &                    ,YY,ZZ,GGV0,NF,SUMZ,IELM,GRID,POR,MODE,VV,SSX
     &                    ,SSY,SSZ,WW,RR,IFLG,EV,ESX,ESY,ESZ,EW,ER,DBUF)

      USE SF_TYPE

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'SF_STRUCT.h'

      TYPE(SPACE) :: SPC(NELM)

      DIMENSION VV(NUMI,NUMJ,NUMK),SSX(NUMI,NUMJ,NUMK)
     &         ,SSY(NUMI,NUMJ,NUMK),SSZ(NUMI,NUMJ,NUMK)
     &         ,WW(NUMI,NUMJ,NUMK),RR(NUMI,NUMJ,NUMK)
     &         ,XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
     &         ,NF(NUMI,NUMJ,NUMK),GGV0(NUMI,NUMJ,NUMK)
     &         ,GGV(NUMI,NUMJ,NUMK),GGX(NUMI,NUMJ,NUMK)
     &         ,GGY(NUMI,NUMJ,NUMK),GGZ(NUMI,NUMJ,NUMK)
     &         ,GGW(NUMI,NUMJ,NUMK),GGR(NUMI,NUMJ,NUMK)
     &         ,IELM(24,NELM),GRID(3,NNOD),P(3,4,24),XG(2),YG(2),ZG(2)
     &         ,DBUF(NUMBUF*MAXBUF),POR(NELM),IFLG(NUMI,NUMJ,NUMK)
     &         ,EV(NUMI,NUMJ,NUMK),ESX(NUMI,NUMJ,NUMK)
     &         ,ESY(NUMI,NUMJ,NUMK),ESZ(NUMI,NUMJ,NUMK)
     &         ,EW(NUMI,NUMJ,NUMK),ER(NUMI,NUMJ,NUMK),LNDC0(NUMI,NUMJ)
     &         ,LNDC(NUMI,NUMJ),KST(NUMI,NUMJ),SUMZ(NUMI,NUMJ)

      INTEGER, POINTER :: LNDH(:,:,:)
      REAL(8), POINTER :: GGF(:,:,:,:),GGFW(:,:,:)
!-----------------------------------------------------------------------
      EPS = DMAX1( XX(1,NUMI) - XX(1,2), YY(1,NUMJ) - YY(1,2),
     &             ZZ(1,NUMK) - ZZ(1,2) ) * 1.D-12

      VV(:,:,:)  = 0.
      SSX(:,:,:) = 0.
      SSY(:,:,:) = 0.
      SSZ(:,:,:) = 0.
      WW(:,:,:)  = 0.
      RR(:,:,:)  = 0.

      IF( ISTM == 1 ) THEN

        ALLOCATE( LNDH(NUMI,NUMJ,0:2) )
      
        LNDH(:,:,:) = 0

        DO I = MYIS, MYIE
          DO J = MYJS, MYJE
            DO K = NUMK - 1, 1, -1
              IF( NF(I,J,K) == -1 .OR. GGV(I,J,K) < 1.D0 ) THEN
                LNDH(I,J,1) = K
                EXIT
              ENDIF
            ENDDO
          ENDDO
        ENDDO

      ENDIF

      DO IEL = 1, NELM
      
        LTYP = IELM(3,IEL)

        IF( SPC(IEL)%IFIX == 1 ) THEN

          DO IC = 1, SPC(IEL)%N

            I = SPC(IEL)%ID(1,IC)
            J = SPC(IEL)%ID(2,IC)
            K = SPC(IEL)%ID(3,IC)

            VV(I,J,K)  = VV(I,J,K)  + SPC(IEL)%P(1,IC)
            SSX(I,J,K) = SSX(I,J,K) + SPC(IEL)%P(2,IC)
            SSY(I,J,K) = SSY(I,J,K) + SPC(IEL)%P(3,IC)
            SSZ(I,J,K) = SSZ(I,J,K) + SPC(IEL)%P(4,IC)
            WW(I,J,K)  = WW(I,J,K)  + SPC(IEL)%P(5,IC)
            RR(I,J,K)  = RR(I,J,K)  + SPC(IEL)%P(6,IC)
     
            IF( ISTM == 1 .AND. SPC(IEL)%P(1,IC) > 0 .AND. LTYP > 0 )
     &        LNDH(I,J,LTYP) = MAX0( K, LNDH(I,J,LTYP) )

          ENDDO

          CYCLE

        ELSE

          IF( ALLOCATED( SPC(IEL)%ID ) ) DEALLOCATE( SPC(IEL)%ID )
          IF( ALLOCATED( SPC(IEL)%P ) ) DEALLOCATE( SPC(IEL)%P )

        ENDIF

        CALL SF_ELEM_RANGE(ISKIP,IS,IE,JS,JE,KS,KE,XX,YY,ZZ,EPS
     &                   ,IELM(4,IEL),IELM(5,IEL),GRID)

        IF( ISKIP == 1 ) THEN
          SPC(IEL)%N = 0
          CYCLE
        ENDIF

        CALL SF_TETDIV(NT,P,IELM(4,IEL),IELM(5,IEL),GRID)

        FAC = 1.D0 - POR(IEL)

        IC = 0

        IFLG(IS:IE,JS:JE,KS:KE) = 0

        EV(IS:IE,JS:JE,KS:KE)  = 0.
        ESX(IS:IE,JS:JE,KS:KE) = 0.
        ESY(IS:IE,JS:JE,KS:KE) = 0.
        ESZ(IS:IE,JS:JE,KS:KE) = 0.
        EW(IS:IE,JS:JE,KS:KE)  = 0.
        ER(IS:IE,JS:JE,KS:KE)  = 0.

        DO IT = 1, NT

          DO K = KS, KE

            ZG(1) = ZZ(1,K)

            IF( K == NUMK ) THEN
              ZG(2) = ZZ(1,K) + ZZ(2,K)
            ELSE
              ZG(2) = ZZ(1,K+1)
            ENDIF

            DO J = JS, JE

              YG(1) = YY(1,J)

              IF( J == NUMJ ) THEN
                YG(2) = YY(1,J) + YY(2,J)
              ELSE
                YG(2) = YY(1,J+1)
              ENDIF

              DO I = IS, IE

                XG(1) = XX(1,I)

                IF( I == NUMI ) THEN
                  XG(2) = XX(1,I) + XX(2,I)
                ELSE
                  XG(2) = XX(1,I+1)
                ENDIF

                CALL SF_CUT3(V,SX,SY,SZ,P(1,1,IT),XG,YG,ZG,EPS)

                IF( DMAX1(V,SX,SY,SZ) > 0.D0 ) THEN

                  IF( IFLG(I,J,K) == 0 ) THEN
                    IFLG(I,J,K) = 1
                    IC = IC + 1
                  ENDIF

                  EV(I,J,K)  = EV(I,J,K)  + V * FAC
                  ESX(I,J,K) = ESX(I,J,K) + SX * FAC
                  ESY(I,J,K) = ESY(I,J,K) + SY * FAC
                  ESZ(I,J,K) = ESZ(I,J,K) + SZ * FAC
                  EW(I,J,K)  = EW(I,J,K)  + V
                  IF( LTYP == 0 ) ER(I,J,K) = ER(I,J,K) + V

                ENDIF

              ENDDO

            ENDDO

          ENDDO

        ENDDO

        N = IC

        SPC(IEL)%N = N

        IF( N == 0 ) CYCLE

        ALLOCATE( SPC(IEL)%ID(3,N) )
        ALLOCATE( SPC(IEL)%P(6,N) )

        IC = 0

        DO K = KS, KE
          DO J = JS, JE
            DO I = IS, IE

              IF( IFLG(I,J,K) == 1 ) THEN

                IC = IC + 1

                SPC(IEL)%ID(1,IC) = I
                SPC(IEL)%ID(2,IC) = J
                SPC(IEL)%ID(3,IC) = K

                SPC(IEL)%P(1,IC) = EV(I,J,K)
                SPC(IEL)%P(2,IC) = ESX(I,J,K)
                SPC(IEL)%P(3,IC) = ESY(I,J,K)
                SPC(IEL)%P(4,IC) = ESZ(I,J,K)
                SPC(IEL)%P(5,IC) = EW(I,J,K)
                SPC(IEL)%P(6,IC) = ER(I,J,K)

                VV(I,J,K)  = VV(I,J,K)  + EV(I,J,K)
                SSX(I,J,K) = SSX(I,J,K) + ESX(I,J,K)
                SSY(I,J,K) = SSY(I,J,K) + ESY(I,J,K)
                SSZ(I,J,K) = SSZ(I,J,K) + ESZ(I,J,K)
                WW(I,J,K)  = WW(I,J,K)  + EW(I,J,K)
                RR(I,J,K)  = RR(I,J,K)  + ER(I,J,K)

                IF( ISTM == 1 .AND. SPC(IEL)%P(1,IC) > 0 .AND. LTYP > 0)
     &            LNDH(I,J,LTYP) = MAX0( K, LNDH(I,J,LTYP) )

              ENDIF

            ENDDO
          ENDDO
        ENDDO

      ENDDO

      IF( MYMIE == 1 ) THEN
        IP = 1
      ELSE
        IP = 0
      ENDIF

      IF( MYMJE == 1 ) THEN
        JP = 1
      ELSE
        JP = 0
      ENDIF

      IF( ISTM == 1 .AND. MODE == 2 ) THEN

        ALLOCATE( GGF(6,NUMI,NUMJ,NUMK) )

        GGF(:,:,:,:) = 1.D0

        DO I = MYIS, MYIE
          DO J = MYJS, MYJE
            IF( LNDC0(I,J) == 1 ) THEN
              H = SUMZ(I,J)
              DO K = KST(I,J), NUMK - 1
                IF( H <= 0.D0 ) EXIT
                CALL SF_FILL(H,GGW(I,J,K),GGV(I,J,K),GGF(1,I,J,K)
     &                      ,ZZ(2,K),PORS)
              ENDDO
            ENDIF
          ENDDO
        ENDDO

        ALLOCATE( GGFW(NUMI,NUMJ,NUMK) )

        DO I = 1, 6
          GGFW(:,:,:) = GGF(I,:,:,:)
          CALL VF_P3SRD1(GGFW,DBUF,0)
          GGF(I,:,:,:) = GGFW(:,:,:)
        ENDDO

        DEALLOCATE( GGFW )

        DO K = 2, NUMK - 1
          DO J = MYJS, MYJE
            DO I = MYIS, MYIE + IP
              IF( GGF(2,I-1,J,K) == 1.D0 .AND. GGF(1,I,J,K) == 1.D0 )
     &          CYCLE
              GGX(I,J,K) = DMIN1(GGF(2,I-1,J,K),GGF(1,I,J,K),GGX(I,J,K))
            ENDDO
          ENDDO
        ENDDO

        DO K = 2, NUMK - 1
          DO J = MYJS, MYJE + JP
            DO I = MYIS, MYIE
              IF( GGF(4,I,J-1,K) == 1.D0 .AND. GGF(3,I,J,K) == 1.D0 )
     &          CYCLE
              GGY(I,J,K) = DMIN1(GGF(4,I,J-1,K),GGF(3,I,J,K),GGY(I,J,K))
            ENDDO
          ENDDO
        ENDDO

        DO K = 2, NUMK
          DO J = MYJS, MYJE
            DO I = MYIS, MYIE
              IF( GGF(6,I,J,K-1) == 1.D0 .AND. GGF(5,I,J,K) == 1.D0 )
     &          CYCLE
              GGZ(I,J,K) = DMIN1(GGF(6,I,J,K-1),GGF(5,I,J,K),GGZ(I,J,K))
            ENDDO
          ENDDO
        ENDDO

        DEALLOCATE( GGF )

      ENDIF

      DO K = 2, NUMK - 1
        DO J = MYJS, MYJE
          DO I = MYIS, MYIE
            IF( VV(I,J,K) == 0. ) CYCLE
            VV0 = XX(2,I) * YY(2,J) * ZZ(2,K)
            GGV(I,J,K) = ( 1.D0 - VV(I,J,K) / VV0 ) * GGV(I,J,K)
            IF( GGV(I,J,K) < 0. ) GGV(I,J,K) = 0.
          ENDDO
        ENDDO
      ENDDO

      DO K = 2, NUMK - 1
        DO J = MYJS, MYJE
          DO I = MYIS, MYIE + IP
            IF( SSX(I,J,K) == 0. ) CYCLE
            SSX0 = YY(2,J) * ZZ(2,K)
            GGX(I,J,K) = ( 1.D0 - SSX(I,J,K) / SSX0 ) * GGX(I,J,K)
            IF( GGX(I,J,K) < 0. ) GGX(I,J,K) = 0.
          ENDDO
        ENDDO
      ENDDO

      DO K = 2, NUMK - 1
        DO J = MYJS, MYJE + JP
          DO I = MYIS, MYIE
            IF( SSY(I,J,K) == 0. ) CYCLE
            SSY0 = XX(2,I) * ZZ(2,K)
            GGY(I,J,K) = ( 1.D0 - SSY(I,J,K) / SSY0 ) * GGY(I,J,K)
            IF( GGY(I,J,K) < 0. ) GGY(I,J,K) = 0.
          ENDDO
        ENDDO
      ENDDO

      DO K = 2, NUMK
        DO J = MYJS, MYJE
          DO I = MYIS, MYIE
            IF( SSZ(I,J,K) == 0. ) CYCLE
            SSZ0 = XX(2,I) * YY(2,J)
            GGZ(I,J,K) = ( 1.D0 - SSZ(I,J,K) / SSZ0 ) * GGZ(I,J,K)
            IF( GGZ(I,J,K) < 0. ) GGZ(I,J,K) = 0.
          ENDDO
        ENDDO
      ENDDO

      DO K = 2, NUMK - 1
        DO J = MYJS, MYJE
          DO I = MYIS, MYIE
            IF( GGV0(I,J,K) < 1.D0 ) THEN
              GGW(I,J,K) = 0.
            ELSEIF( WW(I,J,K) > 0. ) THEN
              VV0 = XX(2,I) * YY(2,J) * ZZ(2,K)
              GGW(I,J,K) = ( 1.D0 - WW(I,J,K) / VV0 ) * GGW(I,J,K)
              IF( GGW(I,J,K) < 0. ) GGW(I,J,K) = 0.
            ENDIF
          ENDDO
        ENDDO
      ENDDO

      DO K = 2, NUMK - 1
        DO J = MYJS, MYJE
          DO I = MYIS, MYIE
            VV0 = XX(2,I) * YY(2,J) * ZZ(2,K)
            GGR(I,J,K) = RR(I,J,K) / VV0
          ENDDO
        ENDDO
      ENDDO

      IF( ISTM == 1 ) THEN

        DO I = MYIS, MYIE
          DO J = MYJS, MYJE
            DO K = NUMK - 1, 2, -1
              IF( GGR(I,J,K) > .5D0 ) THEN
                LNDH(I,J,0) = K
                EXIT
              ENDIF
            ENDDO
          ENDDO
        ENDDO

        IF( MODE == 0 ) THEN
          CALL SF_MDLNDH(LNDH(1,1,2),XX,YY,ZZ,IELM,GRID)
          DO I = MYIS, MYIE
            DO J = MYJS, MYJE
              IF( LNDH(I,J,2) >= LNDH(I,J,1) ) THEN
                LNDC0(I,J) = 2
              ELSE
                LNDC0(I,J) = 1
              ENDIF
            ENDDO
          ENDDO
        ENDIF

        LNDC(:,:) = LNDC0(:,:)

        DO I = MYIS, MYIE
          DO J = MYJS, MYJE
            IF( LNDH(I,J,0) >= LNDH(I,J,1) .AND.
     &          LNDH(I,J,0) >= LNDH(I,J,2) ) LNDC(I,J) = 0
          ENDDO
        ENDDO

        DEALLOCATE( LNDH )

        IF( MODE == 0 ) THEN
          DO I = MYIS, MYIE
            DO J = MYJS, MYJE
              DO K = NUMK - 1, 1, -1
                IF( NF(I,J,K) == -1 .OR. GGV0(I,J,K) < 1.D0 .OR. 
     &              GGW(I,J,K) + GGR(I,J,K) < 1.D-1 ) THEN
                  KST(I,J) = K + 1
                  EXIT
                ENDIF
              ENDDO
            ENDDO
          ENDDO
        ENDIF

      ENDIF

      CALL VF_P3SRD2(GGV,DBUF,0)
      CALL VF_P3SRD2(GGX,DBUF,1)
      CALL VF_P3SRD2(GGY,DBUF,2)
      CALL VF_P3SRD2(GGZ,DBUF,3)
      CALL VF_P3SRD2(GGW,DBUF,0)
      CALL VF_P3SRD2(GGR,DBUF,0)

      END