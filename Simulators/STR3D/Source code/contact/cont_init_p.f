      SUBROUTINE CONT_INIT_P(ISLV0,ISLVP,RSLV0,PSLV,ISTICK,POS,POSO,KK
     &                      ,NBDY,GRID,IELM,NM,IBEL,INDA0,ICBD,IELC
     &                      ,IELCB,INDA,INDC,IEDA,IEDG,IEDGB,ICTB,EPS)

      USE MPI_PARAM

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KK(*),ISLV0(2,*),ISLVP(*),RSLV0(3,*),PSLV(3,4,*)
     &         ,INDA0(NBDY),INDA(NBDY),INDC(*),IEDA(NBDY),IEDG(6,*)
     &         ,IEDGB(*),ICBD(NBDY),IELC(3,*),IELCB(*),ICTB(NBDY,NBDY)
     &         ,POS(3,*),POSS(3),GRID(3,*),IELM(NM,*),IBEL(*),POSO(3,*)
     &         ,ISTICK(*)

      IF( MYRANK == 1 ) CALL M_MPI_SEND_I(22,1,0)  ! SEND IOP=22 TO GLB_COMM

      NNOD = KK(8)
      NELM = KK(12)
      NNODI = KK(26)
      NNODC = KK(28)
      NELMC = KK(29)
      NIGSF = KK(94)
      NINDC0 = KK(102)
      NIGSFC = KK(108)

      POS(:,1:NNOD) = GRID(:,1:NNOD)

      CALL M_MPI_SEND_D(POS,3*NNODI,0)

      IF( NNODC > 0 ) CALL M_MPI_RECV_D(POS(1,NNOD+1),3*NNODC,0)

      CALL GSURF(POS(1,NNOD+NNODC+1),POS,IELM,NM,NELM+NELMC,IBEL,3)

      POSO(:,1:NNOD+NNODC+NIGSF+NIGSFC)=POS(:,1:NNOD+NNODC+NIGSF+NIGSFC)

      CALL M_MPI_BCAST_D(PSLV,12*NINDC0)

      ISLV0(:,1:NINDC0) = 0
      ISLVP(1:NINDC0) = 0
      RSLV0(:,1:NINDC0) = 0.

      DO IBDY = 1, NBDY

        CALL ADDSET4(IS,IE,INDA0,IBDY)

        DO I = IS, IE

          POSS(:) = PSLV(:,2,I)

          DO JBDY = 1, NBDY

            IF( ICTB(IBDY,JBDY) == 0 ) CYCLE

            CALL ADDSET4(JS,JE,INDA,JBDY)

            DO J = JS, JE

              IF( INDC(J) > NNODI ) CYCLE

              CALL VRTXSTK(ISLV0(1,I),POSS,INDC(J),POS,EPS)

              IF( ISLV0(1,I) > 0 ) GOTO 10

            ENDDO

            CALL ADDSET4(JS,JE,IEDA,JBDY)

            DO J = JS, JE

              IF( IEDGB(J) == 0 ) CYCLE

              CALL EDGESTK(ISLV0(1,I),RSLV0(1,I),POSS,J,IEDG(1,J),POS
     &                    ,EPS)

              IF( ISLV0(1,I) > 0 ) GOTO 10

            ENDDO

            CALL ADDSET4(JS,JE,ICBD,JBDY)

            DO J = JS, JE

              IF( IELCB(J) == 0 ) CYCLE

              CALL FACESTK(ISLV0(1,I),RSLV0(1,I),POSS,J,IELC(1,J),POS
     &                    ,EPS)

              IF( ISLV0(1,I) > 0 ) GOTO 10

            ENDDO

          ENDDO

   10     IF( ISLV0(1,I) > 0 ) ISLVP(I) = MYRANK

        ENDDO

      ENDDO

      CALL M_MPI_SEND_I(ISLV0,2*NINDC0,0)
      CALL M_MPI_SEND_I(ISLVP,NINDC0,0)
      CALL M_MPI_SEND_D(RSLV0,3*NINDC0,0)

      CALL M_MPI_BCAST_I(ISLV0,2*NINDC0)
      CALL M_MPI_BCAST_I(ISLVP,NINDC0)
      CALL M_MPI_BCAST_D(RSLV0,3*NINDC0)

      ISTICK(1:NINDC0) = 1

      END
