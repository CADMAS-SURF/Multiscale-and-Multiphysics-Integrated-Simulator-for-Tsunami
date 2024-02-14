      SUBROUTINE CONT_INIT(ISLV,RSLV,POS,POSO,EDML,IFRIC,KK,NBDY,ICBD
     &                    ,IELC,INDA,INDC,IEDA,IEDG,IELQ,IFCQ,IEDQ,IVRQ
     &                    ,ICTB,GRID,IELM,IBEL,ISLVG,EPS)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KK(*),ISLV(2,*),RSLV(3,*),INDA(NBDY),INDC(*)
     &         ,ICTB(NBDY,NBDY),GRID(3,*),POS(3,*),POSO(3,*),IELM(*)
     &         ,IBEL(*),EDML(NBDY),IEDA(NBDY),IEDG(6,*),IFRIC(10,*)
     &         ,ICBD(NBDY),IELC(3,*),ISLVG(3,*),IELQ(4,*),IFCQ(*)
     &         ,IEDQ(*),IVRQ(*)

      NNOD  = KK(8)
      NELM  = KK(12)
      NIGSF = KK(94)
      NINDC = KK(95)

      POS(:,1:NNOD) = GRID(:,1:NNOD)

      CALL GSURF(POS(1,NNOD+1),POS,IELM,KK(37),NELM,IBEL,3)

      POSO(:,1:NNOD+NIGSF) = POS(:,1:NNOD+NIGSF)

      ISLV(:,1:NINDC) = 0
      RSLV(:,1:NINDC) = 0.

      DO IBDY = 1, NBDY

        CALL ADDSET4(IS,IE,INDA,IBDY)

        DO I = IS, IE

          IND = INDC(I)

          DO JBDY = 1, NBDY

            IF( ICTB(IBDY,JBDY) == 0 ) CYCLE

            CALL ADDSET4(JS,JE,INDA,JBDY)

            DO J = JS, JE

              CALL VRTXSTK(ISLV(1,I),POS(1,IND),INDC(J),POS,EPS)

              IF( ISLV(1,I) > 0 ) GOTO 10

            ENDDO

            CALL ADDSET4(JS,JE,IEDA,JBDY)

            DO J = JS, JE

              CALL EDGESTK(ISLV(1,I),RSLV(1,I),POS(1,IND),J,IEDG(1,J)
     &                    ,POS,EPS)

              IF( ISLV(1,I) > 0 ) GOTO 10

            ENDDO

            CALL ADDSET4(JS,JE,ICBD,JBDY)

            DO J = JS, JE

              CALL FACESTK(ISLV(1,I),RSLV(1,I),POS(1,IND),J,IELC(1,J)
     &                    ,POS,EPS)

              IF( ISLV(1,I) > 0 ) GOTO 10

            ENDDO

          ENDDO

   10   ENDDO

      ENDDO

      CALL CIRCHK(ISLV,ISLVG,NNOD,NINDC,INDC,IEDG,IELC,IELQ,IFCQ,IEDQ
     &           ,IVRQ)

      DO IBDY = 1, NBDY
        CALL ADDSET4(IS,IE,IEDA,IBDY)
        EDML(IBDY) = 0.
        DO J = IS, IE
          CALL LENGTH(DIST,POS(1,IEDG(1,J)),POS(1,IEDG(2,J)),3)
          EDML(IBDY) = EDML(IBDY) + DIST
        ENDDO
        EDML(IBDY) = EDML(IBDY) / ( IE - IS + 1 )
      ENDDO

      IFRIC(:,1:NINDC) = 0
      IFRIC(3,1:NINDC) = 1

      END
