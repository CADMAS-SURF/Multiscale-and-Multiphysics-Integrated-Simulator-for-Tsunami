      SUBROUTINE SLVNRM(SNRM,SNRMW,NNOD,NIGSF,NIELC,NINDC,IELC,INDC,POS)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION IELC(3,NIELC),INDC(NINDC),SNRM(3,NINDC)
     &         ,SNRMW(3,NNOD+NIGSF),POS(3,NNOD+NIGSF),IG(3),V12(3)
     &         ,V13(3),EN(3)

      SNRMW(:,:) = 0.

      DO I = 1, NIELC

        IG(:) = IELC(:,I)

        V12(:) = POS(:,IG(2)) - POS(:,IG(1))
        V13(:) = POS(:,IG(3)) - POS(:,IG(1))

        CALL CROSS2(V13,V12,EN)

        DO J = 1, 3
          SNRMW(:,IG(J)) = SNRMW(:,IG(J)) + EN(:)
        ENDDO

      ENDDO

      DO I = 1, NINDC
        CALL DIRCOS(SNRM(1,I),SNRMW(1,INDC(I)),3)
      ENDDO

      END