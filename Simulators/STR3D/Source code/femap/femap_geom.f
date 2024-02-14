      SUBROUTINE FEMAP_GEOM(KK,IFL)

      USE M_VAL

      DIMENSION KK(*)

!     --- HEADER ---

      CALL BLK_S(100,IFL)

      WRITE(IFL,'(A)') 'ASTEA MECHANICAL'

      WRITE(IFL,'(A)') '9.31,'

      CALL BLK_E(IFL)

!     --- NODES ---

      CALL BLK_S(403,IFL)

      DO I = 1, KK(8)
        CALL FMP_NODE(INDG(I),GRID(1,I),IFL)
      ENDDO

      CALL BLK_E(IFL)

!     --- ELEMENTS ---

      CALL BLK_S(404,IFL)

      DO I = 1, KK(12)
        CALL FMP_ELEM(KK(11),KK(15),IELM(1,I),IELM(2,I),IELM(4,I)
     &               ,IELM(5,I),IELM(3,I),IELM(8,I),INDG,IFL)
      ENDDO

      CALL BLK_E(IFL)

!     --- PROPERTIES ---

      CALL BLK_S(402,IFL)

      DO I = 1, KK(11)
        CALL FMP_PROP(I,25,IFL)
      ENDDO

      DO I = KK(11) + 1, KK(11) + KK(15) + KK(17)
        CALL FMP_PROP(I,1,IFL)
      ENDDO

      CALL BLK_E(IFL)

!     --- MATERIALS ---

      CALL BLK_S(601,IFL)

      DO I = 1, KK(11) + KK(15) + KK(17)
        CALL FMP_MAT(I,IFL)
      ENDDO

      CALL BLK_E(IFL)

      END
