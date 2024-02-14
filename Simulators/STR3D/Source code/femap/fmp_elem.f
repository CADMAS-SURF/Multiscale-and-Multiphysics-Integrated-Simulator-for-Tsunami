      SUBROUTINE FMP_ELEM(NMAT,NRODA,ID,ITYPE,IDM,IDS,NP,IP0,IG,IFL)

      CHARACTER*200 CHAR
      CHARACTER*1 C /','/
      DIMENSION IP0(NP),IP(NP),JP(20),IG(*)

      IP(:) = IG( IP0(:) )

      JP(:) = 0

      SELECT CASE( ITYPE )
      CASE( 2, 6 )

        IDP = IDM

        SELECT CASE( NP )
        CASE( 4 )
          ITYP = 25
          ITOP = 6
          JP(1:3)   = IP(1:3)
          JP(5)     = IP(4)
        CASE( 10 )
          ITYP = 26
          ITOP = 10
          JP(1:3)   = IP(1:3)
          JP(5)     = IP(4)
          JP(9:11)  = IP(5:7)
          JP(13:15) = IP(8:10)
        CASE( 6 )
          ITYP = 25
          ITOP = 7
          JP(1:3)   = IP(1:3)
          JP(5:7)   = IP(4:6)
        CASE( 15 )
          ITYP = 26
          ITOP = 11
          JP(1:3)   = IP(1:3)
          JP(5:7)   = IP(4:6)
          JP(9:11)  = IP(7:9)
          JP(13:15) = IP(10:12)
          JP(17:19) = IP(13:15)
        CASE( 8 )
          ITYP = 25
          ITOP = 8
          JP(1:8)   = IP(1:8)
        CASE( 20 )
          ITYP = 26
          ITOP = 12
          JP(1:20)  = IP(1:20)
        END SELECT

      CASE( 3 )

        IDP = NMAT + IDS
        ITYP = 1
        ITOP = 0
        JP(1:2) = IP(1:2)

      CASE( 4 )

        IDP = NMAT + NRODA + IDS
        ITYP = 1
        ITOP = 0
        JP(1:2) = IP(1:2)

      END SELECT
      
      WRITE(CHAR,'(I10,12I4)') 
     &  ID,124,IDP,ITYP,ITOP,1,0,0,0,0,0,0,0

      CALL WT_DATA(CHAR,IFL)

      WRITE(CHAR,'(10I10)') JP(1:10)

      CALL WT_DATA(CHAR,IFL)

      WRITE(CHAR,'(10I10)') JP(11:20)

      CALL WT_DATA(CHAR,IFL)

      WRITE(IFL,'(A)') '0.,0.,0.,0,0,0,0,0,0,'

      WRITE(IFL,'(A)') '0.,0.,0.,'

      WRITE(IFL,'(A)') '0.,0.,0.,'

      WRITE(IFL,'(16(I1,A))') (0,C,I=1,16)

      END
