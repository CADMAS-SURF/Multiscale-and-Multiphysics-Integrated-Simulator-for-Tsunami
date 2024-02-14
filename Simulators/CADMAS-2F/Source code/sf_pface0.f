      SUBROUTINE SF_PFACE0()

      USE SF_ARRAY

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'SF_STRUCT.h'

      DIMENSION ITET(8,6),IPEN(8,6),IHEX(8,6),IORDER(8,6),IEP(20)
     &         ,IP(8),JORDER(8),JEP(20),JP(8)

      INTEGER, POINTER :: IPF(:,:), NEF(:), IEF(:,:,:)

      ! 要素タイプごと，構成面の構成節点の並び

      DATA ITET / 3,2,1,6,5,7,0,0,     1,2,4,5,9,8,0,0,
     &            2,3,4,6,10,9,0,0,    3,1,4,7,8,10,0,0,
     &            0,0,0,0,0,0,0,0,     0,0,0,0,0,0,0,0 /
      DATA IPEN / 3,2,1,8,7,9,0,0,     4,5,6,13,14,15,0,0,
     &            1,2,5,4,7,11,13,10,  2,3,6,5,8,12,14,11,
     &            3,1,4,6,9,10,15,12,  0,0,0,0,0,0,0,0 /
      DATA IHEX / 4,3,2,1,11,10,9,12,  5,6,7,8,17,18,19,20,
     &            1,2,6,5,9,14,17,13,  2,3,7,6,10,15,18,14,
     &            3,4,8,7,11,16,19,15, 4,1,5,8,12,13,20,16 /

      IF( MYRANK > 0 ) RETURN

      ! IPFのペア検索候補テーブル作成
      ! NEF(I) : 節点Iを第1節点とする面の数
      ! IEF(1,J,I) : 節点Iを第1節点とする面が貼り付いている要素
      ! IEF(2,J,I) : 節点Iを第1節点とする面が貼り付いている要素における要素面番号

      ALLOCATE( NEF(NNOD) )

      NEF(:) = 0

      DO IE = 1, NELM

        NI = IELM(4,IE)

        IEP(1:NI) = IELM(5:4+NI,IE)

        SELECT CASE( NI )
        CASE( 4, 10 )
          NIFC = 4
          IORDER(1,:) = ITET(1,:)
        CASE( 6, 15 )
          NIFC = 5
          IORDER(1,:) = IPEN(1,:)
        CASE( 8, 20 )
          NIFC = 6
          IORDER(1,:) = IHEX(1,:)
        END SELECT

        DO IFC = 1, NIFC

          IP1 = IEP( IORDER(1,IFC) )

          NEF(IP1) = NEF(IP1) + 1

        ENDDO

      ENDDO

      ALLOCATE( IEF(2,MAXVAL(NEF),NNOD) )

      NEF(:) = 0

      DO IE = 1, NELM

        NI = IELM(4,IE)

        IEP(1:NI) = IELM(5:4+NI,IE)

        SELECT CASE( NI )
        CASE( 4, 10 )
          NIFC = 4
          IORDER(1,:) = ITET(1,:)
        CASE( 6, 15 )
          NIFC = 5
          IORDER(1,:) = IPEN(1,:)
        CASE( 8, 20 )
          NIFC = 6
          IORDER(1,:) = IHEX(1,:)
        END SELECT

        DO IFC = 1, NIFC

          IP1 = IEP( IORDER(1,IFC) )

          NEF(IP1) = NEF(IP1) + 1

          IEF(1,NEF(IP1),IP1) = IE
          IEF(2,NEF(IP1),IP1) = IFC

        ENDDO

      ENDDO

      ! IPF(I,J) : 要素JのI番目の構成面に対し
      !            ペアとなる他の要素の構成面により塞がれて，外表面となっていない -> 1
      !            外表面となっている -> 0

      ALLOCATE( IPF(6,NELM) )

      NPFC = 0

      IPF(:,:) = 0

      DO 10 IE = 1, NELM

        NI = IELM(4,IE)

        IEP(1:NI) = IELM(5:4+NI,IE)

        SELECT CASE( NI )
        CASE( 4, 10 )
          NIFC = 4
          IORDER(:,:) = ITET(:,:)
        CASE( 6, 15 )
          NIFC = 5
          IORDER(:,:) = IPEN(:,:)
        CASE( 8, 20 )
          NIFC = 6
          IORDER(:,:) = IHEX(:,:)
        END SELECT

        DO 20 IFC = 1, NIFC

          IF( IPF(IFC,IE) == 1 ) CYCLE

          IF( NIFC == 4 .OR. ( NIFC == 5 .AND. IFC <= 2 ) ) THEN
            NIP = 3
          ELSE
            NIP = 4
          ENDIF

          IP(1:NIP) = IEP( IORDER(1:NIP,IFC) )

          DO 30 I = 1, NIP

            IG = IP(I)

            DO 40 J = 1, NEF(IG)

              JE  = IEF(1,J,IG)
              JFC = IEF(2,J,IG)

              IF( JE <= IE ) CYCLE

              IF( IPF(JFC,JE) == 1 ) CYCLE

              NJ = IELM(4,JE)

              JEP(1:NJ) = IELM(5:4+NJ,JE)

              SELECT CASE( NJ )
              CASE( 4, 10 )
                NJFC = 4
                JORDER(:) = ITET(:,JFC)
              CASE( 6, 15 )
                NJFC = 5
                JORDER(:) = IPEN(:,JFC)
              CASE( 8, 20 )
                NJFC = 6
                JORDER(:) = IHEX(:,JFC)
              END SELECT

              IF( NJFC == 4 .OR. ( NJFC == 5 .AND. JFC <= 2 ) ) THEN
                NJP = 3
              ELSE
                NJP = 4
              ENDIF

              IF( NIP /= NJP ) CYCLE

              JP(1:NJP) = JEP( JORDER(1:NJP) )

              ! 構成面同士の照合

              IF( NIP == 3 ) THEN
                CALL SF_COL3(IBING,IP,JP)
              ELSEIF( NIP == 4 ) THEN
                CALL SF_COL4(IBING,IP,JP)
              ENDIF

              IF( IBING == 1 ) THEN  ! ペアとなる構成面あり -> 外表面とならない
                IPF(IFC,IE) = 1
                IPF(JFC,JE) = 1
                GOTO 20
              ENDIF

   40       CONTINUE

   30     CONTINUE

          NPFC = NPFC + 1  ! ペアとなる構成面見つからず -> 外表面となる

   20   CONTINUE

   10 CONTINUE

      DEALLOCATE( NEF )
      DEALLOCATE( IEF )

      ALLOCATE( IPFACE(12,NPFC) )

      IPFC = 0

      IPFACE(:,:) = 0

      DO IE = 1, NELM

        ITYP = IELM(2,IE)

        NI = IELM(4,IE)

        IEP(1:NI) = IELM(5:4+NI,IE)

        SELECT CASE( NI )
        CASE( 4, 10 )
          NIFC = 4
          IORDER(:,:) = ITET(:,:)
        CASE( 6, 15 )
          NIFC = 5
          IORDER(:,:) = IPEN(:,:)
        CASE( 8, 20 )
          NIFC = 6
          IORDER(:,:) = IHEX(:,:)
        END SELECT

        DO IFC = 1, NIFC

          IF( IPF(IFC,IE) == 1 ) CYCLE

          IF( NIFC == 4 .OR. ( NIFC == 5 .AND. IFC <= 2 ) ) THEN
            SELECT CASE( NI )
            CASE( 4, 6 )
              NIP = 3
            CASE( 10, 15 )
              NIP = 6
            END SELECT
          ELSE
            SELECT CASE( NI )
            CASE( 6, 8 )
              NIP = 4
            CASE( 15, 20 )
              NIP = 8
            END SELECT
          ENDIF

          IP(1:NIP) = IEP( IORDER(1:NIP,IFC) )

          IPFC = IPFC + 1

          IPFACE(2,IPFC) = ITYP

          IPFACE(3,IPFC) = IE

          IPFACE(4,IPFC) = NIP

          IPFACE(5:4+NIP,IPFC) = IP(1:NIP)

        ENDDO

      ENDDO

      DEALLOCATE( IPF )

      NPFC0 = NPFC

      ALLOCATE( IPFACE0(12,NPFC0) )

      IPFACE0(:,:) = IPFACE(:,:)

      ALLOCATE( IGFC(NNOD) )

      IGFC(:) = 0

      DO I = 1, NPFC
        N = IPFACE(4,I)
        IGFC( IPFACE(5:4+N,I) ) = 1
      ENDDO

      ALLOCATE( AFC(NPFC0) )
      ALLOCATE( IPGRID0(2,NNOD0) )
      ALLOCATE( IPND0(NNOD0) )
      ALLOCATE( PRES0(NNOD0) )

      END