      SUBROUTINE GLB_COMM(KK,RR,IFL,FLNAME,NLEN,ICHK)

      USE M_VAL

      IMPLICIT REAL*8(A-H,O-Z)

      CHARACTER*256 FLNAME

      DIMENSION KK(*),RR(*),IFL(*)

      DO

        CALL M_MPI_RECV_I(IOP,1,1)

        SELECT CASE( IOP )
        CASE( 0 ) 
          RETURN
        CASE( 1 ) 
          CALL RECV_KK(KK)
        CASE( 2 ) 
          CALL RECV_RR(RR)
        CASE( 3 ) 
          CALL RESFRC0(RR,IFL(11))
        CASE( 4 ) 
          CALL NPFLOW0(IFL(11))
        CASE( 5 )
          CALL FEMAP_OUT0(KK,INDG,IFL(15))
        CASE( 6 )
          CALL ENS_GEOM0(KK,IFL(20),FLNAME,NLEN,ICHK)
        CASE( 7 )
          CALL ENS_CASE_T(KK,IFL(20),FLNAME,NLEN,ICHK)
        CASE( 8 )
          CALL OUTPUT0(KK,IFL,FLNAME,NLEN,ICHK)
        CASE( 10 )
          CALL RDPRESS0(KK,RR,IFL(14),FLNAME,NLEN,ICHK)
        CASE( 11 )
          CALL RECV_SURF0(KK,RR,ICHK)
        CASE( 12 )
          CALL RECV_PRES0(KK)
        CASE( 14 )
          CALL SEND_POS0(KK(8),KK(10),KK(25))
        CASE( 15 )
          CALL RMSH_INIT0(KK,ICHK)
        CASE( 16 )
          CALL REMESH0(KK)
        CASE( 20 )
          CALL BCTSET0(KK)
        CASE( 21 )
          CALL SEND_CONT0(KK)
        CASE( 22 )
          CALL CONT_INIT_0(KK)
        CASE( 23 )
          CALL PART_SEND_TBLX(KK)
        CASE( 24 )
          CALL CONTACT0(KK,RR,IFL(11))
        CASE( 25 )
          CALL MDPRESS0(KK)
        CASE( 30 )
          CALL WT_RESTART0(KK,IFL,FLNAME,NLEN)
        CASE( 31 )
          CALL RD_RESTART0(KK,IFL(17),FLNAME,NLEN)
        CASE( 32 )
          CALL RECV_RST0()
        END SELECT

      ENDDO

      END
