      SUBROUTINE OUTPUT0(KK,IFL,FLNAME,NLEN,ICHK)
C
      USE M_VAL
C
      CHARACTER*256 FLNAME
      CHARACTER*256 FLN
      CHARACTER*4 NO
C
      DIMENSION KK(*),IFL(*)
      REAL(8),POINTER :: UG(:,:)
C
      DATA JOUT / -1 /
C-----------------------------------------------------------------------
      FLN = 'str_out/'//FLNAME(1:NLEN)
      N = 8 + NLEN
C
      JOUT = JOUT + 1
      WRITE(NO,'(I4.4)') JOUT
C
      OPEN( IFL(20), FILE = FLN(1:N)//'.disp_'//NO )
      OPEN( IFL(21), FILE = FLN(1:N)//'.vel_'//NO )
      OPEN( IFL(22), FILE = FLN(1:N)//'.acc_'//NO )
      OPEN( IFL(23), FILE = FLN(1:N)//'.rfc_'//NO )
      IF( KK(25) > 0 ) 
     &  OPEN( IFL(24), FILE = FLN(1:N)//'.pprs_'//NO )
C
      CALL OUTNOD0(KK(8),KK(25),IFL)
C
      CLOSE( IFL(20) )
      CLOSE( IFL(21) )
      CLOSE( IFL(22) )
      CLOSE( IFL(23) )
      IF( KK(25) > 0 ) CLOSE( IFL(24) )
C
      OPEN( IFL(20), FILE = FLN(1:N)//'.sols_'//NO )
      OPEN( IFL(21), FILE = FLN(1:N)//'.sole_'//NO )
      OPEN( IFL(22), FILE = FLN(1:N)//'.solps_'//NO )
C
      CALL OUTSOL0(KK(8),IFL)
C
      CLOSE( IFL(20) )
      CLOSE( IFL(21) )
      CLOSE( IFL(22) )
C
      IF( KK(25) > 0 ) THEN
C
        OPEN( IFL(20), FILE = FLN(1:N)//'.flux_'//NO )
C
        CALL OUTSOIL0(KK(8),IFL(20))
C
        CLOSE( IFL(20) )
C
      ENDIF
C
      IF( ICHK < 2 ) RETURN
C
      IF( KK(81) > 0 .OR. KK(92) > 0 ) THEN
        OPEN( IFL(20), FILE = FLN(1:N)//'.dispw_'//NO )
        ALLOCATE( UG(6,KK(8)+KK(94)) )
        CALL GATHER_NODAL_D(UG,6)
        CALL GSURFQ(UG(1,KK(8)+1),6,KK(94),UG,IELQ,IVRQ(KK(8)+1))
      ENDIF
C
      IF( KK(81) > 0 ) THEN
        OPEN( IFL(21), FILE = FLN(1:N)//'.surfp_'//NO )
        ALLOCATE( PPND(KK(8)) )
        CALL GATHER_NODAL_D(PPND,1)
      ENDIF
C
      CALL OUTNODW(UG,PPND,KK(8),KK(11),KK(81),KK(92),KK(94),N_PART
     &            ,NG_ENSW,IG_ENSW,IFL)
C
      IF( KK(81) > 0 .OR. KK(92) > 0 ) THEN
        DEALLOCATE( UG )
        CLOSE( IFL(20) )
      ENDIF
C
      IF( KK(81) > 0 ) THEN
        DEALLOCATE( PPND )
        CLOSE( IFL(21) )
      ENDIF
C
      IF( KK(81) > 0 ) THEN
C
        OPEN( IFL(20), FILE = FLN(1:N)//'.surfa_'//NO )
C
        ALLOCATE( AFC(KK(81)) )
        CALL GATHER_SURFACE(AFC,1)
C
        CALL WTSURF(AFC,KK(11),KK(81),N_PART,NE_ENSW,IE_ENSW,IFL(20))
C
        DEALLOCATE( AFC )
C
        CLOSE( IFL(20) )
C
      ENDIF
C
      END
