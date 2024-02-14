      SUBROUTINE OUTPUT(KK,NNOD,NIGSF,MGP,GRID,IELM,IELQ,IVRQ,DT1,DT2
     &                 ,UG1,UG2,UG3,DZ,UG,VEL,ACC,RFCO,PG3,PPND,AFC,EPSG
     &                 ,SIGG,VELG,VELE,N_PART,NP_ENS,NE_ENS,IE_ENS
     &                 ,NG_ENS,IG_ENS,NG_ENSW,IG_ENSW,NE_ENSW,IE_ENSW
     &                 ,IDYN,ITO,IFL,FLNAME,NLEN,ICHK)
C
      USE MPI_PARAM
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*256 FLNAME
      CHARACTER*256 FLN
      CHARACTER*4 NO
      DIMENSION KK(*),IELM(*),GRID(3,NNOD),IELQ(*),IVRQ(NNOD+NIGSF)
     &         ,UG1(6,NNOD),UG2(6,NNOD),UG3(6,NNOD+NIGSF),DZ(NNOD+NIGSF)
     &         ,UG(6,NNOD+NIGSF),RFCO(6,NNOD),NP_ENS(6),NE_ENS(6,N_PART)
     &         ,IE_ENS(*),NG_ENS(N_PART),IG_ENS(NNOD,N_PART),IFL(*)
     &         ,VEL(3,NNOD),ACC(3,NNOD),EPSG(6*MGP,*),SIGG(6*MGP,*)
     &         ,PG3(NNOD),VELG(3,MGP,*),VELE(*),PPND(NNOD),AFC(*)
     &         ,NG_ENSW(*),IG_ENSW(*),NE_ENSW(*),IE_ENSW(*)
      DATA IOUT / -1 /
C-----------------------------------------------------------------------
      IF( MYRANK == 1 ) CALL M_MPI_SEND_I(8,1,0)  ! SEND IOP=8 TO GLB_COMM
C
      IF( MYRANK == 0 ) THEN
C
        FLN = 'str_out/'//FLNAME(1:NLEN)
        N = 8 + NLEN
C
        IOUT = IOUT + 1
        WRITE(NO,'(I4.4)') IOUT
C
        OPEN( IFL(20), FILE = FLN(1:N)//'.disp_'//NO )
        OPEN( IFL(21), FILE = FLN(1:N)//'.vel_'//NO )
        OPEN( IFL(22), FILE = FLN(1:N)//'.acc_'//NO )
        OPEN( IFL(23), FILE = FLN(1:N)//'.rfc_'//NO )
        IF( KK(25) > 0 ) 
     &    OPEN( IFL(24), FILE = FLN(1:N)//'.pprs_'//NO )
C
      ENDIF
C
      UG(:,1:NNOD) = UG3(:,1:NNOD)
      IF( ISTM == 1 ) UG(3,1:NNOD) = UG(3,1:NNOD) + DZ(1:NNOD)
C
      CALL OUTNOD(DT1,DT2,UG1,UG2,UG3,UG,VEL,ACC,RFCO,PG3,NNOD,KK(26)
     &           ,N_PART,NP_ENS,NG_ENS,IG_ENS,KK(25),IDYN,IFL)
C
      IF( MYRANK == 0 ) THEN
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
      ENDIF
C
      CALL OUTSOL(KK,GRID,IELM,KK(37),EPSG,SIGG,MGP,PG3,NP_ENS,NG_ENS
     &           ,IG_ENS,ITO,IFL)
C
      IF( MYRANK == 0 ) THEN
C
        CLOSE( IFL(20) )
        CLOSE( IFL(21) )
        CLOSE( IFL(22) )
C
      ENDIF
C
      IF( KK(25) > 0 ) THEN
C
        IF( MYRANK == 0 ) OPEN( IFL(20), FILE = FLN(1:N)//'.flux_'//NO )
C
        CALL OUTSOIL(KK,IELM,KK(37),VELG,MGP,VELE,NP_ENS,NG_ENS,IG_ENS
     &              ,IFL(20))
C
        IF( MYRANK == 0 ) CLOSE( IFL(20) )
C
      ENDIF
C
      IF( ICHK < 2 ) RETURN
C
      IF( MYRANK == 0 ) THEN
C
        IF( KK(81) > 0 .OR. KK(92) > 0 )
     &    OPEN( IFL(20), FILE = FLN(1:N)//'.dispw_'//NO )
        IF( KK(81) > 0 )
     &    OPEN( IFL(21), FILE = FLN(1:N)//'.surfp_'//NO )
C
        IF( KK(92) > 0 ) THEN
          UG(:,NNOD+1:NNOD+NIGSF) = UG3(:,NNOD+1:NNOD+NIGSF)
          IF( ISTM == 1 ) THEN
            CALL GSURFQ(DZ(NNOD+1),1,NIGSF,DZ,IELQ,IVRQ(NNOD+1))
            UG(3,NNOD+1:NNOD+NIGSF) = UG(3,NNOD+1:NNOD+NIGSF)
     &                              + DZ(NNOD+1:NNOD+NIGSF)
          ENDIF
        ENDIF
C
        CALL OUTNODW(UG,PPND,NNOD,KK(11),KK(81),KK(92),KK(94),N_PART
     &              ,NG_ENSW,IG_ENSW,IFL)
C
        IF( KK(81) > 0 .OR. KK(92) > 0 ) CLOSE( IFL(20) )
        IF( KK(81) > 0 ) CLOSE( IFL(21) )
C
        IF( KK(81) > 0 ) THEN
C
          OPEN( IFL(20), FILE = FLN(1:N)//'.surfa_'//NO )
C
          CALL WTSURF(AFC,KK(11),KK(81),N_PART,NE_ENSW,IE_ENSW,IFL(20))
C
          CLOSE( IFL(20) )
C
        ENDIF
C
      ELSE
C
        IF( ICPL > 0 .OR. KK(92) > 0 ) CALL M_MPI_SEND_D(UG,6*KK(26),0)
        IF( ICPL > 0 ) CALL M_MPI_SEND_D(PPND,KK(26),0)
        IF( ICPL > 0 ) CALL M_MPI_SEND_D(AFC,KK(81),0)
C
      ENDIF
C
      END
