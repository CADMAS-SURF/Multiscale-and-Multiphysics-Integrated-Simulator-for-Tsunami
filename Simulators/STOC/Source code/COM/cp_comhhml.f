      SUBROUTINE CP_COMHHML(HH_ML,I_NS,J_NS,MX_ML,MY_ML,MX_NS,MY_NS,
     $                     IEAS,IWES,JSOU,JNOR)
C-----------------------------------------------------------------------
C     HH_MLを通信する
C-----------------------------------------------------------------------
C
      use mod_comm,only: comm_model
      IMPLICIT NONE
C
      INCLUDE 'CONNEC.h'
      INCLUDE 'mpif.h'
C
      INTEGER,INTENT(INOUT)::MX_ML,MY_ML,MX_NS,MY_NS
      INTEGER,INTENT(INOUT)::IEAS,IWES,JSOU,JNOR
C
      REAL(8),INTENT(INOUT)::HH_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1)
      INTEGER,INTENT(INOUT)::I_NS(2,MX_NS),J_NS(2,MY_NS)
C
      INTEGER::ISTAT(MPI_STATUS_SIZE)
      INTEGER::I,J,JS,JE,IS,IE,ISND,IRCV,ITAG,IREQ,IERROR
      REAL(8)::HH_MLX(IWES-1:IEAS+1),HH_MLY(JSOU-1:JNOR+1)
C
      ITAG = 0
      JS = 0
      JE = 0
      IF(IPECON(4,NRANK+1).GE.0) THEN
        ISND = IPECON(4,NRANK+1)
        JS = J_NS(2,2)
        DO 100 I=IWES-1,IEAS+1
          HH_MLX(I) = HH_ML(I,JS)
  100   CONTINUE
        CALL MPI_ISEND(HH_MLX(IWES-1),IEAS-IWES+3,MPI_DOUBLE_PRECISION,
     $                 ISND,ITAG,comm_model,IREQ,IERROR)
        CALL MPI_WAIT(IREQ,ISTAT,IERROR)
      END IF
C
      IF(IPECON(7,NRANK+1).GE.0) THEN
        IRCV = IPECON(7,NRANK+1)
        CALL MPI_IRECV(HH_MLX(IWES-1),IEAS-IWES+3,MPI_DOUBLE_PRECISION,
     $                 IRCV,MPI_ANY_TAG,comm_model,IREQ,IERROR)
        CALL MPI_WAIT(IREQ,ISTAT,IERROR)
        JE = J_NS(2,MY_NS-1)
        IS = IWES-1
        IE = IEAS+1
        IF(IPECON(5,NRANK+1).GE.0) IS=I_NS(2,2)
        IF(IPECON(6,NRANK+1).GE.0) IE=I_NS(2,MX_NS-1)
        DO 110 I=IS,IE
          HH_ML(I,JE+1) = HH_MLX(I)
  110   CONTINUE
      END IF
C
      IS = 0
      IE = 0
      IF(IPECON(5,NRANK+1).GE.0) THEN
        ISND = IPECON(5,NRANK+1)
        IS = I_NS(2,2)
        DO 120 J=JSOU-1,JNOR+1
          HH_MLY(J) = HH_ML(IS,J)
  120   CONTINUE
        CALL MPI_ISEND(HH_MLY(JSOU-1),JNOR-JSOU+3,MPI_DOUBLE_PRECISION,
     $                 ISND,ITAG,comm_model,IREQ,IERROR)
        CALL MPI_WAIT(IREQ,ISTAT,IERROR)
      END IF
C
      IF(IPECON(6,NRANK+1).GE.0) THEN
        IRCV = IPECON(6,NRANK+1)
        CALL MPI_IRECV(HH_MLY(JSOU-1),JNOR-JSOU+3,MPI_DOUBLE_PRECISION,
     $                 IRCV,MPI_ANY_TAG,comm_model,IREQ,IERROR)
        CALL MPI_WAIT(IREQ,ISTAT,IERROR)
        IE = I_NS(2,MX_NS-1)
        JS = JSOU-1
        JE = JNOR+1
        IF(IPECON(4,NRANK+1).GE.0) JS=J_NS(2,2)
        IF(IPECON(7,NRANK+1).GE.0) JE=J_NS(2,MY_NS-1)
        DO 130 J=JS,JE
          HH_ML(IE+1,J) = HH_MLY(J)
  130   CONTINUE
      END IF
C
      IS = 0
      IE = 0
      IF(IPECON(6,NRANK+1).GE.0) THEN
        ISND = IPECON(6,NRANK+1)
        IE = I_NS(2,MX_NS-1)
        DO 140 J=JSOU-1,JNOR+1
          HH_MLY(J) = HH_ML(IE,J)
  140   CONTINUE
        CALL MPI_ISEND(HH_MLY(JSOU-1),JNOR-JSOU+3,MPI_DOUBLE_PRECISION,
     $                 ISND,ITAG,comm_model,IREQ,IERROR)
        CALL MPI_WAIT(IREQ,ISTAT,IERROR)
      END IF
C
      IF(IPECON(5,NRANK+1).GE.0) THEN
        IRCV = IPECON(5,NRANK+1)
        CALL MPI_IRECV(HH_MLY,JNOR-JSOU+3,MPI_DOUBLE_PRECISION,
     $                 IRCV,MPI_ANY_TAG,comm_model,IREQ,IERROR)
        CALL MPI_WAIT(IREQ,ISTAT,IERROR)
        IS = I_NS(2,2)
        JS = JSOU-1
        JE = JNOR+1
        IF(IPECON(4,NRANK+1).GE.0) JS=J_NS(2,2)
        IF(IPECON(7,NRANK+1).GE.0) JE=J_NS(2,MY_NS-1)
        DO 150 J=JS,JE
          HH_ML(IS-1,J) = HH_MLY(J)
  150   CONTINUE
      END IF
C
      JS = 0
      JE = 0
      IF(IPECON(7,NRANK+1).GE.0) THEN
        ISND = IPECON(7,NRANK+1)
        JE = J_NS(2,MY_NS-1)
        DO 160 I=IWES-1,IEAS+1
          HH_MLX(I) = HH_ML(I,JE)
  160   CONTINUE
        CALL MPI_ISEND(HH_MLX(IWES-1),IEAS-IWES+3,MPI_DOUBLE_PRECISION,
     $                 ISND,ITAG,comm_model,IREQ,IERROR)
        CALL MPI_WAIT(IREQ,ISTAT,IERROR)
      END IF
C
      IF(IPECON(4,NRANK+1).GE.0) THEN
        IRCV = IPECON(4,NRANK+1)
        CALL MPI_IRECV(HH_MLX(IWES-1),IEAS-IWES+3,MPI_DOUBLE_PRECISION,
     $                 IRCV,MPI_ANY_TAG,comm_model,IREQ,IERROR)
        CALL MPI_WAIT(IREQ,ISTAT,IERROR)
        JS = J_NS(2,2)
        JE = J_NS(2,MY_NS-1)
        IS = IWES-1
        IE = IEAS+1
        IF(IPECON(5,NRANK+1).GE.0) IS=I_NS(2,2)
        IF(IPECON(6,NRANK+1).GE.0) IE=I_NS(2,MX_NS-1)
        DO 170 I=IS,IE
          HH_ML(I,JS-1) = HH_MLX(I)
  170   CONTINUE
      END IF
C
      RETURN
      END
