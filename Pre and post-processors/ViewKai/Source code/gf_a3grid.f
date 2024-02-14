      SUBROUTINE GF_A3GRID(N,XX,YY,ZZ,VAL)

CD=== �T�v ===========================================================

CDT   GF_A3GRID:�i�q���W���̓Ǎ��Əo��

C==== �錾 ===========================================================

C     -- ���^ --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- ���ϐ� --
      INCLUDE 'GF_CONV.h'

CD    -- ���� --
CD    N         : IN  : I*4 : �����̃����N+1
CD    XX(NUMI0) : OUT : R*8 : x�����i�q���W
CD    YY(NUMJ0) : OUT : R*8 : y�����i�q���W
CD    ZZ(NUMK0) : OUT : R*8 : z�����i�q���W
CD    VAL(@@@@) : OUT : R*8 : ������
      DIMENSION XX(NUMI0),YY(NUMJ0),ZZ(NUMK0)
      DIMENSION VAL(NUMI0,NUMJ0,NUMK0)

CD    -- �Ǐ��ϐ� --
      CHARACTER*13 FILENM

C==== ���s ===========================================================

CD    -- ���̓t�@�C���̃t�@�C���� --
      IFL=IFLIN+N-1
      WRITE(FILENM,'(I5.5)') N-1
      FILENM='data.grp'//FILENM

CD    -- �i�q���W --
      LP=MYGIS(N)-1
      READ (IFL  ,ERR=9010) (XX(LP+I),I=1,NUMI(N))
      IF (N.EQ.NPROCS) THEN
        WRITE(IFLOU,ERR=9020) (XX(I),I=1,NUMI0)
      ENDIF
      LP=MYGJS(N)-1
      READ (IFL  ,ERR=9010) (YY(LP+J),J=1,NUMJ(N))
      IF (N.EQ.NPROCS) THEN
        WRITE(IFLOU,ERR=9020) (YY(J),J=1,NUMJ0)
      ENDIF
      LP=0
      READ (IFL  ,ERR=9010) (ZZ(LP+K),K=1,NUMK0  )
      IF (N.EQ.NPROCS) THEN
        WRITE(IFLOU,ERR=9020) (ZZ(K),K=1,NUMK0)
      ENDIF

CD    -- ���E�̃C���f�b�N�X --
      CALL GF_BCI(N)
C@    XXXXXXXXXXXXXX

CD    -- �󌄗� --
      IF (ISWLG.EQ.0) THEN
        CALL GF_VALD(N,VAL,0)
        IF (N.EQ.NPROCS) THEN
          WRITE(IFLOU,ERR=9020)
     &           (((VAL(I,J,K),I=IG1,IG2),J=JG1,JG2),K=KG1,KG2)
        ENDIF
      ENDIF

C     -- ���s���̏I�� --
      GOTO 9999

C==== �G���[���� =====================================================

 9010 CONTINUE
      WRITE(*,*) 'GF_A3GRID : READ ERROR (',FILENM,').'
      STOP

 9020 CONTINUE
      WRITE(*,*) 'GF_A3GRID : WRITE ERROR (data.grp).'
      STOP

C==== �I�� ===========================================================

 9999 CONTINUE
      RETURN
      END
