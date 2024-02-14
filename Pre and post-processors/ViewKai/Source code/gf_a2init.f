      SUBROUTINE GF_A2INIT(N)

CD=== �T�v ===========================================================

CDT   GF_A2INIT:���������̓Ǎ��Əo��

C==== �錾 ===========================================================

C     -- ���^ --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- ���ϐ� --
      INCLUDE 'GF_CONV.h'

CD    -- ���� --
CD    N : CNS : I*4 : �����̃����N+1

CD    -- �Ǐ��ϐ� --
      CHARACTER*13 FILENM

C==== ���s ===========================================================

CD    -- ���̓t�@�C���̃I�[�v���ƃ��b�Z�[�W�̏o�� --
      IFL=IFLIN+N-1
      WRITE(FILENM,'(I5.5)') N-1
      FILENM='data.grp'//FILENM
      WRITE(*,*) 'IN  :',FILENM,N-1
      OPEN(IFL,ERR=9010,FILE=FILENM,
     &     STATUS='OLD',FORM='UNFORMATTED' )

CD    -- �o�̓t�@�C���̃I�[�v���ƃ��b�Z�[�W�̏o�� --
      IF (N.EQ.NPROCS) THEN
        OPEN(IFLOU,ERR=9040,FILE='data.grp',
     &       STATUS='NEW',FORM='UNFORMATTED' )
        WRITE(*,*) 'OUT :','data.grp'
      ENDIF

CD    -- �o�[�W���� --
      READ (IFL  ,ERR=9020) I1,I2
      IF (N.EQ.NPROCS) THEN
        WRITE(IFLOU,ERR=9050) I1,I2
      ENDIF

CD    -- ��͗̈� --
      READ (IFL  ,ERR=9020) NUMI0,NUMJ0,NUMK0
      IF (N.EQ.NPROCS) THEN
        WRITE(IFLOU,ERR=9050) NUMI0,NUMJ0,NUMK0
      ENDIF
      READ (IFL  ,ERR=9020) D1,D2,D3
      IF (N.EQ.NPROCS) THEN
        WRITE(IFLOU,ERR=9050) D1,D2,D3
      ENDIF
      READ (IFL  ,ERR=9020) D1,D2,D3
      IF (N.EQ.NPROCS) THEN
        WRITE(IFLOU,ERR=9050) D1,D2,D3
      ENDIF

CD    -- �o�͗̈� --
      READ (IFL  ,ERR=9020) IG1,JG1,KG1,IG2,JG2,KG2
      IF (N.EQ.NPROCS) THEN
        WRITE(IFLOU,ERR=9050) IG1,JG1,KG1,IG2,JG2,KG2
      ENDIF
      READ (IFL  ,ERR=9020) NBX(N),NBY(N),NBZ(N)
      IF (N.EQ.NPROCS) THEN
        WRITE(IFLOU,ERR=9050) 0     ,0     ,0
      ENDIF
      IG1=IG1-1
      JG1=JG1-1
      KG1=KG1-1
      IG2=IG2-1
      JG2=JG2-1
      KG2=KG2-1

CD    -- ������ --
      READ(IFL,ERR=9020) NPROCS,NUMI(N),NUMJ(N)
      READ(IFL,ERR=9020) MYIS (N),MYIE (N),MYJS (N),MYJE (N)
      READ(IFL,ERR=9020) MYMIS(N),MYMIE(N),MYMJS(N),MYMJE(N)
      READ(IFL,ERR=9020) MYGIS(N),MYGIE(N),MYGJS(N),MYGJE(N)
      IF (NPROCS.GT.MPROCS) GOTO 9030

CD    -- ���Ԗ��ɏo�͂��镨���ʂ̃t���O --
      READ (IFL  ,ERR=9020) ISWLN,ISWLV,ISWLP,ISWLF,ISWLK,
     &                      ISWLT,ISWLS,ISWLG,ISWL1,ISWL2
      IF (N.EQ.NPROCS) THEN
        WRITE(IFLOU,ERR=9050) ISWLN,ISWLV,ISWLP,ISWLF,ISWLK,
     &                        ISWLT,ISWLS,ISWLG,ISWL1,ISWL2
      ENDIF

C     -- ���s���̏I�� --
      GOTO 9999

C==== �G���[���� =====================================================

 9010 CONTINUE
      WRITE(*,*) 'GF_A2INIT : CAN NOT OPEN (',FILENM,').'
      STOP

 9020 CONTINUE
      WRITE(*,*) 'GF_A2INIT : READ ERROR (',FILENM,').'
      STOP

 9030 CONTINUE
      WRITE(*,*) 'GF_A2INIT : AREA IS FULL.'
      STOP

 9040 CONTINUE
      WRITE(*,*) 'GF_A2INIT : CAN NOT OPEN (data.grp).'
      STOP

 9050 CONTINUE
      WRITE(*,*) 'GF_A2INIT : WRITE ERROR (data.grp).'
      STOP

C==== �I�� ===========================================================

 9999 CONTINUE
      RETURN
      END
