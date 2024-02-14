      SUBROUTINE FLOPEN(IFL)
C
      IMPLICIT NONE
C
      INCLUDE  'CONNEC.h'
C
      INTEGER,INTENT(INOUT)::IFL
C
      CHARACTER(60)::FN
      CHARACTER(1)::CNUMB(10)
      DATA CNUMB /'0','1','2','3','4','5','6','7','8','9'/
C
      INTEGER::I1,I10,MED
C
      MED = NRANK
      I1 = MOD(MED,10)
      I10 = (MED-I1)/10
c      write(6,*) 'flopen,ifl,i1,i10=',ifl,i1,i10
C
      IF(IFL.EQ.6) THEN
         FN = './FT06_' // CNUMB(I10+1) // CNUMB(I1+1)
         OPEN(IFL,STATUS='UNKNOWN',FILE=FN)
         write(6,*) 'ifl,fn=',ifl,fn
      ELSEIF(IFL.EQ.16) THEN
         FN = './FT16_' // CNUMB(I10+1) // CNUMB(I1+1)
         OPEN(IFL,STATUS='UNKNOWN',FILE=FN)
         write(6,*) 'ifl,fn=',ifl,fn
      ELSEIF(IFL.EQ.30) THEN
         FN = './data.grp_' // CNUMB(I10+1) // CNUMB(I1+1)
         OPEN(IFL,STATUS='UNKNOWN',FORM='UNFORMATTED',FILE=FN)
         write(6,*) 'ifl,fn=',ifl,fn
      ELSEIF(IFL.EQ.80) THEN
         FN = './data.grp_ml_' // CNUMB(I10+1) // CNUMB(I1+1)
         OPEN(IFL,STATUS='NEW',FILE=FN,FORM='UNFORMATTED' )
      ENDIF
C
      RETURN
      END
