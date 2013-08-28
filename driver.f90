program driver

USE ISO_VARYING_STRING
IMPLICIT NONE

type(VARYING_STRING) :: line, word, fname
INTEGER :: ierr,nd,wcount=0

OPEN(UNIT=1, FILE='v0/materials.xml')

CALL GET(1, line,IOSTAT=ierr)

print *, CHAR(line)

print *, ierr

END program
