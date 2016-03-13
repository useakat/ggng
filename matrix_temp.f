      REAL*8 FUNCTION MATRIX(P,NHEL,IC)
      IMPLICIT NONE
C     
C     CONSTANTS
C     
      include 'cparam.inc'
      INTEGER    NWAVEFUNCS
      PARAMETER (NWAVEFUNCS=NEXTERNAL)
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
C     
C     ARGUMENTS 
C     
      REAL*8 P(0:3,NEXTERNAL)
      INTEGER NHEL(NEXTERNAL), IC(NEXTERNAL)
C     
C     LOCAL VARIABLES 
C     
      INTEGER I,J
      COMPLEX*16 JAMP
      COMPLEX*16 W(6,NWAVEFUNCS)
C     
C     GLOBAL VARIABLES
C     
      INCLUDE 'coupl.inc'
      INTEGER IP(NEXTERNAL),IPFLAG
      common /IP/ IP
      integer icf
      common /icf/ icf
C     ----------
C     BEGIN CODE
C     ----------
      insert_amp
      MATRIX = 3**(NEXTERNAL-2)*(3**2-1)*JAMP*DCONJG(JAMP)

      END
