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
      CALL VXXXXX(P(0,1),ZERO,NHEL(1),-1*IC(1),W(1,1))
      CALL VXXXXX(P(0,2),ZERO,NHEL(2),-1*IC(2),W(1,2))
      CALL VXXXXX(P(0,3),ZERO,NHEL(3),+1*IC(3),W(1,3))
      CALL VXXXXX(P(0,4),ZERO,NHEL(4),+1*IC(4),W(1,4))
      CALL GLUON4_0(W(1,IP(1)),W(1,IP(2)),W(1,IP(3)),W(1,IP(4)),
     &    GC_4/DSQRT(2D0),JAMP)
      MATRIX = 3**(NEXTERNAL-2)*(3**2-1)*JAMP*DCONJG(JAMP)

      END
