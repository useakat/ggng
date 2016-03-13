      double precision function func(z)
      implicit none

      integer i,j
      double precision z(*)

      include 'hmparm.inc'
      include 'smptab.inc'
      include 'gnbeam.inc'
      include 'cparam.inc'
      include 'cinit.inc'
      include 'cfinal.inc'
      include 'ccuts.inc'
      include 'coupl.inc'

      integer id,ierr
      double precision pdf(2)
      double precision phffin,phfpdf,fluxf
      double precision pt,eta,dr,pti,ptj,ptij
      double precision amp2
      double precision ene,cos,phi,tau,taul
      integer nh,ic(nexternal)

c-- functions
c      double precision psdrap, deltar, ptvec
      real*8 matrix,alphas,dot
      external matrix,alphas,dot

      func = 0.d0
      fluxf = 1d0/(2d0*s)

      call pshk0_dphi(s,nfin,z,ptcut,etacut,xbk(1),xbk(2),phffin,ierr)
c      call pshk0(s,nfin,z,ptcut,etacut,xbk(1),xbk(2),phffin,ierr)
      if (ierr.ne.0) return

      call apply_cuts(p(0,3),ierr)
      if (ierr.ne.0) return      

c      ren_scale = dsqrt(2*dot(P(0,1),P(0,2)))
      ren_scale = 20d0
c      fac_scale(1) = dsqrt(2*dot(P(0,1),P(0,2)))
c      fac_scale(2) = dsqrt(2*dot(P(0,1),P(0,2)))
      fac_scale(1) = 20d0
      fac_scale(2) = 20d0

      call set_pdf(lpp,pdg,xbk,fac_scale,pdf)
      phfpdf = pdf(1)*pdf(2)

      G = DSQRT(4d0*PI*ALPHAS(ren_scale))
      GC_4 = -G

      nh = int(dble(nhtot)*z(ndim))
      nh = mod(nh,nhtot)
      do i=1,nexternal
         nhel(i) = 2*mod(nh/2**(i-1),2)-1
      enddo
      do i = 1,nexternal
         ic(i) = 1
      enddo
      amp2 = matrix(p,nhel,ic)      
      func = phfpdf*fluxf*phfini*amp2*phffin
      
c      do i=1,nfin
c         call fvcomp(p(0,nini+i),ene,cos,eta,phi,pt)
c         id = 10*i
c         call xhfill(id+1,eta,func)
c         call xhfill(id+2,phi,func)
c         call xhfill(id+3,pt,func)
c         call xhfill(id+4,pt,func)
c         call xhfill(id+5,cos,func)
c      enddo
c      call xhfill(111,dfloat(nhel(1)+1+(nhel(2)+1)/2),func)
      do i=1,ndim
         call xhfill(i+1000,z(i),func)
      enddo
      
      return
      end
      
