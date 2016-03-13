c-----------------------------------------------------------------------------
      subroutine pshk0_dphi(s,n,rdn,ptcut,etacut,x1,x2,jacob,iflg)
      implicit none

      double precision s
      integer n
      double precision rdn(30)
      double precision ptcut, etacut

      double precision x1, x2
      double precision wpsn, jacob
      integer iflg

      double precision pi
      parameter (pi = 3.14159265358979323846d0)

      include 'cparam.inc'
      include 'cfinal.inc'

      integer i
      double precision psumx, psumy, e00, pz0
      double precision shat, pipt2, w, e, el, etacut2
      double precision eta(n),deta,pt2,pt,phi(n),dphi
     
      double precision ptcutl, paul

      wpsn  = 0.0d0
      iflg = 0 

      w = sqrt(s)
      e = 0.5d0*w

      ptcutl = log(ptcut)
      el = log(e)

      etacut2 = 2.d0*etacut

      psumx = 0.0d0 
      psumy = 0.0d0 
      pipt2 = 1.0d0
      e00 = 0.0d0
      pz0 = 0.0d0
      
      eta(1) = -etacut+etacut2*rdn(n)
      phi(1) = 2*pi*rdn(2*n)
      do i=1,n-1
         pt = exp(ptcutl+(el-ptcutl)*rdn(i))
         if (i.ne.1) then
            deta = etacut2*rdn((n-1)+i)
            eta(i) = -etacut+dmod(eta(i-1)+etacut+deta,etacut2)
            dphi = 2*pi*rdn((2*n-1)+i)
            phi(i) = dmod(phi(i-1) +dphi,2*pi)
         endif
         psumx = psumx-pt*cos(phi(i))
         psumy = psumy-pt*sin(phi(i))
c         p(0,i+2) = 0.5d0*pt*(exp(eta)+exp(-eta))
c         p(3,i+2) = 0.5d0*pt*(exp(eta)-exp(-eta))
         p(0,i+2) = pt*cosh(eta(i))
         p(3,i+2) = pt*sinh(eta(i))
         p(1,i+2) = pt*cos(phi(i))
         p(2,i+2) = pt*sin(phi(i)) 
         e00 = e00+p(0,i+2)
         pz0 = pz0+p(3,i+2) 
         pipt2 = pipt2*pt**2
      enddo
      deta = etacut2*rdn(2*n-1)
      eta(n) = -etacut+dmod(eta(n-1)+etacut+deta,etacut2)
      pt = sqrt(psumx**2+psumy**2)
      phi(n) = atan2(psumy,psumx)

c      p(0,n+2) = 0.5d0*pt*(exp(eta)+exp(-eta))
c      p(3,n+2) = 0.5d0*pt*(exp(eta)-exp(-eta))
      p(0,n+2) = pt*cosh(eta(n))
      p(3,n+2) = pt*sinh(eta(n))
      p(1,n+2) = pt*cos(phi(n))
      p(2,n+2) = pt*sin(phi(n)) 
      e00 = e00+p(0,n+2)
      pz0 = pz0+p(3,n+2) 

      x1 = (e00+pz0)/w 
      x2 = (e00-pz0)/w 
      shat = s*x1*x2

      if (x1.gt.1.0d0 .or. x2.gt.1.0d0) then
         iflg = 1
         return
      endif

      p(0,1) = x1*e
      p(1,1) = 0.d0
      p(2,1) = 0.d0
      p(3,1) = x1*e

      p(0,2) = x2*e
      p(1,2) = 0.d0
      p(2,2) = 0.d0
      p(3,2) = -x2*e
      
      jacob = pipt2 * (2.d0*pi)*etacut2/shat 
     &     * (etacut2 * (el-ptcutl) / (8.d0*pi**2))**(n-1)
      
      return                                                                   
      end 
