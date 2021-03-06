

c Define common block containing all coupling constants and masses
c  which are used in the HELAS routines.
c
c  These include masses, widths and real/complex couplings.
c
c  This file can be built automatically from particles.dat and
c  interactions.dat
c
c====================================================================
c
c
c
c     QCD 
c
      double complex       gg(2)  
      double precision     alpha,ee, sin2w, gfermi, alfas,g
      common /COUPL_BASIC/ alpha,ee, sin2w, gfermi, alfas,g,gg  
c
c    kinematical masses
c
      double precision     hmass, wmass, zmass, amass,
     &                     tmass, bmass, lmass, cmass
      common /COUPL_MASS/  hmass, wmass, zmass, amass,
     &                     tmass, bmass, lmass, cmass
c
c   widths
c
      double precision     hwidth, wwidth, zwidth, 
     &                     twidth, lwidth, awidth
      common /COUPL_WIDTH/ hwidth, wwidth, zwidth, 
     &                     twidth, lwidth, awidth
c
c  couplings in the feynman rules
c
      double complex       gal(2), gad(2), gau(2), gwf(2),
     &                     gzn(2), gzl(2), gzd(2), gzu(2)
      double precision     gw, gwwa, gwwz
      common /COUPL_GAUGE/ gal   , gad   , gau   , gwf   ,
     &                     gzn   , gzl   , gzd   , gzu   ,
     &                     gw, gwwa, gwwz
c
      double complex       gwfc(2),  gwfs(2), gwfm(2)
      common /coupl_ckm/   gwfc,     gwfs   , gwfm
c
      double complex       gwwh, gzzh, gwwhh, gzzhh, ghhh, ghhhh
      common /COUPL_SCAL/  gwwh, gzzh, gwwhh, gzzhh, ghhh, ghhhh
c
      double complex       ghtop(2), ghbot(2), ghtau(2), ghcha(2)
      common /COUPL_YUK/   ghtop   , ghbot   , ghtau   , ghcha

      double precision     xzmass, xwmass
      common /COUPL_XMASS/ xzmass, xwmass

      double complex       xzl(2) , xzb(2) , xzt(2) ,
     &                     xwpq(2), xwmq(2), xwpl(2), xwml(2)
      common /COUPL_XFFV/  xzl    , xzb    , xzt    ,
     &                     xwpq   , xwmq   , xwpl   , xwml

      double complex       xzhz, xwhwp, xwhwm
      common /COUPL_XVSS/  xzhz, xwhwp, xwhwm

      double complex       xwzwp, xwzwm, xwawp, xwawm
      common /COUPL_XVVS/  xwzwp, xwzwm, xwawp, xwawm

      double complex       xwzhwp, xwzhwm, xwahwp, xwahwm
      common /COUPL_XVVSS/ xwzhwp, xwzhwm, xwahwp, xwahwm

      double complex   GG2(2), GG0(2), G2I,GC_4
      common /CFQCD_COUPL1/ GG2, GG0, G2I, GC_4

      double precision G2
      common /CFQCD_COUPL/ G2
