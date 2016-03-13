	integer nini, nfin, nexternal
	parameter (nini=2,nfin=2,nexternal=nini+nfin)

	integer ndim
	parameter (ndim=3*nfin-2+1)
	integer nwild
	parameter (nwild=ndim)

	integer pdg(2)
	real*8 fac_scale(2),ren_scale
	common /pdf/ pdg,fac_scale,ren_scale