      double precision ptcut
      parameter (ptcut = 20.d0)
      double precision etacut
      parameter (etacut = 2.5d0)
      double precision drcut
      parameter (drcut = 0.7d0)
        
      double precision whatmin, taulmin
      common /ccuts/ whatmin, taulmin
      save /ccuts/
