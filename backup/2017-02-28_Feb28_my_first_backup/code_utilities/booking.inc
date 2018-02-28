
      integer nbintypT(nvarMAX)

      integer nbin(nvarMAX,nbinvarMAX,0:extypMAX) 
      real hmin(nvarMAX,nbinvarMAX,0:extypMAX)
      real hmax(nvarMAX,nbinvarMAX,0:extypMAX)

      character*7 bintag(nvarMAX,nbinvarMAX)

      COMMON /booking/ nbintypT, 
     |                 nbin, hmin, hmax,
     |                 bintag
