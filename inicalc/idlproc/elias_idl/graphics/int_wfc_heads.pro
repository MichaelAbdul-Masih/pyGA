  pro int_wfc_heads,ra,dec,h1,h2,h3,h4,small=small

  scale=1./cos(dec*!dtor)

  dra=[5.,-1043.,-10.,0.]*scale/3600.
  ddec=[-704.,-357,701.,0.]/3600.

  ra1=ra+dra(0)
  ra2=ra+dra(1)
  ra3=ra+dra(2)
  ra4=ra+dra(3)

  dec1=dec+ddec(0)
  dec2=dec+ddec(1)
  dec3=dec+ddec(2)
  dec4=dec+ddec(3)

  w=4128
  h=2148
  pix=0.33/3600.
  
  if keyword_set(small) then begin 
     w=round(w/10)
     h=round(h/10)
     pix=pix*10.
   endif


  icmkhdr,h,w,-pix,pix,ra1,dec1,90.,h1
  icmkhdr,h,w,-pix,pix,ra2,dec2,0.,h2
  icmkhdr,h,w,-pix,pix,ra3,dec3,90.,h3
  icmkhdr,h,w,-pix,pix,ra4,dec4,90.,h4
;stop
  end


