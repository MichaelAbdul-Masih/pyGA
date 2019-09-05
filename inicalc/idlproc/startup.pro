!path=!path+':.:/home/klein/uh101aw/idlproc:/home/klein/uh101aw/idlproc/miguel:/home/klein/uh101aw/idlproc/textoidl:/home/klein/uh101aw/idlproc/elias_idl/graphics:/home/klein/uh101aw/massloss/idl_lib:/home/klein/uh101aw/massloss/idl_lib/ourprog/tasho/in_out:/home/klein/uh101aw/massloss/idl_lib/astron/pro'
!path =  !path + ':' + $
          expand_path('+/home/klein/uh101aw/idlproc/coyote')
device,retain=2,decompose=0,true_color=24
.run convol
.run /home/klein/uh101aw/massloss/idl_lib/astron/pro/readfits
