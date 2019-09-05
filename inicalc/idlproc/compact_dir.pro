pro compact_dir,dir

  spawn,'rm '+dir+'/CONT_FORMAL*'
  spawn,'rm '+dir+'/ENION'
  spawn,'rm '+dir+'/*POP'
  spawn,'rm '+dir+'/MAX_CHANGE'
  spawn,'rm '+dir+'/METAL_RESTART'
  spawn,'rm '+dir+'/MODEL'
  spawn,'rm '+dir+'/TEMP'
  spawn,'rm '+dir+'/*LA.dat'
  spawn,'rm '+dir+'/TAU_ROS'
    
return
end
