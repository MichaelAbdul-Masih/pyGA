function get_rev, raster

if(tag_exist(raster,'SSCD_NAME')) then begin
	junk = raster.sscd_name
	itest = strpos(junk, 'CSSCCSS')
	if(itest(0) ne -1) then begin
		rev = fix(strmid(junk,itest(0)+7,3))
		return, rev
	endif
	itest = strpos(junk, 'CSSC')
	if(itest(0) ne -1) then begin
		rev = fix(strmid(junk,itest(0)+4,3))
		return, rev
	endif
endif

if(tag_exist(raster,'SADRASTER_NAME')) then begin
	junk = raster.sadraster_name
	itest = strpos(junk, 'CSAD')
	if(itest(0) ne -1) then begin
		rev = fix(strmid(junk,itest(0)+4,3))
		return, rev
	endif
endif

if(tag_exist(raster,'SAD_NAME')) then begin
	junk = raster.sadraster_name
	itest = strpos(junk, 'CSAD')
	if(itest(0) ne -1) then begin
		rev = fix(strmid(junk,itest(0)+4,3))
		return, rev
	endif
endif

;;; Last resort - get rev from current directory

message,'WARNING: unable to get revolution number from raster',/info
message,'WARNING: Using current working directory',/info

spawn,'pwd',current_dir
current_dir = scalar(current_dir)

itest = strpos(current_dir,'p0')
if(itest(0) ne -1) then begin
	junk = strmid(current_dir,itest(0),strlen(current_dir))
	itest = strpos(junk, '/')
	if(itest(0) ne -1) then begin
		rev = fix(strmid(junk, itest(0)+1,3))
		return, rev
	endif
endif

print,'ERROR: Unable to get revolution number; setting rev=0'
return, 0
end


