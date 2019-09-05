function scalar, input
if(n_elements(input) gt 1) then begin
	message,'Input must have exactly 1 element',/info
	return, input
endif

d = datatype(input,1)
if(d eq 'Undefined') then begin
	message,'Cannot operate on undefined data types',/info
	return,input
endif
if(d eq 'Structure') then begin
	message,'Structures not supported',/info
	return,input
endif
if(d eq 'Pointer') then begin
	message,'Pointers not supported',/info
	return, input
endif
if(d eq 'Object') then begin
	message,'Objects not supported',/info
	return, input
endif
if(d eq 'Byte') then output=0B
if(d eq 'Integer') then output=0
if(d eq 'Long') then output=0L
if(d eq 'Float') then output=0.0
if(d eq 'Double') then output=0.0d0
if(d eq 'Complex') then output = complex(0.0,0.0)
if(d eq 'DComplex') then output = dcomplex(0.0d0,0.0d0)
if(d eq 'String') then output=''

output = input(0)
return, output
end

