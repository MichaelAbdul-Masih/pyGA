; $Id: uniq.pro,v 1.5 1998/01/15 18:44:34 scottm Exp $
;
; Copyright (c) 1988-1998, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.

;+
; NAME:
;	NOTUNIQ
;
; PURPOSE:
;	Return the subscripts of the notunique elements in an array.
;
;	Note that repeated elements must be adjacent in order to be
;	found.  This routine is intended to be used with the SORT
;	function.  See the discussion of the IDX argument below.
;
;	This command is inspired by the Unix uniq(1) command.
;
; CATEGORY:
;	Array manipulation.
;
; CALLING SEQUENCE:
;	NOTUNIQ(Array [, Idx])
;
; INPUTS:
;	Array:	The array to be scanned.  The type and number of dimensions
;		of the array are not important.  The array must be sorted
;		into monotonic order unless the optional parameter Idx is 
;		supplied.
;
; OPTIONAL INPUT PARAMETERS:
;	IDX:	This optional parameter is an array of indices into Array
;		that order the elements into monotonic order.
;		That is, the expression:
;
;			Array(Idx)
;
;		yields an array in which the elements of Array are
;		rearranged into monotonic order.  If the array is not
;		already in monotonic order, use the command:
;
;			NOTUNIQ(Array, SORT(Array))
;
;		The expression below finds the notunique elements of an unsorted
;		array:
;
;			Array(NOTUNIQ(Array, SORT(Array)))
;
; OUTPUTS:
;	An array of indicies into ARRAY is returned.  The expression:
;
;		ARRAY(NOTUNIQ(ARRAY))
;
;	will be a copy of the sorted Array with duplicate adjacent
;	elements removed.
;
; COMMON BLOCKS:
;	None.
;
; MODIFICATION HISTORY:
;	1988, AB, Written.
;	29 July 1992, ACY - Corrected for case of all elements the same.
;	Nov, 1995.  DMS, Return a 0 if argument is a scalar.
;       Jul 27 1999, created as exact opposite to intrinsic IDL UNIQ
;       function, Seb Oliver
;
;-
;

function NOTUNIQ, ARRAY, IDX

; Check the arguments.
  s = size(ARRAY)
  if (s[0] eq 0) then return, 0		;A scalar
  if n_params() ge 2 then begin		;IDX supplied?
     q = array[idx]
     indices = where(q eq shift(q,-1), count)
     if (count GT 0) then return, idx[indices] $
     else return,-1
  endif else begin
     indices = where(array eq shift(array, -1), count)
     if (count GT 0) then return, indices $
     else return, -1
  endelse
end
