FUNCTION MakePoly, NSides, CENTER=Center, POINTDOWN=PointDown, $
                   SIZE=Size, HELP=Help
;+
; NAME:     
;    MakePoly
;
; FUNCTION: 
;        Define a polygon as a vector of (x,y) coordinates, in a form
;           suitable for use by the USERSYM function.
;
; USAGE: 
;         Polygon = MakePoly ( NSides, CENTER=Center, $
;                                POINTDOWN=PointDown, SIZE=Size, $
;                                HELP=Help )
;
; INPUT ARGUMENTS:
;   NSides:  Number of sides.
;
; INPUT/OUTPUT ARGUMENTS: 
;       None.
;
; OUTPUT ARGUMENTS:
;   Polygon: 2xN array of vertex locations.
;
; OPTIONAL ARGUMENTS: None.
;
; KEYWORD ARGUMENTS:
;   Center:    2-element vector with location of center of polygon.
;              Default=(0,0)
;   PointDown: If 0 or not present, one side of the polygon will be
;              horizontal. Otherwise, one vertex will be the lowest
;              point and will be located directly beneath the center.
;   Size:      Length of the line from the center to the vertices.
;              Units are the same as used by IDL for plot symbols.
;   Help:      If present, accesses module help and returns.
;
; REQUIRED MODULES: None.
;
; SIDE EFFECTS: None.
;
; NOTES:
;
;------------------------------------------------------------------------

   IF ( KEYWORD_SET ( HELP ) ) THEN BEGIN
      PRINT, 'MakePoly: Define 2xN array of polygon vertices'
      PRINT, 'USAGE: MakePoly, NSides, Polygon, CENTER=Center, $'
      PRINT, '                 POINTDOWN=PointDown, SIZE=Size,
HELP=Help'
      PRINT, 'NSides (I) = Number of sides  Polygon (O) = 2xN array'
      PRINT, 'Center = Location of center, default (0,0)'
      PRINT, 'PointDown = 0 ==> flat side down = 1 ==> vertex down'
      PRINT, 'Size = distance from center to vertex. Default = 1'
      PRINT, '       Units are the same as used by IDL for plot symbols'
      Polygon = 0
      GOTO, AllDone
   ENDIF

   IF ( NOT ( KEYWORD_SET ( Center ) ) ) THEN Center = [ 0., 0. ]
   IF ( NOT ( KEYWORD_SET ( Size   ) ) ) THEN Size   = 1.0

   DTheta = 2.D0 * !DPi / DOUBLE ( NSides )

   Theta = DINDGEN ( NSides ) * DTheta

   IF (  ( NSides MOD 2 ) EQ 1 ) THEN BEGIN
      IF ( NOT ( KEYWORD_SET ( PointDown ) ) ) $
      THEN Theta = Theta + 0.5 * Theta
   ENDIF ELSE IF ( KEYWORD_SET ( PointDown ) ) THEN BEGIN
      Theta = Theta + 0.5 * Theta
   ENDIF

   X = Center(0) + Size * COS ( Theta )
   Y = Center(1) + Size * SIN ( Theta )

   Polygon = TRANSPOSE ( [ [X], [Y] ] )

AllDone:

   RETURN, Polygon

END




