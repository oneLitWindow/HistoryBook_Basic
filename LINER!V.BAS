DECLARE FUNCTION xory! ()
DECLARE SUB get1 (n$)
DECLARE SUB put1 (n$)
DECLARE SUB form01 ()
DECLARE SUB form02 ()
DECLARE SUB form03 ()
DECLARE SUB form04 ()
DECLARE SUB form05 ()
DECLARE SUB form06 ()
DECLARE SUB form07 ()
DECLARE SUB form08 ()
DECLARE SUB form09 ()
DECLARE SUB form10 ()
DECLARE SUB form11 ()
DECLARE SUB form12 ()
DECLARE SUB x80x60 ()
DECLARE SUB ctr4 (t$)
DECLARE SUB ctr5 (t$)
DECLARE SUB coloris (red!, green!, blue!)
DECLARE SUB getanswer (alpha!, betta!, gamma!, zetta!, rr!, gg!, bb!)
DECLARE FUNCTION getform! (rr!, gg!, bb!)
DECLARE FUNCTION sincostanlog! ()
DECLARE SUB exitnes2 ()
DECLARE SUB endzoom (ook!, ook2!)
DECLARE SUB axisv (ook!, ook2!)
DECLARE SUB exitnes ()
DECLARE SUB grid ()
DECLARE SUB getpix (x!, y!)
DECLARE SUB zoom2 ()
DECLARE FUNCTION whatdo! ()
DECLARE FUNCTION order! ()
DECLARE SUB modify ()
DECLARE FUNCTION runable! ()
DECLARE SUB ctr2 (t$)
DECLARE SUB ctr (t$)
DECLARE SUB print2 (x$, x!, c!)
DECLARE FUNCTION inputstr$ (LENgth!, get$, tpr!, ttr!, tc!)
DECLARE SUB cadr (x1!, y1!, x2!, y2!, dly!, colr!)
DECLARE SUB getakey ()
DECLARE SUB initcadr (colr!)
DECLARE SUB ctr3 (x!, t$)
DECLARE SUB delay (x!)
DECLARE SUB circles (x!, y!, R!, c!, dly!)
DECLARE FUNCTION keypressed! ()
DECLARE SUB setrgbpalette (x!, R!, G!, B!)
DECLARE FUNCTION date2$ ()
DECLARE SUB getkeys ()
DECLARE FUNCTION str2$ (x!)
DECLARE FUNCTION str3$ (x AS LONG)
DECLARE FUNCTION time2$ ()
ON ERROR RESUME NEXT
SCREEN 12
'OPEN "MRHamid.dat" FOR INPUT AS #1
'INPUT #1, maxx
'INPUT #1, minx
'INPUT #1, ysc
'INPUT #1, alpha
'INPUT #1, betta
'INPUT #1, gamma
'INPUT #1, zetta
'CLOSE #1
'gamma = gamma - zetta
'zetta = 0
'aa = -betta / alpha
'bb = -gamma / alpha
'cgc = (-gamma / alpha)
'ss$ = str2$(-betta / alpha)
'GOTO datareadbefor
'gamma = 0
'zetta = 0
'betta = 0
'alpha = 0
initcadr 11
cadr 0, 0, 640, 480, 0, 11
COLOR 14
ctr3 15, "Liner IV By Amin Safinejad - 2000. Hit any key..."
ctr3 17, "Downloaded from www.safinejad.com - 2008"
getakey
ctr3 15, "                                                 "
ctr3 17, "                                                 "
initcadr 10
LOCATE 2, 1
datareadbefor:
refresh:
CLS 0
VIEW PRINT 1 TO 30


LINE (160 - (zoom), 120 - (zoom))-(320 + 160 + (zoom - 1), 240 + 120 + (zoom)), 14, B

PAINT (161, 121), 2, 14

'LINE (160, 120)-(320 + 160, 240 + 120), 14, B
maxx = 640
minx = 0
ysc = 40
axis = (ABS(maxx - minx) / 640) + 10
ayis = ysc
zoom = 1

PAINT (1, 1), 1, 14
VIEW PRINT 1 TO 30
VIEW SCREEN (160 - 1, 120 - 1)-(160 + 320, 120 + 241), 2, 14
'LINE (160, 120)-(160 + 320, 120 + 240), 0, BF

LINE (160 - (zoom), 120 - (zoom))-(320 + 160 + (zoom - 1), 240 + 120 + (zoom)), 14, B
coloris rr, gg, bb
reorder:
grid
LINE (160 - (zoom), 120 - (zoom))-(320 + 160 + (zoom - 1), 240 + 120 + (zoom)), 14, B
COLOR 13

getanswer alpha, betta, gamma, zetta, rr, gg, bb

repof:
o = order
IF o = 1 THEN modify: GOTO repof
IF o = 2 THEN LINE (160 - 1, 120 - 1)-(160 + 320, 120 + 241), 2, BF: GOTO refreshdata
IF o = 3 THEN GOTO refreshdata
IF o = 4 THEN exitnes: GOTO repof

refreshdata:
'getanswer alpha, betta, gamma, zetta
'gamma = gamma - zetta
'zetta = 0
'aa = -betta / alpha
'bb = -gamma / alpha
'cgc = (-gamma / alpha)
'ss$ = str2$(-betta / alpha)
'IF ss$ = "-1" THEN ss$ = "-"
'IF ss$ = "-1" OR ss$ = "1" THEN ss$ = ""
'moad$ = "Y = " + ss$ + "X "
'IF cgc < 0 THEN moad$ = moad$ + "- " + str2$(ABS(cgc))
'IF cgc > 0 THEN moad$ = moad$ + "+ " + str2$(cgc)
GOTO reorder

SUB axisv (ook, ook2)
pr = 5
DIM table(250 * pr)
VIEW SCREEN (160 - pr - 1, 120 - pr - 1)-(160 + 320 + pr, 120 + 241 + pr)', table
x = 320
y = 240
xo = x
yo = y
GET (x - pr, y - pr)-(x + pr, y + pr), table
p = POINT(x, y)
LOCATE 1, 1
PRINT "000,000"
br = 1
rd = 0
DO
'rd = rd + br
'IF rd >= 63 THEN br = -1
'IF rd <= 0 THEN br = 1
'LOCATE 2, 1: PRINT br; rd
'IF rd MOD 9 = 0 THEN PALETTE 13, (63 - rd) + 256 ^ 2 * rd
'SOUND 0, .1
x$ = str2$(x - 320)
y$ = str2$(y - 240)
p$ = str2$(p)
IF p$ = "2" THEN p$ = "0"
x10d1: IF LEN(x$) < 4 THEN x$ = " " + x$: GOTO x10d1
y10d1: IF LEN(y$) < 4 THEN y$ = " " + y$: GOTO y10d1
p10d1: IF LEN(p$) < 2 THEN p$ = "0" + p$: GOTO p10d1
a$ = INKEY$
IF a$ = CHR$(0) + CHR$(72) THEN y = y - 1'ook2 \ 2
IF a$ = CHR$(0) + CHR$(80) THEN y = y + 1'ook2 \ 2
IF a$ = CHR$(0) + CHR$(75) THEN x = x - 1'ook \ 2
IF a$ = CHR$(0) + CHR$(77) THEN x = x + 1'ook
IF y > 120 + 240 THEN y = 120 + 240
IF x > 160 + 320 - 1 THEN x = 160 + 320 - 1
IF y < 240 - 120 THEN y = 240 - 120
IF x < 320 - 160 THEN x = 320 - 160
IF xo <> x OR yo <> y THEN
LINE (xo - pr, yo - pr)-(xo + pr, yo + pr), 0, BF

PUT (xo - pr, yo - pr), table, XOR

LINE (xo - 2.5, 120 - 1)-(xo + 2.5, 120 - 1), 14
LINE (160 - 1, yo - 2.5)-(160 - 1, yo + 2.5), 14
LINE (xo - 2.5, 240 + 120 + 1)-(xo + 2.5, 240 + 120 + 1), 14
LINE (320 + 160, yo - 2.5)-(320 + 160, yo + 2.5), 14
PSET (xo, yo), p
GET (x - pr, y - pr)-(x + pr, y + pr), table
END IF
IF xo <> x THEN PSET (xo, yo), p: xo = x: p = POINT(x, y)
IF yo <> y THEN PSET (xo, yo), p: yo = y: p = POINT(x, y)
'LINE (x, 240 - 120)-(x, 240 + 120), 13
'LINE (320 - 160, y)-(320 + 160, y), 13
'PSET (x, y), RND * 15
LINE (x - pr, y)-(x + pr, y), 14
LINE (x, y - pr)-(x, y + pr), 14

LINE (x - 2.5, 120 - 1)-(x + 2.5, 120 - 1), 0
LINE (160 - 1, y - 2.5)-(160 - 1, y + 2.5), 0
LINE (x - 2.5, 240 + 120 + 1)-(x + 2.5, 240 + 120 + 1), 0
LINE (320 + 160, y - 2.5)-(320 + 160, y + 2.5), 0

COLOR 10
LOCATE 1, 1
PRINT x$; ","; y$; " : "; p$; "  "
IF a$ = CHR$(13) THEN EXIT DO
IF a$ = CHR$(27) THEN x = -100: EXIT DO
LOOP
COLOR 0
LOCATE 1, 1
PRINT x$; ","; y$; " : "; p$; "  "
COLOR 10
'LINE (xo - pr, yo - pr)-(xo + pr, yo + pr), 0, BF
'PUT (xo - pr, yo - pr), table, XOR

'LINE (x - pr, y)-(x + pr, y), 13
'LINE (x, y - pr)-(x, y + pr), 13
LINE (xo - 2.5, 120 - 1)-(xo + 2.5, 120 - 1), 14
LINE (160 - 1, yo - 2.5)-(160 - 1, yo + 2.5), 14
LINE (xo - 2.5, 240 + 120 + 1)-(xo + 2.5, 240 + 120 + 1), 14
LINE (320 + 160, yo - 2.5)-(320 + 160, yo + 2.5), 14
LINE (xo - pr, yo - pr)-(xo + pr, yo + pr), 0, BF

PUT (xo - pr, yo - pr), table, XOR


END SUB

SUB cadr (x1, y1, x2, y2, dly, colr)
x1 = x1 + 6
y1 = y1 + 6
x2 = x2 - 7
y2 = y2 - 7
IF dly <> 0 THEN
FOR dd = x1 TO x2 - 1
PSET (dd, y1), colr
dd2 = (x2 - dd) '+ 6
PSET (dd2 + x1, y2), colr
SOUND 0, dly
NEXT
FOR dd = y1 TO y2 - 1
PSET (x2, dd), colr
dd2 = (y2 - dd) '+ 6
PSET (x1, dd2 + y1), colr
SOUND 0, dly
NEXT
END IF
LINE (x1, y1)-(x2, y2), colr, B
FOR i = 1 TO 5
j = i
COLOR j
LINE (x1 - i - 1, y1 - i - 1)-(x2 + i + 1, y2 + i + 1), j, B
NEXT
END SUB

SUB circles (x, y, R, c, dly)
CIRCLE (x, y), R, c
FOR t = 1 TO 4 STEP 1
'IF c = 10 THEN setrgbpalette t, 0, INT(t * 6.3 * 2), 0
'IF c = 11 THEN setrgbpalette t, 0, INT(t * 6.3 * 2), INT(t * 6.3 * 2)
'IF c = 12 THEN setrgbpalette t, INT(t * 6.3 * 2), 0, 0
'IF c = 13 THEN setrgbpalette t, INT(t * 6.3 * 2), 0, INT(t * 6.3 * 2)
'IF c = 14 THEN setrgbpalette t, INT(t * 6.3 * 2), INT(t * 6.3 * 2), 0
'IF c = 15 THEN setrgbpalette t, INT(t * 6.3 * 2), INT(t * 6.3 * 2), INT(t * 6.3 * 2)
CIRCLE (x, y), R + t + 1, t
IF dly <> 0 THEN delay (dly)
NEXT
END SUB

SUB clrscr
FOR t = 0 TO 640 STEP 4
LINE (t, 0)-(t, 480), 0
LINE (t + 1, 0)-(t + 1, 480), 0
LINE (t + 2, 0)-(t + 2, 480), 0
SOUND 0, .05
NEXT
FOR t = 639 TO 1 STEP -4
LINE (t, 0)-(t, 480), 0
LINE (t - 1, 0)-(t - 1, 480), 0
LINE (t - 2, 0)-(t - 2, 480), 0
SOUND 0, .05
NEXT
END SUB

SUB coloris (red, green, blue)
LOCATE 25
'"ปบผอศษฯัณ"
COLOR 14
'ctr2 "ษออออออออออออออัอออออออป"
'ctr2 "บ Choose Color ณ  Red  บ"
'ctr2 "บ ÛÛÛÛÛÛÛÛÛÛÛÛ ณ Green บ"
'ctr2 "บ ÛÛÛÛÛÛÛÛÛÛÛÛ ณ Blue  บ"
'ctr2 "ศออออออออออออออฯอออออออผ"
'LOCATE 29, 49: PRINT "*": END
'44
'COLOR 2
'LOCATE 27, 30
'PRINT "ÛÛÛÛÛÛÛÛÛÛÛÛ"
'LOCATE , 30
'PRINT "ÛÛÛÛÛÛÛÛÛÛÛÛ"
'COLOR 0: LOCATE 30, 1
setrgbpalette 1, 0, 0, 0
lo = 0
l = 1
red = 0
green = 0
blue = 50
red$ = "000"
blue$ = "079"
green$ = "000"
setrgbpalette 2, red, green, blue
DO
a$ = INKEY$
IF a$ <> "" THEN setrgbpalette 2, red, green, blue
IF lo <> l THEN
LOCATE 25
lo = l
'"ปบผอศษฯัณ"
COLOR 14
                                            ctr2 "ษอออออออออออออออัออออออออออออป"
                                            ctr2 "บ Choose Color: ณ Red   " + red$ + "% บ": LOCATE 27, 25
PRINT "บ "; : COLOR 2: PRINT "ÛÛÛÛÛÛÛÛÛÛÛÛÛ"; : COLOR 14: PRINT " ณ Green " + green$ + "% บ": LOCATE , 25
PRINT "บ "; : COLOR 2: PRINT "ÛÛÛÛÛÛÛÛÛÛÛÛÛ"; : COLOR 14: PRINT " ณ Blue  " + blue$ + "% บ"
                                            ctr2 "ศอออออออออออออออฯออออออออออออผ"

COLOR 2
LOCATE 27, 28
PRINT "ÛÛÛÛÛÛÛÛÛÛÛÛ"
LOCATE , 28
PRINT "ÛÛÛÛÛÛÛÛÛÛÛÛ"
END IF
IF a$ = CHR$(0) + CHR$(77) THEN
IF l = 1 THEN red = red + 1
IF l = 2 THEN green = green + 1
IF l = 3 THEN blue = blue + 1
END IF
IF a$ = CHR$(0) + CHR$(75) THEN
IF l = 1 THEN red = red - 1
IF l = 2 THEN green = green - 1
IF l = 3 THEN blue = blue - 1
END IF
IF a$ = CHR$(0) + CHR$(72) THEN l = l - 1
IF a$ = CHR$(0) + CHR$(80) THEN l = l + 1
IF red < 0 THEN red = 0
IF red > 63 THEN red = 63
IF green < 0 THEN green = 0
IF green > 63 THEN green = 63
IF blue < 0 THEN blue = 0
IF blue > 63 THEN blue = 63
IF l < 1 THEN l = 3
IF l > 3 THEN l = 1
IF l = 1 THEN COLOR 12: LOCATE 26, 42: PRINT "þRed   "; red$; "%"
IF l = 2 THEN COLOR 10: LOCATE 27, 42: PRINT "þGreen "; green$; "%"
 IF l = 3 THEN COLOR 9: LOCATE 28, 42: PRINT "þBlue  "; blue$; "%"
red$ = str2$((red * 100) \ 63)
green$ = str2$((green * 100) \ 63)
blue$ = str2$((blue * 100) \ 63)
redd: IF LEN(red$) < 3 THEN red$ = "0" + red$: GOTO redd
gre: IF LEN(green$) < 3 THEN green$ = "0" + green$: GOTO gre
blu: IF LEN(blue$) < 3 THEN blue$ = "0" + blue$: GOTO blu
LOCATE 29, 48
IF a$ = CHR$(27) OR a$ = CHR$(13) THEN EXIT DO
LOOP
LOCATE 25
ctr2 "                              "
ctr2 "                              "
ctr2 "                              "
ctr2 "                              "
ctr2 "                              "

END SUB

SUB ctr (t$)
LOCATE , (80 - LEN(t$)) \ 2
PRINT t$;
END SUB

SUB ctr2 (t$)
LOCATE , (80 - LEN(t$)) \ 2
PRINT t$
END SUB

SUB ctr3 (x, t$)
LOCATE x, (80 - LEN(t$)) \ 2
PRINT t$;
END SUB

SUB ctr4 (t$)
LOCATE , ((80 - LEN(t$)) \ 2) + 1
PRINT t$
END SUB

SUB ctr5 (t$)
LOCATE , ((80 - LEN(t$)) \ 2) + 1
PRINT t$;

END SUB

FUNCTION date2$
mounth = VAL(DATE$)
tgt = INSTR(DATE$, "-")
day = VAL(MID$(DATE$, tgt + 1))
tgt2 = INSTR(MID$(DATE$, tgt + 1), "-")
year = VAL(MID$(MID$(DATE$, tgt + 1), tgt2 + 1))
gg$ = LTRIM$(RTRIM$(STR$(hour)))
ggoy$ = str2$(year)
ggo2:
IF LEN(ggoy$) < 4 THEN ggoy$ = "0" + ggoy$: GOTO ggo2
ggom$ = str2$(mounth)
ggo3:
IF LEN(ggom$) < 2 THEN ggom$ = "0" + ggom$: GOTO ggo3
ggod$ = str2$(day)
ggo4:
IF LEN(ggod$) < 2 THEN ggod$ = "0" + ggod$: GOTO ggo4

date2$ = ggoy$ + "/" + ggom$ + "/" + ggod$
END FUNCTION

SUB delay (x)
FOR xx = 0 TO x STEP .1
SOUND 0, .1
NEXT
END SUB

SUB draws (x, y, z, c)
circles INT(x), INT(y), INT(z), INT(c), 2
END SUB

SUB endzoom (ook, ook2)
helmont:
LOCATE 24, 1
COLOR 13
ctr2 "                                  "
ctr2 "                                  "
ctr2 "ษออออออออออออออออออออออออออออออออป"
ctr2 "วฤฤฤฤฤฤฤฤ Zoom Finished ฤฤฤฤฤฤฤฤฤถ"
ctr2 "บ þ Pixel Axis , Ayis Viewer     บ"
ctr2 "บ þ Zoom Back and Exit           บ"
 ctr "ศออออออออออออออออออออออออออออออออผ"
l = 1
lo = 1
DO
a$ = INKEY$
IF a$ = CHR$(0) + CHR$(72) THEN l = l - 1
IF a$ = CHR$(0) + CHR$(80) THEN l = l + 1
IF l < 1 THEN l = 2
IF l > 2 THEN l = 1
IF l <> lo THEN
lo = l
LOCATE 24, 1
COLOR 13
ctr2 "                                  "
ctr2 "                                  "
ctr2 "ษออออออออออออออออออออออออออออออออป"
ctr2 "วฤฤฤฤฤฤฤฤ Zoom Finished ฤฤฤฤฤฤฤฤฤถ"
ctr2 "บ þ Pixel Axis , Ayis Viewer     บ"
ctr2 "บ þ Zoom Back and Exit           บ"
 ctr "ศออออออออออออออออออออออออออออออออผ"
END IF
LOCATE 27 + l
COLOR 10
IF l = 1 THEN ctr " ฏ Pixel Axis , Ayis Viewer     "
IF l = 2 THEN ctr " ฏ Zoom Back and Exit           "
IF a$ = CHR$(13) THEN EXIT DO
IF a$ = CHR$(27) THEN l = 2: EXIT DO
LOOP
LOCATE 24, 1
COLOR 0
ctr2 "                                  "
ctr2 "                                  "
ctr2 "ษออออออออออออออออออออออออออออออออป"
ctr2 "วฤฤฤฤฤฤฤฤ Zoom Finished ฤฤฤฤฤฤฤฤฤถ"
ctr2 "บ þ Pixel Axis , Ayis Viewer     บ"
ctr2 "บ þ Zoom Back and Exit           บ"
 ctr "ศออออออออออออออออออออออออออออออออผ"
IF l = 1 THEN axisv ook, ook2: GOTO helmont

END SUB

SUB exitnes
DIM otix(5000)
VIEW SCREEN (0, 0)-(639, 479)
GET (320 - 160, 240 + 125)-(320 + 160, 479), otix
LOCATE 24, 1
COLOR 13
ctr2 "                                  "
ctr2 "                                  "
ctr2 "ษออออออออออออออออออออออออออออออออป"
ctr2 "วฤฤฤฤฤฤฤฤ Exit Program? ฤฤฤฤฤฤฤฤฤถ"
ctr2 "บ þ Yes , Leave Liner.           บ"
ctr2 "บ þ No! , Continue Program.      บ"
 ctr "ศออออออออออออออออออออออออออออออออผ"
l = 1
lo = 1
DO
a$ = INKEY$
IF a$ = CHR$(0) + CHR$(72) THEN l = l - 1
IF a$ = CHR$(0) + CHR$(80) THEN l = l + 1
IF l < 1 THEN l = 2
IF l > 2 THEN l = 1
IF l <> lo THEN
lo = l
LOCATE 24, 1
COLOR 13
ctr2 "                                  "
ctr2 "                                  "
ctr2 "ษออออออออออออออออออออออออออออออออป"
ctr2 "วฤฤฤฤฤฤฤฤ Exit Program? ฤฤฤฤฤฤฤฤฤถ"
ctr2 "บ þ Yes , Leave Liner.           บ"
ctr2 "บ þ No! , Continue Program.      บ"
 ctr "ศออออออออออออออออออออออออออออออออผ"
END IF
LOCATE 27 + l
COLOR 10
IF l = 1 THEN ctr " ฏ Yes , Leave Liner.           "
IF l = 2 THEN ctr " ฏ No! , Continue Program.      "
IF a$ = CHR$(13) THEN EXIT DO
IF a$ = CHR$(27) THEN l = 2: EXIT DO
LOOP
IF l = 1 THEN SCREEN 0: END
LINE (320 - 160, 240 + 125)-(320 + 160, 479), 0, BF
PUT (320 - 160, 240 + 125), otix
VIEW SCREEN (160 - pr - 1, 120 - pr - 1)-(160 + 320 + pr, 120 + 241 + pr), , 14
END SUB

SUB exitnes2
LOCATE 2, 1
COLOR 13
ctr2 "ษออออออออออออออออออออออออออออออออป"
ctr2 "วฤฤฤฤฤฤฤฤ Exit Program? ฤฤฤฤฤฤฤฤฤถ"
ctr2 "บ þ Yes , Leave Liner.           บ"
ctr2 "บ þ No! , Continue Program.      บ"
 ctr "ศออออออออออออออออออออออออออออออออผ"
l = 1
lo = 1
DO
a$ = INKEY$
IF a$ = CHR$(0) + CHR$(72) THEN l = l - 1
IF a$ = CHR$(0) + CHR$(80) THEN l = l + 1
IF l < 1 THEN l = 2
IF l > 2 THEN l = 1
IF l <> lo THEN
lo = l
LOCATE 2, 1
COLOR 13
ctr2 "ษออออออออออออออออออออออออออออออออป"
ctr2 "วฤฤฤฤฤฤฤฤ Exit Program? ฤฤฤฤฤฤฤฤฤถ"
ctr2 "บ þ Yes , Leave Liner.           บ"
ctr2 "บ þ No! , Continue Program.      บ"
 ctr "ศออออออออออออออออออออออออออออออออผ"
END IF
LOCATE 3 + l
COLOR 10
IF l = 1 THEN ctr " ฏ Yes , Leave Liner.           "
IF l = 2 THEN ctr " ฏ No! , Continue Program.      "
IF a$ = CHR$(13) THEN EXIT DO
IF a$ = CHR$(27) THEN l = 2: EXIT DO
LOOP
IF l = 1 THEN SCREEN 0: END
LOCATE 2, 1
COLOR 0
ctr2 "ษออออออออออออออออออออออออออออออออป"
ctr2 "วฤฤฤฤฤฤฤฤ Exit Program? ฤฤฤฤฤฤฤฤฤถ"
ctr2 "บ þ Yes , Leave Liner.           บ"
ctr2 "บ þ No! , Continue Program.      บ"
 ctr "ศออออออออออออออออออออออออออออออออผ"

END SUB

SUB form01
LOCATE 24 * 2, 1
COLOR 13
ctr2 "ษอออออออ เY + แX + ็ = ๅ ออออออออป"
ctr2 "บ þ Enter เ:                     บ"
ctr2 "บ þ Enter แ:                     บ"
ctr2 "บ þ Enter ็:                     บ"
ctr2 "บ þ Enter ๅ:                     บ"
ctr2 "บ     Abort Program = ESCape     บ"
 ctr "ศออออออออออออออออออออออออออออออออผ"
rea:
LOCATE 49
print2 " ฏ Enter เ: ;", 24, 11
cong$ = inputstr$(6, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO rea
alpha = VAL(cong$)
LOCATE 49
print2 " ๛ Enter เ: ;", 24, 3
reb:
LOCATE 50
print2 " ฏ Enter แ: ;", 24, 11
cong$ = inputstr$(6, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO reb
betta = VAL(cong$)
LOCATE 50
print2 " ๛ Enter แ: ;", 24, 3
reg:
LOCATE 51
print2 " ฏ Enter ็: ;", 24, 11
cong$ = inputstr$(10, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO reg
gamma = VAL(cong$)
LOCATE 51
print2 " ๛ Enter ็: ;", 24, 3
rez:
LOCATE 52
print2 " ฏ Enter ๅ: ;", 24, 11
cong$ = inputstr$(10, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO rez
zetta = VAL(cong$)
LOCATE 52
print2 " ๛ Enter ๅ: ;", 24, 3

LOCATE 24 * 2, 1
COLOR 0
ctr2 "ษอออออออ เY + แX + ็ = ๅ ออออออออป"
ctr2 "บ þ Enter เ:                     บ"
ctr2 "บ þ Enter แ:                     บ"
ctr2 "บ þ Enter ็:                     บ"
ctr2 "บ þ Enter ๅ:                     บ"
ctr2 "บ     Abort Program = ESCape     บ"
 ctr "ศออออออออออออออออออออออออออออออออผ"
COLOR 10
gamma = gamma - zetta
zetta = 0
aa = -betta / alpha
bb = -gamma / alpha
cgc = (-gamma / alpha)
ss$ = str2$(-betta / alpha)
IF ss$ = "-1" THEN ss$ = "-"
IF ss$ = "-1" OR ss$ = "1" THEN ss$ = ""
moad$ = "Y = " + ss$ + "X "
IF cgc < 0 THEN moad$ = moad$ + "- " + str2$(ABS(cgc))
IF cgc > 0 THEN moad$ = moad$ + "+ " + str2$(cgc)
minx = 0
maxx = 640
ysc = 40

IF ss$ = "-1" OR ss$ = "1" THEN ss$ = ""
moad$ = "Y = " + ss$ + "X "
IF cgc < 0 THEN moad$ = moad$ + "- " + str2$(ABS(cgc))
IF cgc > 0 THEN moad$ = moad$ + "+ " + str2$(cgc)
LOCATE 1
ctr2 moad$ + " Form 01"

FOR x = -160 TO 160 STEP .01
'x = xt - 500
y = (aa * x) + bb
IF x = -160 THEN PSET (x + 320, (480 - y) - 240), 15 ELSE LINE -(x + 320, (480 - y) - 240), 15
NEXT

END SUB

SUB form02
'บ þ เY + แX + ็ = ๅ              บ
'บ þ Y  = เX + แ                  บ
'บ þ เY^แ = ็Sin ( ๅ X^้ ) + ๋    บ
'บ þ เY = แ X^็ + ๅ X^้ + ๋       บ
LOCATE 24 * 2, 1
COLOR 13
ctr2 "                                  "
ctr2 "                                  "
ctr2 "ษอออออออออ Y  = เX + แ ออออออออออป"
ctr2 "บ þ Enter เ:                     บ"
ctr2 "บ þ Enter แ:                     บ"
ctr2 "บ     Abort Program = ESCape     บ"
 ctr "ศออออออออออออออออออออออออออออออออผ"
rea1:
LOCATE 51
print2 " ฏ Enter เ: ;", 24, 11
cong$ = inputstr$(6, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO rea1
alpha = VAL(cong$)
LOCATE 51
print2 " ๛ Enter เ: ;", 24, 3
reb1:
LOCATE 52
print2 " ฏ Enter แ: ;", 24, 11
cong$ = inputstr$(6, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO reb1
betta = VAL(cong$)
LOCATE 52
print2 " ๛ Enter แ: ;", 24, 3

LOCATE 24 * 2, 1
COLOR 0
ctr2 "                                  "
ctr2 "                                  "
ctr2 "ษอออออออออ Y  = เX + แ ออออออออออป"
ctr2 "บ þ Enter เ:                     บ"
ctr2 "บ þ Enter แ:                     บ"
ctr2 "บ     Abort Program = ESCape     บ"
 ctr "ศออออออออออออออออออออออออออออออออผ"
COLOR 10
gamma = gamma - zetta
zetta = 0
aa = alpha
bb = betta
cgc = (bb)
ss$ = str2$(aa)
IF ss$ = "-1" THEN ss$ = "-"
IF ss$ = "-1" OR ss$ = "1" THEN ss$ = ""
moad$ = "Y = " + ss$ + "X "
IF cgc < 0 THEN moad$ = moad$ + "- " + str2$(ABS(cgc))
IF cgc > 0 THEN moad$ = moad$ + "+ " + str2$(cgc)
minx = 0
maxx = 640
ysc = 40

IF ss$ = "-1" OR ss$ = "1" THEN ss$ = ""
moad$ = "Y = " + ss$ + "X "
IF cgc < 0 THEN moad$ = moad$ + "- " + str2$(ABS(cgc))
IF cgc > 0 THEN moad$ = moad$ + "+ " + str2$(cgc)
LOCATE 1
ctr2 moad$ + " Form 02"

FOR x = -160 TO 160 STEP .01
'x = xt - 500
y = (aa * x) + bb
IF x = -160 THEN PSET (x + 320, (480 - y) - 240), 15 ELSE LINE -(x + 320, (480 - y) - 240), 15
NEXT


END SUB

SUB form03
DIM otex(2000)
VIEW SCREEN (0, 0)-(639, 479)
GET (320 - 160, 240 + 80)-(320 + 160, 240 + 122), otex
'บ þ เY + แX + ็ = ๅ              บ
'บ þ Y  = เX + แ                  บ
'บ þ เY^แ = ็Sin ( ๅ X^้ ) + ๋    บ
'บ þ เY = แ X^็ + ๅ X^้ + ๋       บ

LOCATE 22, 1
COLOR 13
ctr2 "ษออ เY^แ = ็Sin ( ๅ X^้ ) + ๋ อออป"
ctr2 "บ þ Enter เ:                     บ"
ctr2 "บ þ Enter แ:                     บ"
ctr2 "บ þ Enter ็:                     บ"
ctr2 "บ þ Sin , Cos , Tan , Log        บ"
ctr2 "บ þ Enter ๅ:                     บ"
ctr2 "บ þ Enter ้:                     บ"
ctr2 "บ þ Enter ๋:                     บ"
 ctr "ศอออออออออออออออออออออออออออ(ESC)ผ"
rea2:
LOCATE 23
print2 " ฏ Enter เ: ;", 24, 11
cong$ = inputstr$(6, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO rea2
alpha = VAL(cong$)
LOCATE 23
print2 " ๛ Enter เ: ;", 24, 3
reb2:
LOCATE 24
print2 " ฏ Enter แ: ;", 24, 11
cong$ = inputstr$(6, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO reb2
betta = VAL(cong$)
LOCATE 24
print2 " ๛ Enter แ: ;", 24, 3
reg2:
LOCATE 25
print2 " ฏ Enter ็: ;", 24, 11
cong$ = inputstr$(10, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO reg2
gamma = VAL(cong$)
LOCATE 25
print2 " ๛ Enter ็: ;", 24, 3
COLOR 11
LOCATE 26, 24: PRINT " ฏ ";
adv = sincostanlog
COLOR 3
LOCATE 26, 24: PRINT " ฏ ";

rez2:
LOCATE 27
print2 " ฏ Enter ๅ: ;", 24, 11
cong$ = inputstr$(10, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO rez2
zetta = VAL(cong$)
LOCATE 27
print2 " ๛ Enter ๅ: ;", 24, 3

ret2:
LOCATE 28
print2 " ฏ Enter ้: ;", 24, 11
cong$ = inputstr$(10, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO ret2
tetta = VAL(cong$)
LOCATE 28
print2 " ๛ Enter ้: ;", 24, 3

res2:
LOCATE 29
print2 " ฏ Enter ๋: ;", 24, 11
cong$ = inputstr$(10, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO res2
sigma = VAL(cong$)
LOCATE 29
print2 " ๛ Enter ๋: ;", 24, 3

LOCATE 22, 1
COLOR 0
ctr2 "ษออ เY^แ = ็Sin ( ๅ X^้ ) + ๋ อออป"
ctr2 "บ þ Enter เ:                     บ"
ctr2 "บ þ Enter แ:                     บ"
ctr2 "บ þ Enter ็:                     บ"
ctr2 "บ þ Sin , Cos , Tan , Log        บ"
ctr2 "บ þ Enter ๅ:                     บ"
ctr2 "บ þ Enter ้:                     บ"
ctr2 "บ þ Enter ๋:                     บ"
 ctr "ศอออออออออออออออออออออออออออ(ESC)ผ"
COLOR 10
IF adv = 1 THEN ss$ = "Sin"
IF adv = 2 THEN ss$ = "Cos"
IF adv = 3 THEN ss$ = "Tan"
IF adv = 4 THEN ss$ = "Log"
moad$ = "Y = " + str2$(betta) + "๛((" + str2$(gamma) + ss$ + "(" + str2$(zetta) + "X^" + str2$(tetta) + ") + " + str2$(sigma) + ") / " + str2$(alpha) + ")"

minx = 0
maxx = 640
ysc = 40

LOCATE 1
ctr2 moad$ + " Form 03"
LINE (320 - 160, 240 + 80)-(320 + 160, 240 + 122), 0, BF
PUT (320 - 160, 240 + 80), otex
VIEW SCREEN (160 - 1, 120 - 1)-(160 + 320, 120 + 241)
grid
FOR x = -160 TO 160 STEP .01
'x = xt - 500
IF adv = 1 THEN sctl = SIN(zetta * (x ^ tetta))
IF adv = 2 THEN sctl = COS(zetta * (x ^ tetta))
IF adv = 3 THEN sctl = TAN(zetta * (x ^ tetta))
IF adv = 4 THEN sctl = LOG(zetta * (x ^ tetta))
sorat = (gamma * sctl) + sigma
kasr = sorat / alpha
y = kasr ^ (1 / betta)
IF x = -160 THEN PSET (x + 320, (480 - y) - 240), 15 ELSE LINE -(x + 320, (480 - y) - 240), 15
NEXT
'VIEW SCREEN (0, 0)-(639, 479)
'LINE (320 - 160, 240 + 80)-(320 + 160, 240 + 122), 0, BF
'PUT (320 - 160, 240 + 80), otex
'VIEW SCREEN (160 - 1, 120 - 1)-(160 + 320, 120 + 241), 2, 14


END SUB

SUB form04
LOCATE 23 * 2 + 1, 1
COLOR 13
'ctr2 "ษออออ เY = แ X^็ + ๅ X^้ + ๋ ออออป"

ctr2 "ษออออ เY = แ X^็ + ๅ X^้ + ๋ ออออป"
ctr2 "บ þ Enter เ:                     บ"
ctr2 "บ þ Enter แ:                     บ"
ctr2 "บ þ Enter ็:                     บ"
ctr2 "บ þ Enter ๅ:                     บ"
ctr2 "บ þ Enter ้:                     บ"
ctr2 "บ þ Enter ๋:                     บ"
 ctr "ศอออออออออออออออออออออออออออ(ESC)ผ"
rea04:
LOCATE 48
print2 " ฏ Enter เ: ;", 24, 11
cong$ = inputstr$(6, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO rea04
alpha = VAL(cong$)
LOCATE 48
print2 " ๛ Enter เ: ;", 24, 3
reb04:
LOCATE 49
print2 " ฏ Enter แ: ;", 24, 11
cong$ = inputstr$(6, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO reb04
betta = VAL(cong$)
LOCATE 49
print2 " ๛ Enter แ: ;", 24, 3
reg04:
LOCATE 50
print2 " ฏ Enter ็: ;", 24, 11
cong$ = inputstr$(10, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO reg04
gamma = VAL(cong$)
LOCATE 50
print2 " ๛ Enter ็: ;", 24, 3
rez04:
LOCATE 51
print2 " ฏ Enter ๅ: ;", 24, 11
cong$ = inputstr$(10, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO rez04
zetta = VAL(cong$)
LOCATE 51
print2 " ๛ Enter ๅ: ;", 24, 3

ret04:
LOCATE 52
print2 " ฏ Enter ้: ;", 24, 11
cong$ = inputstr$(10, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO ret04
tetta = VAL(cong$)
LOCATE 52
print2 " ๛ Enter ้: ;", 24, 3

res04:
LOCATE 53
print2 " ฏ Enter ๋: ;", 24, 11
cong$ = inputstr$(10, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO res04
sigma = VAL(cong$)
LOCATE 53
print2 " ๛ Enter ๋: ;", 24, 3


LOCATE 23 * 2 + 1, 1
COLOR 0
ctr2 "ษออออ เY = แ X^็ + ๅ X^้ + ๋ ออออป"
ctr2 "บ þ Enter เ:                     บ"
ctr2 "บ þ Enter แ:                     บ"
ctr2 "บ þ Enter ็:                     บ"
ctr2 "บ þ Enter ๅ:                     บ"
ctr2 "บ þ Enter ้:                     บ"
ctr2 "บ þ Enter ๋:                     บ"
 ctr "ศอออออออออออออออออออออออออออ(ESC)ผ"
COLOR 10
minx = 0
maxx = 640
ysc = 40
'ctr4 "บ04 เY = แ X^็ + ๅ X^้ + ๋              o"
moad$ = "Y = (" + str2$(betta) + "* X^" + str2$(gamma) + ") + (" + str2$(zetta) + " * X^" + str2$(tetta) + ") + " + str2$(sigma) + " Form 04"
LOCATE 1
ctr2 moad$

FOR x = -160 TO 160 STEP .01

xgo = betta * (x ^ gamma) + zetta * (x ^ tetta) + sigma
y = xgo / alpha

IF x = -160 THEN PSET (x + 320, (480 - y) - 240), 15 ELSE LINE -(x + 320, (480 - y) - 240), 15
NEXT


END SUB

SUB form05

END SUB

SUB form06

END SUB

SUB form07

END SUB

SUB form08
LOCATE 23 * 2 + 1, 1
COLOR 13
   'ctr4 "บ08 ๅ(X/เ) + ้(Y/แ) = ็                 m"

ctr2 "ษอออออ ๅ(X/เ) + ้(Y/แ) = ็ ออออออป"
ctr2 "บ þ Enter เ:                     บ"
ctr2 "บ þ Enter แ:                     บ"
ctr2 "บ þ Enter ็:                     บ"
ctr2 "บ þ Enter ๅ:                     บ"
ctr2 "บ þ Enter ้:                     บ"
 ctr "ศอออออออออออออออออออออออออออ(ESC)ผ"
rea12:
LOCATE 48
print2 " ฏ Enter เ: ;", 24, 11
cong$ = inputstr$(6, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO rea12
alpha = VAL(cong$)
LOCATE 48
print2 " ๛ Enter เ: ;", 24, 3
reb12:
LOCATE 49
print2 " ฏ Enter แ: ;", 24, 11
cong$ = inputstr$(6, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO reb12
betta = VAL(cong$)
LOCATE 49
print2 " ๛ Enter แ: ;", 24, 3
reg12:
LOCATE 50
print2 " ฏ Enter ็: ;", 24, 11
cong$ = inputstr$(10, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO reg12
gamma = VAL(cong$)
LOCATE 50
print2 " ๛ Enter ็: ;", 24, 3
rez12:
LOCATE 51
print2 " ฏ Enter ๅ: ;", 24, 11
cong$ = inputstr$(10, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO rez12
zetta = VAL(cong$)
LOCATE 51
print2 " ๛ Enter ๅ: ;", 24, 3

ret12:
LOCATE 52
print2 " ฏ Enter ้: ;", 24, 11
cong$ = inputstr$(10, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO ret12
tetta = VAL(cong$)
LOCATE 52
print2 " ๛ Enter ้: ;", 24, 3


LOCATE 23 * 2 + 1, 1
COLOR 0
ctr2 "ษอออออ ๅ(X/เ) + ้(Y/แ) = ็ ออออออป"
ctr2 "บ þ Enter เ:                     บ"
ctr2 "บ þ Enter แ:                     บ"
ctr2 "บ þ Enter ็:                     บ"
ctr2 "บ þ Enter ๅ:                     บ"
ctr2 "บ þ Enter ้:                     บ"
 ctr "ศอออออออออออออออออออออออออออ(ESC)ผ"
COLOR 10
minx = 0
maxx = 640
ysc = 40
moad$ = "Y = (" + str2$(betta) + "/" + str2$(tetta) + ") * (" + str2$(gamma) + " - ((" + str2$(zetta) + "*X) / " + str2$(alpha) + ")) Form 08"
LOCATE 1
ctr2 moad$

FOR x = -160 TO 160 STEP .01

y = (betta / tetta) * (gamma - ((zetta * x) / alpha))

IF x = -160 THEN PSET (x + 320, (480 - y) - 240), 15 ELSE LINE -(x + 320, (480 - y) - 240), 15
NEXT


END SUB

SUB form09
LOCATE 23 * 2 + 1, 1
COLOR 13
'ctr4 "บ09 Y =  เ(X/็)^แ                       บ"

ctr2 "ษออออออออ Y =  เ(X/็)^แ อออออออออป"
ctr2 "บ þ Enter เ:                     บ"
ctr2 "บ þ Enter แ:                     บ"
ctr2 "บ þ Enter ็:                     บ"
ctr2 "บ þ X or Y                       บ"
 ctr "ศอออออออออออออออออออออออออออ(ESC)ผ"
rea012:
LOCATE 48
print2 " ฏ Enter เ: ;", 24, 11
cong$ = inputstr$(6, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO rea012
alpha = VAL(cong$)
LOCATE 48
print2 " ๛ Enter เ: ;", 24, 3
reb012:
LOCATE 49
print2 " ฏ Enter แ: ;", 24, 11
cong$ = inputstr$(6, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO reb012
betta = VAL(cong$)
LOCATE 49
print2 " ๛ Enter แ: ;", 24, 3
reg012:
LOCATE 50
print2 " ฏ Enter ็: ;", 24, 11
cong$ = inputstr$(10, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO reg012
gamma = VAL(cong$)
LOCATE 50
print2 " ฏ Enter ็: ;", 24, 3

LOCATE 51, 25
COLOR 11: PRINT "ฏ ";
xy = xory
LOCATE 51, 25
COLOR 3: PRINT "ฏ ";

LOCATE 23 * 2 + 1, 1
COLOR 0
'ctr4 "บ09 Y =  เ(X/็)^แ                       บ"

ctr2 "ษออออออออ Y =  เ(X/็)^แ อออออออออป"
ctr2 "บ þ Enter เ:                     บ"
ctr2 "บ þ Enter แ:                     บ"
ctr2 "บ þ Enter ็:                     บ"
ctr2 "บ þ X or Y                       บ"
 ctr "ศอออออออออออออออออออออออออออ(ESC)ผ"
COLOR 10
minx = 0
maxx = 640
ysc = 40
IF xy = -1 THEN
moad$ = "Y = " + str2$(alpha) + " * ( X /" + str2$(gamma) + " )^" + str2$(betta) + " Form 09"
LOCATE 1
ctr2 moad$
FOR x = -160 TO 160 STEP .01

y = alpha * (x / gamma) ^ betta

IF x = -160 THEN PSET (x + 320, (480 - y) - 240), 15 ELSE LINE -(x + 320, (480 - y) - 240), 15
NEXT
ELSE
moad$ = "X = " + str2$(alpha) + " * ( Y /" + str2$(gamma) + " )^" + str2$(betta) + " Form 09"
LOCATE 1
ctr2 moad$
FOR y = -120 TO 120 STEP .01

x = alpha * (y / gamma) ^ betta

IF y = -120 THEN PSET (x + 320, (480 - y) - 240), 15 ELSE LINE -(x + 320, (480 - y) - 240), 15
NEXT
END IF

END SUB

SUB form10
LOCATE 23 * 2 + 1, 1
COLOR 13
   'ctr4 "บ08 ๅ(X/เ) + ้(Y/แ) = ็                 m"

ctr2 "ษออออออออออ XY = เ^แ ออออออออออออป"
ctr2 "บ þ Enter เ:                     บ"
ctr2 "บ þ Enter แ:                     บ"
 ctr "ศอออออออออออออออออออออออออออ(ESC)ผ"
rea010:
LOCATE 48
print2 " ฏ Enter เ: ;", 24, 11
cong$ = inputstr$(6, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO rea010
alpha = VAL(cong$)
LOCATE 48
print2 " ๛ Enter เ: ;", 24, 3
reb010:
LOCATE 49
print2 " ฏ Enter แ: ;", 24, 11
cong$ = inputstr$(6, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO reb010
betta = VAL(cong$)

LOCATE 23 * 2 + 1, 1
COLOR 0
ctr2 "ษออออออออออ XY = เ^แ ออออออออออออป"
ctr2 "บ þ Enter เ:                     บ"
ctr2 "บ þ Enter แ:                     บ"
 ctr "ศอออออออออออออออออออออออออออ(ESC)ผ"
COLOR 10
minx = 0
maxx = 640
ysc = 40
moad$ = "Y = (" + str2$(alpha) + "^" + str2$(betta) + ")/X Form 10"
LOCATE 1
ctr2 moad$

FOR x = -160 TO 160 STEP .001

y = (alpha ^ betta) / x

'IF INT(x) = -160 AND INT(x) = 0 THEN PSET (x + 320, (480 - y) - 240), 15 ELSE LINE -(x + 320, (480 - y) - 240), 15
PSET (x + 320, (480 - y) - 240), 15
IF INKEY$ = CHR$(13) THEN EXIT FOR
NEXT



END SUB

SUB form11
LOCATE 23 * 2 + 1, 1
COLOR 13
'ctr4 "บ11 ๅ (X/เ)ý + ้ (Y/แ)ý = ็             บ"

ctr2 "ษอออออ (X/เ)ý + ็(Y/แ)ý = 1 อออออป"
ctr2 "บ þ Enter เ:                     บ"
ctr2 "บ þ Enter แ:                     บ"
ctr2 "บ þ Enter ็:                     บ"
ctr2 "บ þ X or Y                       บ"
 ctr "ศอออออออออออออออออออออออออออ(ESC)ผ"
rea11:
LOCATE 48
print2 " ฏ Enter เ: ;", 24, 11
cong$ = inputstr$(6, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO rea11
alpha = VAL(cong$)
LOCATE 48
print2 " ๛ Enter เ: ;", 24, 3
reb11:
LOCATE 49
print2 " ฏ Enter แ: ;", 24, 11
cong$ = inputstr$(6, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO reb11
betta = VAL(cong$)
LOCATE 49
print2 " ๛ Enter แ: ;", 24, 3

reg11:
LOCATE 50
print2 " ฏ Enter ็: ;", 24, 11
cong$ = inputstr$(2, "-1" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO reg11
gamma = VAL(cong$)
COLOR 3
LOCATE 50
print2 " ๛ Enter ็: ;", 24, 3

LOCATE 51, 24
COLOR 11: PRINT " ฏ ";
xoryi = xory

LOCATE 23 * 2 + 1, 1
COLOR 0
'ctr4 "บ09 Y =  เ(X/็)^แ                       บ"

ctr2 "ษอออออ (X/เ)ý + ็(Y/แ)ý = 1 อออออป"
ctr2 "บ þ Enter เ:                     บ"
ctr2 "บ þ Enter แ:                     บ"
ctr2 "บ þ Enter ็:                     บ"
ctr2 "บ þ X or Y                       บ"
 ctr "ศอออออออออออออออออออออออออออ(ESC)ผ"
COLOR 10
minx = 0
maxx = 640
ysc = 40
IF xoryi = -1 THEN
moad$ = "Y = ๛(" + str2$(betta) + "ý * (( Xý - " + str2$(alpha) + "ý ) /" + str2$(alpha) + "ý)/" + str2$(gamma) + ") Form 11"
LOCATE 1
ctr2 moad$
DIM y AS LONG, x AS DOUBLE
FOR x = -160 TO 160 STEP .01
ON ERROR RESUME NEXT
y = SQR((betta ^ 2) * (((x ^ 2 - alpha ^ 2) / alpha ^ 2) / gamma))
IF ERR <> 5 THEN PSET (x + 320, (480 - y) - 240), 15'ELSE LINE -(x + 320, (480 - y) - 240), 15
NEXT
FOR x = -160 TO 160 STEP .01
ON ERROR RESUME NEXT
y = -SQR((betta ^ 2) * ((((x ^ 2) - (alpha ^ 2)) / (alpha ^ 2)) / gamma))
IF ERR <> 5 THEN PSET (x + 320, (480 - y) - 240), 15'ELSE LINE -(x + 320, (480 - y) - 240), 15
NEXT
ELSE

moad$ = "X = ๛(" + str2$(betta) + "ý * (( Yý - " + str2$(alpha) + "ý ) /" + str2$(alpha) + "ý)/" + str2$(gamma) + ") Form 11"
LOCATE 1
ctr2 moad$
DIM x1 AS LONG, y1 AS DOUBLE

FOR y1 = -120 TO 120 STEP .01
ON ERROR RESUME NEXT
x1 = SQR((betta ^ 2) * (((y1 ^ 2 - alpha ^ 2) / alpha ^ 2) / gamma))
IF ERR <> 5 THEN PSET (x1 + 320, (480 - y1) - 240), 15'ELSE LINE -(x + 320, (480 - y) - 240), 15
NEXT

FOR y1 = -120 TO 120 STEP .01
ON ERROR RESUME NEXT
x1 = -SQR((betta ^ 2) * (((y1 ^ 2 - alpha ^ 2) / alpha ^ 2) / gamma))
IF ERR <> 5 THEN PSET (x1 + 320, (480 - y1) - 240), 15'ELSE LINE -(x + 320, (480 - y) - 240), 15
'LOCATE 1, 1: PRINT ERR
NEXT

END IF
END SUB

SUB form12
LOCATE 23 * 2 + 1, 1
COLOR 13
'ctr4 "บ12 (X+เ)ý + (Y+แ)ý = ็ý                บ"

ctr2 "ษอออออ (X+เ)ý + (Y+แ)ý = ็ý อออออป"
ctr2 "บ þ Enter เ:                     บ"
ctr2 "บ þ Enter แ:                     บ"
ctr2 "บ þ Enter ็:                     บ"
 ctr "ศอออออออออออออออออออออออออออ(ESC)ผ"
rea13:
LOCATE 48
print2 " ฏ Enter เ: ;", 24, 11
cong$ = inputstr$(6, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO rea13
alpha = VAL(cong$)
LOCATE 48
print2 " ๛ Enter เ: ;", 24, 3
reb13:
LOCATE 49
print2 " ฏ Enter แ: ;", 24, 11
cong$ = inputstr$(6, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO reb13
betta = VAL(cong$)
LOCATE 49
print2 " ๛ Enter แ: ;", 24, 3

reg13:
LOCATE 50
print2 " ฏ Enter ็: ;", 24, 11
cong$ = inputstr$(6, "-.0123456789" + CHR$(27), 1, 0, 9)
IF cong$ = "{}" THEN exitnes2: GOTO reg13
gamma = VAL(cong$)
COLOR 3
LOCATE 50
print2 " ๛ Enter ็: ;", 24, 3

LOCATE 23 * 2 + 1, 1
COLOR 0
'ctr4 "บ09 Y =  เ(X/็)^แ                       บ"

ctr2 "ษอออออ (X+เ)ý + (Y+แ)ý = ็ý อออออป"
ctr2 "บ þ Enter เ:                     บ"
ctr2 "บ þ Enter แ:                     บ"
ctr2 "บ þ Enter ็:                     บ"
 ctr "ศอออออออออออออออออออออออออออ(ESC)ผ"
COLOR 10
minx = 0
maxx = 640
ysc = 40
'IF xoryi = -1 THEN
moad$ = "Y = ๛(" + str2$(betta) + "ý * (( Xý - " + str2$(alpha) + "ý ) /" + str2$(alpha) + "ý)/" + str2$(gamma) + ") Form 12"
LOCATE 1
ctr2 moad$
DIM y AS LONG, x AS DOUBLE
FOR x = -160 TO 160 STEP .01
ON ERROR RESUME NEXT
y = (SQR(gamma ^ 2 - (x ^ 2 + alpha ^ 2 + 2 * alpha * x))) - betta
IF ERR <> 5 THEN PSET (x + 320, (480 - y) - 240), 15'ELSE LINE -(x + 320, (480 - y) - 240), 15
NEXT
FOR x = -160 TO 160 STEP .01
ON ERROR RESUME NEXT
y = -(SQR(gamma ^ 2 - (x ^ 2 + alpha ^ 2 + 2 * alpha * x))) - betta
IF ERR <> 5 THEN PSET (x + 320, (480 - y) - 240), 15'ELSE LINE -(x + 320, (480 - y) - 240), 15
NEXT

'ELSE
'
'moad$ = "X = ๛(" + str2$(betta) + "ý * (( Yý - " + str2$(alpha) + "ý ) /" + str2$(alpha) + "ý)/" + str2$(gamma) + ") Form 12"
'LOCATE 1
'ctr2 moad$
'DIM x1 AS LONG, y1 AS DOUBLE
'
'FOR y1 = -120 TO 120 STEP .01
'ON ERROR RESUME NEXT
'x1 = SQR((betta ^ 2) * (((y1 ^ 2 - alpha ^ 2) / alpha ^ 2) / gamma))
'IF ERR <> 5 THEN PSET (x1 + 320, (480 - y1) - 240), 15'ELSE LINE -(x + 320, (480 - y) - 240), 15
'NEXT
'
'FOR y1 = -120 TO 120 STEP .01
'ON ERROR RESUME NEXT
'x1 = -SQR((betta ^ 2) * (((y1 ^ 2 - alpha ^ 2) / alpha ^ 2) / gamma))
'IF ERR <> 5 THEN PSET (x1 + 320, (480 - y1) - 240), 15'ELSE LINE -(x + 320, (480 - y) - 240), 15
''LOCATE 1, 1: PRINT ERR
'NEXT
'
'END IF

END SUB

SUB get1 (n$)
n$ = ""
kc = 0
FOR x = 1 TO 80
IF kc = 0 THEN IF CHR$(SCREEN(1, x)) <> " " THEN kc = 1
IF kc = 1 THEN n$ = n$ + CHR$(SCREEN(1, x))
NEXT
n$ = LTRIM$(RTRIM$(n$))
END SUB

SUB getakey
getkeys
a$ = INPUT$(1)
getkeys
END SUB

SUB getanswer (alpha, betta, gamma, zetta, rr, gg, bb)
LOCATE 1, 1: PRINT SPACE$(80)
f = getform(rr, gg, bb)
VIEW SCREEN (160 - 1, 120 - 1)-(160 + 320, 120 + 241), , 14
 VIEW PRINT 1 TO 30
DIM table(10000)
GET (160 - 1, 120 - 1)-(160 + 320, 120 + 241), table
WIDTH 80, 60
VIEW PRINT 1 TO 60
CLS
VIEW SCREEN (160 - 1, 120 - 1)-(160 + 320, 120 + 241), , 14
PUT (160 - 1, 120 - 1), table
setrgbpalette 2, rr, gg, bb
COLOR 12
x80x60

 IF f = 1 THEN form01
 IF f = 2 THEN form02
 IF f = 3 THEN form03
 IF f = 4 THEN form04
 IF f = 5 THEN form05
 IF f = 6 THEN form06
 IF f = 7 THEN form07
 IF f = 8 THEN form08
 IF f = 9 THEN form09
IF f = 10 THEN form10
IF f = 11 THEN form11
IF f = 12 THEN form12
get1 name$
DIM table2(10000)
GET (160 - 1, 120 - 1)-(160 + 320, 120 + 241), table2

SCREEN 0: SCREEN 12
VIEW PRINT 1 TO 30
VIEW SCREEN (160 - 1, 120 - 1)-(160 + 320, 120 + 241), , 14
setrgbpalette 2, rr, gg, bb
'LINE (160 - 1, 120 - 1)-(160 + 320, 120 + 241), 0, BF
PUT (160 - 1, 120 - 1), table2, OR
PUT (160 - 1, 120 - 1), table, OR
grid
put1 name$
END SUB

FUNCTION getform (rr, gg, bb)
DIM table(10000)
GET (160 - 1, 120 - 1)-(160 + 320, 120 + 241), table
WIDTH 80, 60
VIEW PRINT 1 TO 60
CLS
PUT (160 - 1, 120 - 1), table
setrgbpalette 2, rr, gg, bb
COLOR 12
x80x60
LOCATE 47, 1
COLOR 13
ctr4 "ษอออออออออออออออออออออออออออออออออออออออป"
ctr4 "บ01 เY + แX + ็ = ๅ                     บ"
ctr4 "บ02 Y  = เX + แ                         F"
ctr4 "บ03 เY^แ = ็Sin ( ๅ X^้ ) + ๋           บ"
ctr4 "บ04 เY = แ X^็ + ๅ X^้ + ๋              o"
ctr4 "บ05 Y = เ(Sin(็X))^แ + ๅ(Sin(๋X))^้ + C บ"
ctr4 "บ06 Y = เ Sin(แX) + ็                   r"
ctr4 "บ07 Y = เ(Sin(แ(Sin(็X^๋))^้))^ๅ        บ"
ctr4 "บ08 ๅ(X/เ) + ้(Y/แ) = ็                 m"
ctr4 "บ09 Y =  เ(X/็)^แ                       บ"
ctr4 "บ10 XY = เ^แ                            s"
ctr4 "บ11 (X / เ)ý + (Y / แ)ý = 1             บ"
ctr4 "บ12 (X+เ)ý + (Y+แ)ý = ็ý                บ"
ctr5 "ศออออออออออออออออออออออออออออออออออ(ESC)ผ"

l = 1
lo = 1
DO
a$ = INKEY$
IF a$ = CHR$(0) + CHR$(72) THEN l = l - 1
IF a$ = CHR$(0) + CHR$(80) THEN l = l + 1
IF l < 1 THEN l = 12
IF l > 12 THEN l = 1
IF l <> lo THEN
lo = l
LOCATE 47, 1
COLOR 13
ctr4 "ษอออออออออออออออออออออออออออออออออออออออป"
ctr4 "บ01 เY + แX + ็ = ๅ                     บ"
ctr4 "บ02 Y  = เX + แ                         F"
ctr4 "บ03 เY^แ = ็Sin ( ๅ X^้ ) + ๋           บ"
ctr4 "บ04 เY = แ X^็ + ๅ X^้ + ๋              o"
ctr4 "บ05 Y = เ(Sin(็X))^แ + ๅ(Sin(๋X))^้ + C บ"
ctr4 "บ06 Y = เ Sin(แX) + ็                   r"
ctr4 "บ07 Y = เ(Sin(แ(Sin(็X^๋))^้))^ๅ        บ"
ctr4 "บ08 ๅ(X/เ) + ้(Y/แ) = ็                 m"
ctr4 "บ09 Y =  เ(X/็)^แ                       บ"
ctr4 "บ10 XY = เ^แ                            s"
ctr4 "บ11 (X / เ)ý + (Y / แ)ý = 1             บ"
ctr4 "บ12 (X+เ)ý + (Y+แ)ý = ็ý                บ"
ctr5 "ศออออออออออออออออออออออออออออออออออ(ESC)ผ"
END IF
LOCATE 47 + l
COLOR 10
 IF l = 1 THEN ctr5 "ฏฏ เY + แX + ็ = ๅ                     "
 IF l = 2 THEN ctr5 "ฏฏ Y  = เX + แ                         "
 IF l = 3 THEN ctr5 "ฏฏ เY^แ = ็Sin ( ๅ X^้ ) + ๋           "
 IF l = 4 THEN ctr5 "ฏฏ เY = แ X^็ + ๅ X^้ + ๋              "
 IF l = 5 THEN ctr5 "ฏฏ Y = เ(Sin(็X))^แ + ๅ(Sin(๋X))^้ + C "
 IF l = 6 THEN ctr5 "ฏฏ Y = เ Sin(แX) + ็                   "
 IF l = 7 THEN ctr5 "ฏฏ Y = เ(Sin(แ(Sin(็X^๋))^้))^ๅ        "
 IF l = 8 THEN ctr5 "ฏฏ ๅ(X/เ) + ้(Y/แ) = ็                 "
 IF l = 9 THEN ctr5 "ฏฏ Y =  เ(X/็)^แ                       "
IF l = 10 THEN ctr5 "ฏฏ XY = เ^แ                            "
IF l = 11 THEN ctr5 "ฏฏ (X / เ)ý + (Y / แ)ý = 1             "
IF l = 12 THEN ctr5 "ฏฏ (X+เ)ý + (Y+แ)ý = ็ý                "
IF a$ = CHR$(13) THEN EXIT DO
IF a$ = CHR$(27) THEN exitnes2
LOOP
LOCATE 47, 1
COLOR 0
ctr4 "ษอออออออออออออออออออออออออออออออออออออออป"
ctr4 "บ01 เY + แX + ็ = ๅ                     บ"
ctr4 "บ02 Y  = เX + แ                         F"
ctr4 "บ03 เY^แ = ็Sin ( ๅ X^้ ) + ๋           บ"
ctr4 "บ04 เY = แ X^็ + ๅ X^้ + ๋              o"
ctr4 "บ05 Y = เ(Sin(็X))^แ + ๅ(Sin(๋X))^้ + C บ"
ctr4 "บ06 Y = เ Sin(แX) + ็                   r"
ctr4 "บ07 Y = เ(Sin(แ(Sin(็X^๋))^้))^ๅ        บ"
ctr4 "บ08 ๅ(X/เ) + ้(Y/แ) = ็                 m"
ctr4 "บ09 Y =  เ(X/็)^แ                       บ"
ctr4 "บ10 XY = เ^แ                            s"
ctr4 "บ11 (X / เ)ý + (Y / แ)ý = 1             บ"
ctr4 "บ12 (X+เ)ý + (Y+แ)ý = ็ý                บ"
ctr5 "ศออออออออออออออออออออออออออออออออออ(ESC)ผ"
SCREEN 0: SCREEN 12
VIEW PRINT 1 TO 30
VIEW SCREEN (160 - 1, 120 - 1)-(160 + 320, 120 + 241), , 14
setrgbpalette 2, rr, gg, bb
PUT (160 - 1, 120 - 1), table, OR
grid
getform = l
END FUNCTION

SUB getkeys
DO WHILE INKEY$ <> ""
LOOP
END SUB

SUB getpix (x, y)
pr = 5
DIM table(250 * pr)
VIEW SCREEN (160 - pr - 1, 120 - pr - 1)-(160 + 320 + pr, 120 + 241 + pr)', table
x = 320
y = 240
xo = x
yo = y
GET (x - pr, y - pr)-(x + pr, y + pr), table
p = POINT(x, y)
LOCATE 1, 1
PRINT "000,000"
br = 1
rd = 0
DO
'rd = rd + br
'IF rd >= 63 THEN br = -1
'IF rd <= 0 THEN br = 1
'LOCATE 2, 1: PRINT br; rd
'IF rd MOD 9 = 0 THEN PALETTE 13, (63 - rd) + 256 ^ 2 * rd
'SOUND 0, .1
x$ = str2$(x - 320)
y$ = str2$(y - 240)
p$ = str2$(p)
IF p$ = "2" THEN p$ = "0"
x10d: IF LEN(x$) < 4 THEN x$ = " " + x$: GOTO x10d
y10d: IF LEN(y$) < 4 THEN y$ = " " + y$: GOTO y10d
p10d: IF LEN(p$) < 2 THEN p$ = "0" + p$: GOTO p10d
a$ = INKEY$
IF a$ = CHR$(0) + CHR$(72) THEN y = y - 5
IF a$ = CHR$(0) + CHR$(80) THEN y = y + 5
IF a$ = CHR$(0) + CHR$(75) THEN x = x - 5
IF a$ = CHR$(0) + CHR$(77) THEN x = x + 5
IF y > 120 + 240 THEN y = 120 + 240
IF x > 160 + 320 - 1 THEN x = 160 + 320 - 1
IF y < 240 - 120 THEN y = 240 - 120
IF x < 320 - 160 THEN x = 320 - 160
IF xo <> x OR yo <> y THEN
LINE (xo - pr, yo - pr)-(xo + pr, yo + pr), 0, BF

PUT (xo - pr, yo - pr), table, XOR

LINE (xo - 2.5, 120 - 1)-(xo + 2.5, 120 - 1), 14
LINE (160 - 1, yo - 2.5)-(160 - 1, yo + 2.5), 14
LINE (xo - 2.5, 240 + 120 + 1)-(xo + 2.5, 240 + 120 + 1), 14
LINE (320 + 160, yo - 2.5)-(320 + 160, yo + 2.5), 14
PSET (xo, yo), p
GET (x - pr, y - pr)-(x + pr, y + pr), table
END IF
IF xo <> x THEN PSET (xo, yo), p: xo = x: p = POINT(x, y)
IF yo <> y THEN PSET (xo, yo), p: yo = y: p = POINT(x, y)
'LINE (x, 240 - 120)-(x, 240 + 120), 13
'LINE (320 - 160, y)-(320 + 160, y), 13
'PSET (x, y), RND * 15
LINE (x - pr, y)-(x + pr, y), 14
LINE (x, y - pr)-(x, y + pr), 14

LINE (x - 2.5, 120 - 1)-(x + 2.5, 120 - 1), 0
LINE (160 - 1, y - 2.5)-(160 - 1, y + 2.5), 0
LINE (x - 2.5, 240 + 120 + 1)-(x + 2.5, 240 + 120 + 1), 0
LINE (320 + 160, y - 2.5)-(320 + 160, y + 2.5), 0

COLOR 10
LOCATE 1, 1
PRINT x$; ","; y$; " : "; p$; "  "
IF a$ = CHR$(13) THEN EXIT DO
IF a$ = CHR$(27) THEN x = -100: EXIT DO
LOOP
COLOR 0
LOCATE 1, 1
PRINT x$; ","; y$; " : "; p$; "  "
COLOR 10
'LINE (xo - pr, yo - pr)-(xo + pr, yo + pr), 0, BF
'PUT (xo - pr, yo - pr), table, XOR

LINE (x - pr, y)-(x + pr, y), 0
LINE (x, y - pr)-(x, y + pr), 0
LINE (xo - 2.5, 120 - 1)-(xo + 2.5, 120 - 1), 14
LINE (160 - 1, yo - 2.5)-(160 - 1, yo + 2.5), 14
LINE (xo - 2.5, 240 + 120 + 1)-(xo + 2.5, 240 + 120 + 1), 14
LINE (320 + 160, yo - 2.5)-(320 + 160, yo + 2.5), 14

'LINE (xo - pr, yo - pr)-(xo + pr, yo + pr), 0, BF

'PUT (xo - pr, yo - pr), table, XOR

END SUB

SUB grid
COLOR 12
FOR ym = 240 - 120 TO 240 + 120
IF POINT(320, ym) <> 15 THEN PSET (320, ym), 12
NEXT

FOR xm = 320 - 160 TO 320 + 160
IF POINT(xm, 240) <> 15 THEN PSET (xm, 240), 12
NEXT

'LINE (320, 240 - 120)-(320, 240 + 120), 12
'LINE (320 - 160, 240)-(320 + 160, 240), 12
ctr3 7, "  -160.                   0.               160.    "
FOR xl = 8 TO 23 STEP 2.5
 sss$ = str2$(INT((xl - 8) * 16) - 120)
ssso:
IF LEN(sss$) < 5 THEN sss$ = " " + sss$: GOTO ssso
 LOCATE INT(xl), 15: PRINT sss$
 NEXT
zoom = 1
minx = 0
ayis = 40
maxx = 640
axis = (ABS(maxx - minx) / 640) + 10
FOR y = 120 TO 240 + 120 STEP ((zoom) * ayis) \ 2

FOR ch = 160 TO 160 + 320 STEP ((zoom) * axis) \ 2
IF POINT(x + ch, y) <> 15 AND POINT(x + ch, y) <> 12 THEN CIRCLE (x + ch, y), -1 + zoom, 10
IF POINT(x + ch, y) = 12 THEN CIRCLE (x + ch, y), -1 + zoom, 14
NEXT

NEXT

END SUB

SUB initcadr (colr)
FOR i = 1 TO 5
j = i
IF colr = 10 THEN setrgbpalette j, 0, INT(i * 6.3 * 2), 0
IF colr = 11 THEN setrgbpalette j, 0, INT(i * 6.3 * 2), INT(i * 6.3 * 2)
IF colr = 12 THEN setrgbpalette j, INT(i * 6.3 * 2), 0, 0
IF colr = 13 THEN setrgbpalette j, INT(i * 6.3 * 2), 0, INT(i * 6.3 * 2)
IF colr = 14 THEN setrgbpalette j, INT(i * 6.3 * 2), INT(i * 6.3 * 2), 0
IF colr = 15 THEN setrgbpalette j, INT(i * 6.3 * 2), INT(i * 6.3 * 2), INT(i * 6.3 * 2)
NEXT

END SUB

FUNCTION inputstr$ (LENgth, get$, tpr, ttr, tc)
x = POS(0)
y = CSRLIN
LOCATE 1, 1: PRINT SPACE$(80)
DO
10 a$ = INKEY$
COLOR tc
IF INT(TIMER * 10) MOD 2 = 0 THEN LOCATE y, x: PRINT n$; : COLOR 13: PRINT "_ " ELSE LOCATE y, x: PRINT n$; "  "
IF a$ = "" THEN GOTO 10
IF a$ = CHR$(13) AND n$ <> "" THEN GOTO endf
IF a$ = CHR$(27) THEN
IF INSTR(get$, CHR$(27)) <> 0 THEN n$ = "{}": GOTO endf
GOTO 10
END IF
IF a$ = CHR$(8) THEN
IF LEN(n$) >= 1 THEN n$ = LEFT$(n$, LEN(n$) - 1) ELSE n$ = ""
GOTO 10
END IF
IF LEN(a$) > 1 THEN a$ = RIGHT$(a$, 1)
IF INSTR(get$, a$) <> 0 THEN IF LEN(n$) < LENgth THEN n$ = n$ + a$
LOOP
endf:
COLOR tc
LOCATE y, x: PRINT n$; "  ";
IF n$ = "" OR n$ = "{}" THEN ttr = 1
IF ttr = 1 THEN LOCATE y, x: PRINT SPACE$(LENgth); "  ";
IF tpr <> 0 THEN PRINT
inputstr$ = n$
END FUNCTION

FUNCTION keypressed
IF INKEY$ <> "" THEN keypressed = 1
END FUNCTION

SUB modify
DIM table(10000)
GET (160 - 1, 120 - 1)-(160 + 320, 120 + 241), table
whatagain:
w = whatdo
IF w = 3 THEN GOTO nowhat
IF w = 2 THEN axisv ook, ook2
IF w = 1 THEN zoom2
LINE (160 - 1, 120 - 1)-(160 + 320, 120 + 241), 0, BF
PUT (160 - 1, 120 - 1), table
GOTO whatagain
nowhat:
END SUB

SUB offall (s, d)
FOR gg = 0 TO 63 STEP s
G = 63 - gg
 setrgbpalette 1, 0, 0, G
 setrgbpalette 2, 0, G, 0
 setrgbpalette 3, 0, G, G
 setrgbpalette 4, G, 0, 0
 setrgbpalette 5, G, 0, G
 setrgbpalette 6, G, G, 0
 setrgbpalette 7, G, G, G
 setrgbpalette 8, G, G, G
 setrgbpalette 9, 0, 0, G
setrgbpalette 10, 0, G, 0
setrgbpalette 11, 0, G, G
setrgbpalette 12, G, 0, 0
setrgbpalette 13, G, 0, G
setrgbpalette 14, G, G, 0
setrgbpalette 15, G, G, G
IF d <> 0 THEN SOUND 0, d
NEXT

END SUB

SUB offf (c, cc)
FOR j = 0 TO 63
i = 63 - j
IF cc = 1 THEN setrgbpalette c, i, 0, 0
IF cc = 2 THEN setrgbpalette c, 0, i, 0
IF cc = 3 THEN setrgbpalette c, 0, 0, i
IF cc = 4 THEN setrgbpalette c, i, i, 0
IF cc = 5 THEN setrgbpalette c, i, 0, i
IF cc = 6 THEN setrgbpalette c, 0, i, i
IF cc = 7 THEN setrgbpalette c, i, i, i
IF keypressed THEN EXIT FOR
delay 2
NEXT

END SUB

'
SUB offs
FOR t = 1 TO 15
FOR G = 0 TO 63 STEP 5
gg = INT(63 - G)
IF t = 1 OR t = 9 THEN setrgbpalette INT(t), 0, 0, INT(gg)
IF t = 2 OR t = 10 THEN setrgbpalette INT(t), INT(gg), 0, 0
IF t = 3 OR t = 11 THEN setrgbpalette INT(t), 0, INT(gg), INT(gg)
IF t = 4 OR t = 12 THEN setrgbpalette INT(t), INT(gg), 0, INT(gg)
IF t = 5 OR t = 13 THEN setrgbpalette INT(t), INT(gg), 0, INT(gg)
IF t = 6 OR t = 14 THEN setrgbpalette INT(t), INT(gg), INT(gg), 0
IF t = 7 OR t = 8 OR t = 15 THEN setrgbpalette INT(t), INT(gg), INT(gg), INT(gg)
delay (1)
NEXT
NEXT
delay (50)
END SUB

'
SUB onall (s, d)
FOR G = 0 TO 63 STEP s
 setrgbpalette 1, 0, 0, G
 setrgbpalette 2, 0, G, 0
 setrgbpalette 3, 0, G, G
 setrgbpalette 4, G, 0, 0
 setrgbpalette 5, G, 0, G
 setrgbpalette 6, G, G, 0
 setrgbpalette 7, G, G, G
 setrgbpalette 8, G, G, G
 setrgbpalette 9, 0, 0, G
setrgbpalette 10, 0, G, 0
setrgbpalette 11, 0, G, G
setrgbpalette 12, G, 0, 0
setrgbpalette 13, G, 0, G
setrgbpalette 14, G, G, 0
setrgbpalette 15, G, G, G
IF d <> 0 THEN SOUND 0, d
NEXT
END SUB

'
SUB onn (c, cc)
FOR i = 0 TO 63
IF cc = 1 THEN setrgbpalette c, i, 0, 0
IF cc = 2 THEN setrgbpalette c, 0, i, 0
IF cc = 3 THEN setrgbpalette c, 0, 0, i
IF cc = 4 THEN setrgbpalette c, i, i, 0
IF cc = 5 THEN setrgbpalette c, i, 0, i
IF cc = 6 THEN setrgbpalette c, 0, i, i
IF cc = 7 THEN setrgbpalette c, i, i, i
IF keypressed THEN EXIT FOR
delay 2
NEXT
END SUB

FUNCTION order
LOCATE 24, 1
COLOR 13
ctr2 "ษออออออออออออออออออออออออออออออออป"
ctr2 "วฤฤฤฤฤฤฤฤฤฤฤ Finished ฤฤฤฤฤฤฤฤฤฤฤถ"
ctr2 "บ þ Modify This Table (Zoom,...) บ"
ctr2 "บ þ Clear Screen and start again บ"
ctr2 "บ þ Start new                    บ"
ctr2 "บ þ Exit Program                 บ"
 ctr "ศออออออออออออออออออออออออออออออออผ"
l = 1
lo = 1
DO
a$ = INKEY$
IF a$ = CHR$(0) + CHR$(72) THEN l = l - 1
IF a$ = CHR$(0) + CHR$(80) THEN l = l + 1
IF l < 1 THEN l = 4
IF l > 4 THEN l = 1
IF l <> lo THEN
lo = l
LOCATE 24, 1
COLOR 13
ctr2 "ษออออออออออออออออออออออออออออออออป"
ctr2 "วฤฤฤฤฤฤฤฤฤฤฤ Finished ฤฤฤฤฤฤฤฤฤฤฤถ"
ctr2 "บ þ Modify This Table (Zoom,...) บ"
ctr2 "บ þ Clear Screen and start again บ"
ctr2 "บ þ Start new                    บ"
ctr2 "บ þ Exit Program                 บ"
 ctr "ศออออออออออออออออออออออออออออออออผ"
END IF
LOCATE 25 + l
COLOR 10
IF l = 1 THEN ctr " ฏ Modify This Table (Zoom,...) "
IF l = 2 THEN ctr " ฏ Clear Screen and start again "
IF l = 3 THEN ctr " ฏ Start new                    "
IF l = 4 THEN ctr " ฏ Exit Program                 "
IF a$ = CHR$(13) THEN EXIT DO
IF a$ = CHR$(27) THEN l = 4: EXIT DO
LOOP
order = l
END FUNCTION

SUB print2 (x$, x, c)
LOCATE , x
COLOR c
IF RIGHT$(x$, 1) = ";" THEN arm = 0: x$ = LEFT$(x$, LEN(x$) - 1) ELSE arm = 1
PRINT x$;
IF arm = 1 THEN PRINT
END SUB

SUB put1 (n$)
COLOR 10
ctr3 1, n$
END SUB

'
SUB rtr (c, cc)
FOR i = c TO cc
setrgbpalette i, 0, 0, 0
NEXT
END SUB

SUB setrgbpalette (x, R, G, B)
PALETTE x, B * 65536 + G * 256 + R
END SUB

FUNCTION sincostanlog
x = POS(0)
y = CSRLIN
l = 1
DO
LOCATE y, x
COLOR 9
IF l = 1 THEN COLOR 14
PRINT "Sin ";
COLOR 9: PRINT ", ";
IF l = 2 THEN COLOR 14
PRINT "Cos ";
COLOR 9: PRINT ", ";
IF l = 3 THEN COLOR 14
PRINT "Tan ";
COLOR 9: PRINT ", ";
IF l = 4 THEN COLOR 14
PRINT "Log ";
a$ = INKEY$
IF a$ = CHR$(0) + CHR$(77) THEN l = l + 1
IF a$ = CHR$(0) + CHR$(75) THEN l = l - 1
IF l > 4 THEN l = 1
IF l < 1 THEN l = 4
IF a$ = CHR$(27) THEN exitnes2
IF a$ = CHR$(13) THEN EXIT DO
LOOP
sincostanlog = l
END FUNCTION

SUB smprint (tx$)
x$ = tx$
IF RIGHT$(x$, 1) = ";" THEN tog = 0 ELSE tog = 1
x$ = LEFT$(x$, LEN(x$) - 1)
FOR xx = 1 TO LEN(x$)
ccl = 11
q$ = MID$(x$, xx, 1)
IF (ASC(q$) >= 65 AND ASC(q$) <= 90) OR (ASC(q$) >= 97 AND ASC(q$) <= 122) THEN GOTO sld
SELECT CASE ASC(q$)
CASE 178 TO 218: ccl = 3: GOTO sld
CASE 126 TO 175, 23 TO 29, 3 TO 9, 34, 35, 42, 59, 95: ccl = 13: GOTO sld
CASE 15 TO 18, 96, 92, 126: ccl = 12: GOTO sld
CASE 64, 62, 60, 40, 41, 91, 93, 123, 125, 19, 26, 33, 36, 58, 63: ccl = 14: GOTO sld
CASE 48 TO 57: ccl = 10: GOTO sld
CASE 219 TO 223: ccl = 9: GOTO sld
CASE 43 TO 47, 53, 61, 94, 223 TO 250: ccl = 15: GOTO sld
CASE 38: ccl = 2: GOTO sld
CASE 2, 1: ccl = 26: GOTO sld
CASE 3: ccl = 28: GOTO sld
CASE 124: ccl = 6: GOTO sld
CASE 176 TO 178: ccl = 5: GOTO sld
CASE 254: ccl = 30: GOTO sld
END SELECT
sld:
COLOR ccl
PRINT q$;
NEXT
IF tog = 1 THEN PRINT
getkeys
END SUB

FUNCTION str2$ (x)
str2$ = LTRIM$(RTRIM$(STR$(x)))
END FUNCTION

FUNCTION str3$ (x AS LONG)
str3$ = LTRIM$(RTRIM$(STR$(x)))
END FUNCTION

FUNCTION time2$
hour = VAL(TIME$)
IF hour > 12 THEN hour = hour - 12: apm$ = "PM" ELSE apm$ = "AM"
IF hour = 12 THEN apm$ = "PM"
tgt = INSTR(TIME$, ":")
gg$ = LTRIM$(RTRIM$(STR$(hour)))
ggo:
IF LEN(gg$) < 2 THEN gg$ = "0" + gg$: GOTO ggo
time2$ = gg$ + MID$(TIME$, tgt) + " " + apm$
END FUNCTION

'
SUB tp (s$)
FOR t = 1 TO LEN(s$)
ss$ = MID$(s$, t, 1)
IF ss$ <> " " THEN SOUND 0, .1
PRINT ss$;
NEXT
PRINT
END SUB

FUNCTION whatdo
DIM table(10000)
GET (160 - 1, 120 - 1)-(160 + 320, 120 + 241), table
LOCATE 24, 1
COLOR 13
ctr2 "                                  "
ctr2 "ษออออออออออออออออออออออออออออออออป"
ctr2 "วฤฤฤฤฤฤฤฤฤ Modify Table ฤฤฤฤฤฤฤฤฤถ"
ctr2 "บ þ Zoom on Table...             บ"
ctr2 "บ þ Pixel Axis , Ayis Viewer     บ"
ctr2 "บ þ Exit Modifier                บ"
 ctr "ศออออออออออออออออออออออออออออออออผ"
l = 1
lo = 1
DO
a$ = INKEY$
IF a$ = CHR$(0) + CHR$(72) THEN l = l - 1
IF a$ = CHR$(0) + CHR$(80) THEN l = l + 1
IF l < 1 THEN l = 3
IF l > 3 THEN l = 1
IF l <> lo THEN
lo = l
LOCATE 24, 1
COLOR 13
ctr2 "                                  "
ctr2 "ษออออออออออออออออออออออออออออออออป"
ctr2 "วฤฤฤฤฤฤฤฤฤ Modify Table ฤฤฤฤฤฤฤฤฤถ"
ctr2 "บ þ Zoom on Table...             บ"
ctr2 "บ þ Pixel Axis , Ayis Viewer     บ"
ctr2 "บ þ Exit Modifier                บ"
 ctr "ศออออออออออออออออออออออออออออออออผ"
END IF
LOCATE 26 + l
COLOR 10
IF l = 1 THEN ctr " ฏ Zoom on Table...             "
IF l = 2 THEN ctr " ฏ Pixel Axis , Ayis Viewer     "
IF l = 3 THEN ctr " ฏ Exit Modifier                "
IF a$ = CHR$(13) THEN EXIT DO
IF a$ = CHR$(27) THEN l = 3: EXIT DO
LOOP
LOCATE 24, 1
COLOR 0
ctr2 "                                  "
ctr2 "ษออออออออออออออออออออออออออออออออป"
ctr2 "วฤฤฤฤฤฤฤฤฤ Modify Table ฤฤฤฤฤฤฤฤฤถ"
ctr2 "บ þ Zoom on Table...             บ"
ctr2 "บ þ Pixel Axis , Ayis Viewer     บ"
ctr2 "บ þ Exit Modifier                บ"
 ctr "ศออออออออออออออออออออออออออออออออผ"
LINE (160 - 1, 120 - 1)-(160 + 320, 120 + 241), 0, BF
PUT (160 - 1, 120 - 1), table
whatdo = l
END FUNCTION

SUB x80x60
ctr3 14, "  -160.                   0.               160.    "
FOR xl = 8 TO 23 STEP 2.5
 sss$ = str2$(INT((xl - 8) * 16) - 120)
ssso1:
IF LEN(sss$) < 5 THEN sss$ = " " + sss$: GOTO ssso1
 LOCATE INT(xl) * 2, 15: PRINT sss$
 NEXT

END SUB

FUNCTION xory
x = POS(0)
y = CSRLIN
l = 1
DO
a$ = INKEY$
IF a$ = CHR$(27) THEN exitnes2
IF a$ = CHR$(0) + CHR$(75) OR a$ = CHR$(0) + CHR$(77) THEN l = -l
IF a$ = CHR$(13) THEN EXIT DO
LOCATE y, x
COLOR 3
IF l = 1 THEN COLOR 14
PRINT "X";
COLOR 10: PRINT " or "; : COLOR 3
IF l = -1 THEN COLOR 14
PRINT "Y"
LOOP
xory = l
END FUNCTION

SUB zoom2
getpix x1, y1
IF x1 = -100 THEN GOTO abort
getpix x2, y2
IF x2 = -100 THEN GOTO abort
'LINE (x1, y1)-(x2, y2), 13, B
x1 = x1 + 1
y1 = y1 + 1
x2 = x2 - 1
y2 = y2 - 1
cga = (((ABS(y2 - y1) + 1)) \ 4)
DIM p1(((ABS(x2 - x1) + 2)), 62) AS INTEGER
DIM p2(((ABS(x2 - x1) + 2)), 62) AS INTEGER
DIM p3(((ABS(x2 - x1) + 2)), 62) AS INTEGER
DIM p4(((ABS(x2 - x1) + 2)), 62) AS INTEGER
IF x1 > x2 THEN SWAP x1, x2
IF y1 > y2 THEN SWAP y1, y2
FOR x = x1 TO x2 + 1
FOR y = y1 TO y2 + 1
'PRINT (x + 1) - x1, (y + 1) - y1
'LOCATE , 1
ny = (y - y1) + 1
'PRINT (x + 1) - x1, ); y; 179 - 1
'PRINT ny; (((ABS(x2 - x1) + 2))), ((((ABS(y2 - y1) + 1)) \ 4) + 5)
IF ny < 60 THEN p1((x + 1) - x1, ny) = POINT(x, y)
IF ny >= 60 AND ny < 120 THEN p2((x + 1) - x1, ny - 59) = POINT(x, y)
IF ny >= 120 AND ny < 180 THEN p3((x + 1) - x1, ny - 119) = POINT(x, y)
IF ny >= 180 THEN p4((x + 1) - x1, ny - 179) = POINT(x, y)
PSET (x, y), POINT(x, y) - 1
NEXT: NEXT
LINE (160 - 1, 120 - 1)-(160 + 320, 120 + 241), 2, BF
LINE (160 - 1, 120 - 1)-(160 + 320, 120 + 241), 14, B
VIEW SCREEN (160 - 1, 120 - 1)-(160 + 320, 120 + 241), , 14
FOR x = 1 TO (x2 - x1) + 1
FOR y = 1 TO (y2 - y1) + 1
tx = (320 / (x2 - x1)) \ 2
ty = (240 / (y2 - y1)) \ 2
xx = ((x - 1) * (320 / (x2 - x1))) + 160
yy = ((y - 1) * (240 / (y2 - y1))) + 120
'LINE (xx - tx, yy - ty)-(xx + tx, yy + ty), p1(x, y), BF

IF y < 60 THEN IF p1(x, y) = 15 AND p1(x, y) > 0 THEN LINE (xx - tx, yy - ty)-(xx + tx + 1, yy + ty + 1), p1(x, y), BF
IF y >= 60 AND y < 120 THEN IF p2(x, y - 59) = 15 AND p2(x, y - 59) > 0 THEN LINE (xx - tx, yy - ty)-(xx + tx + 1, yy + ty + 1), p2(x, y - 59), BF
IF y >= 120 AND y < 180 THEN IF p3(x, y - 119) = 15 AND p3(x, y - 119) > 0 THEN LINE (xx - tx, yy - ty)-(xx + tx + 1, yy + ty + 1), p3(x, y - 119), BF
IF y >= 180 THEN IF p4(x, y - 179) = 15 AND p4(x, y - 179) > 0 THEN LINE (xx - tx, yy - ty)-(xx + tx + 1, yy + ty + 1), p4(x, y - 179), BF
NEXT: NEXT
VIEW SCREEN (160 - 1, 120 - 1)-(160 + 320, 120 + 241), , 14
LINE (160 - 1, 120 - 1)-(160 + 320, 120 + 241), 14, B
grid
ook = tx
ook2 = ty
endzoom ook, ook2
'PRINT x1; y2; x2; y2
abort:
VIEW SCREEN (160 - 1, 120 - 1)-(160 + 320, 120 + 241), 2, 14
LINE (160 - 1, 120 - 1)-(160 + 320, 120 + 241), 0, BF
LINE (160 - 1, 120 - 1)-(160 + 320, 120 + 241), 14, B
END SUB

