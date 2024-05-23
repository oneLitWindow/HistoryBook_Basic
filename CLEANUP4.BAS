DECLARE SUB offs ()
DECLARE SUB drvinfo (drvs$)
DECLARE SUB cadrs2 ()
DECLARE SUB cadrs ()
DECLARE SUB draws (x%, y%, z%, c%)
DECLARE SUB CIRCLEs (x!, y!, r!, c!, dly!)
DECLARE SUB onn (c!, cc!)
DECLARE SUB offf (c!, cc!)
DECLARE SUB cadr (x1!, y1!, x2!, y2!, dly!, colr!)
DECLARE SUB setrgbpalette (x!, r!, g!, B!)
DECLARE SUB delay (x!)
DECLARE FUNCTION keypressed! ()
DECLARE SUB TP (s$)
DECLARE FUNCTION time2$ ()
DECLARE FUNCTION date2$ ()
DECLARE FUNCTION str2$ (x!)
DECLARE FUNCTION ogay! ()
DECLARE SUB remenu ()
DECLARE FUNCTION inp$ (t2$)
DECLARE FUNCTION sure! (y!, x!)
DECLARE SUB bbp (B!)
DECLARE SUB logo ()
DECLARE SUB res ()
DECLARE SUB ctr (y!, TEXT$)
DECLARE SUB errl1 ()
DECLARE SUB help ()
DECLARE SUB Options ()
DECLARE SUB refd ()
DECLARE SUB defg ()
DECLARE SUB defo ()
DECLARE SUB cln (c!)
DECLARE SUB emp (e!)
DECLARE SUB summ (s!)
CONST myx = 320, myy = 400, myre = 22
';;;;;;;;;;;;;;;;;;;;;;;;
'options
'END
';;;;;;;;;;;;;;;;;;;;;;;;
'if s[1]='Textcolorlogoÿ' then i[3]:=1;
'if s[1]='Setcolordrvÿ' then BEGIN;S[2]:=PARAMSTR(2);i[3]:=2;END;
'if s[1]='Textbackgroundhelpÿ' then i[3]:=3;
'if s[1]='Gotoxyaboutÿ' then i[3]:=4;
ON ERROR GOTO rr1
OPEN "ASP_Test.Tmp" FOR OUTPUT AS #1
PRINT #1, "Tested..."
CLOSE #1
KILL "ASP_Test.Tmp"
RESET
ON ERROR RESUME NEXT
DEFINT A-Z
DIM SHARED palettearray(15)
DIM cas$(10)
DIM bh$(80, 23)
DIM bc(80, 23)
DIM d$(23, 80)
DIM f$(38)
GOSUB BEGIN
cadrs
'LINE (186, 303.5)-(624, 303.5), 10
DO
LOCATE ln + 1, 2
COLOR 10, 4: PRINT ln;
LOCATE ln + 1, 6
COLOR 14, 4
IF ln = 1 THEN PRINT " Start Clean Up "
IF ln = 2 THEN PRINT " Drive Select   "
IF ln = 3 THEN PRINT " Options       "
IF ln = 4 THEN PRINT " Drive info.    "
IF ln = 5 THEN PRINT " Exit       "
 LOCATE 9, 2: COLOR 13: PRINT " Time : "; time2$
LOCATE 10, 2: COLOR 13: PRINT " Date : "; date2$;
min$ = MID$(TIME$, 4, 2)
hou$ = LEFT$(TIME$, 2)
sec$ = MID$(TIME$, 7, 2)
sec = VAL(sec$)
hou = VAL(hou$)
min = VAL(min$)
'LOCATE 1, 1: PRINT min; hou; sec
'draws 80, 200, 20, 10
sec = sec - 1
hou = hou - 1
min = min - 1
LINE (80, 200)-(SIN(((1.5 / 3) * 6.28) - ((sec * 6.28) / 60)) * 18 + 80, COS(((1.5 / 3) * 6.28) - ((sec * 6.28) / 60)) * 18 + 200), 0
LINE (80, 200)-(SIN(((1.5 / 3) * 6.28) - ((min * 6.28) / 60)) * 15 + 80, COS(((1.5 / 3) * 6.28) - ((min * 6.28) / 60)) * 15 + 200), 0
LINE (80, 200)-(SIN(((1.5 / 3) * 6.28) - ((hou * 6.28) / 12)) * 10 + 80, COS(((1.5 / 3) * 6.28) - ((hou * 6.28) / 12)) * 10 + 200), 0
sec = sec + 1
hou = hou + 1
min = min + 1

LINE (80, 200)-(SIN(((1.5 / 3) * 6.28) - ((sec * 6.28) / 60)) * 18 + 80, COS(((1.5 / 3) * 6.28) - ((sec * 6.28) / 60)) * 18 + 200), 12
LINE (80, 200)-(SIN(((1.5 / 3) * 6.28) - ((min * 6.28) / 60)) * 15 + 80, COS(((1.5 / 3) * 6.28) - ((min * 6.28) / 60)) * 15 + 200), 14
LINE (80, 200)-(SIN(((1.5 / 3) * 6.28) - ((hou * 6.28) / 12)) * 10 + 80, COS(((1.5 / 3) * 6.28) - ((hou * 6.28) / 12)) * 10 + 200), 11
CIRCLE (80, 200), 1, 13
PAINT (80, 200), 13

'LINE (80, 200)-(SIN((3.14 - (min * 6.28)) / 60) * 15 + 80, COS((3.14 - (min * 6.28)) / 60) * 15 + 200), 14
'LINE (80, 200)-(SIN((hou * 6.28) / 24) * 10 + 80, COS((hou * 6.28) / 24) * 10 + 200), 9

COLOR 11, 1
k$ = INKEY$
IF k$ = CHR$(13) THEN
IF ln = 5 THEN GOSUB rhh
offf 15, 7
GOSUB sel
onn 15, 7
setrgbpalette 9, 0, 0, 63
setrgbpalette 10, 0, 63, 0
setrgbpalette 11, 0, 63, 63
setrgbpalette 12, 63, 0, 0
setrgbpalette 13, 63, 0, 63
setrgbpalette 14, 63, 63, 0
setrgbpalette 15, 63, 63, 63
cadrs
'LINE (186, 303.5)-(624, 303.5), 10
END IF
IF k$ = CHR$(27) THEN EXIT DO
IF k$ = CHR$(0) + CHR$(72) THEN ln = ln - 1
IF k$ = CHR$(0) + CHR$(80) THEN ln = ln + 1
IF ln > 5 THEN ln = 1
IF ln < 1 THEN ln = 5
IF k$ <> "" THEN GOSUB ref
IF k$ = CHR$(0) + CHR$(59) THEN offf 15, 7: help: GOSUB ref: onn 15, 7: cadrs': LINE (186, 303.5)-(624, 303.5), 10
LOOP
rhh:
offs
SCREEN 0
GOSUB ende
END       '+++++++++++++++++++++++++++++++++++++++
sel:       '<=====================================Select
IF ln = 1 THEN GOSUB start
IF ln = 2 THEN GOSUB drvsel
IF ln = 3 THEN Options
IF ln = 4 THEN drvinfo (drvn$)
IF ln = 5 THEN k$ = CHR$(27)
COLOR 11, 1
CLS
GOSUB ref
RETURN
resetdata: '<=====================================Reset Data
OPEN "cleanup4.dat" FOR APPEND AS #1
CLOSE #1
OPEN "cleanup4.dat" FOR INPUT AS #1
z = 0
DO WHILE NOT EOF(1)
z = z + 1
ON ERROR RESUME NEXT
IF z = 1 THEN
INPUT #1, c$
c = VAL(c$)
IF c = 1 THEN c$ = " Normal   (Does not Change attribute) "
IF c = 2 THEN c$ = " Maximume (Change attribute of Deletable files) "
IF c = 3 THEN c$ = " Attriber (Change attribute of ALL files) "
END IF
IF z = 2 THEN
INPUT #1, BEP$
BEP = VAL(BEP$)
IF BEP = 1 THEN BEP$ = "Gorilla"
IF BEP = 2 THEN BEP$ = "Bethoven"
IF BEP = 3 THEN BEP$ = "Nibbles"
IF BEP = 4 THEN BEP$ = "ASP"
IF BEP = 0 THEN BEP$ = "Off"
END IF
IF z = 3 THEN
INPUT #1, ept$
ept = VAL(ept$)
IF ept = 1 THEN ept$ = "Yes"
IF ept = 0 THEN ept$ = "No"
END IF
IF z = 4 THEN
INPUT #1, sum$
sum = VAL(sum$)
IF sum = 0 THEN sum$ = "Never"
IF sum = 1 THEN sum$ = "Allways"
IF sum = 2 THEN sum$ = "When any file was Deleted"
END IF
IF z >= 5 THEN
INPUT #1, a$
a$ = LTRIM$(RTRIM$(UCASE$(a$)))
IF a$ = "==>" THEN kf$ = "1": EXIT DO ELSE kf$ = ""
IF LEFT$(a$, 2) = "*." THEN a$ = MID$(a$, 3)
IF LEN(a$) > 3 THEN a$ = LEFT$(a$, 3)
f$(z - 4) = a$
END IF
LOOP
zz = 0
hj = 0
DO WHILE NOT EOF(1)
IF kf$ = "" THEN INPUT #1, cus$ ELSE cus$ = "==>"

 IF cus$ = "==>" THEN

  IF EOF(1) THEN EXIT DO
   INPUT #1, name$
   IF EOF(1) THEN EXIT DO

    DO WHILE NOT EOF(1)
     zz = zz + 1
     IF zz > 10 THEN hj = 1: EXIT DO
     INPUT #1, cass$
     cass$ = LTRIM$(RTRIM$(UCASE$(cass$)))
     IF LEFT$(cass$, 2) <> "*." THEN cas$(zz) = cass$
    LOOP

  END IF

  IF EOF(1) OR hj = 1 THEN EXIT DO
 LOOP
CLOSE
RETURN
ref:       '<=====================================Refresh

qq = -1
GOSUB resetdata
COLOR 15, 1
LOCATE 2, 2
PRINT " 1   Start Clean Up ": LOCATE , 2
PRINT " 2   Drive Select   ": LOCATE , 2
PRINT " 3   Options   ": LOCATE , 2
PRINT " 4   Drive info.    ": LOCATE , 2
PRINT " 5   Exit   ": LOCATE , 2
COLOR 14
LOCATE 3, 25
FOR z = 1 TO 13
o$ = f$(z)
IF LEN(o$) = 1 THEN o$ = o$ + "  "
IF LEN(o$) = 2 THEN o$ = o$ + " "
PRINT o$; " ";
NEXT
LOCATE 4, 25
FOR z = 14 TO 13 + 13
o$ = f$(z)
IF LEN(o$) = 1 THEN o$ = o$ + "  "
IF LEN(o$) = 2 THEN o$ = o$ + " "
PRINT o$; " ";
NEXT
LOCATE 5, 25
FOR z = 26 TO 38
o$ = f$(z)
IF LEN(o$) = 1 THEN o$ = o$ + "  "
IF LEN(o$) = 2 THEN o$ = o$ + " "
PRINT o$; " ";
NEXT
kop = 11
kop2 = 0
COLOR kop, kop2
LOCATE 2, 25
PRINT "Group of Files to Delete (Maximume : 38 Groups) :   "
LOCATE 6, 25
PRINT "Name of Other Files to Delete :      "
LOCATE 7, 25
COLOR 14, 1
IF LEN(name$) > 52 THEN name$ = LEFT$(name$, 52)
PRINT name$
COLOR kop, kop2
LOCATE 8, 25
PRINT "Program Informations:       "
COLOR 10, 1
LOCATE 9, 25
PRINT "CleanUp IV_1.1 By Amin Safinezhad (ASP) Sep-2001 year"
COLOR kop, kop2
LOCATE 10, 25
PRINT "Drive(s) :        "
COLOR 7, 1
LOCATE 11, 25
PRINT "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z "
COLOR 8, 1
LOCATE 11, 27: PRINT "B"
COLOR 10, 1
FOR gh = 1 TO LEN(drvn$)
drvn2$ = MID$(drvn$, gh, 1)
drvn = ASC(drvn2$) - 64
LOCATE 11, drvn * 2 + 23
PRINT drvn2$
NEXT
COLOR 14, 1
FOR gh = 1 TO LEN(seld$)
seld2$ = MID$(seld$, gh, 1)
seld = ASC(seld2$) - 64
LOCATE 11, seld * 2 + 23
PRINT seld2$
NEXT
COLOR kop, kop2
LOCATE 13 + qq, 25
PRINT "Cleaner :        "
COLOR 14, 1
LOCATE 14 + qq, 25: PRINT LTRIM$(c$)
'²1>úCleaner...........Û 1/2/3 Î 1 : Normal.³ 2 : Maximume.³ 3 : Attriber..............þ
'²2>úBeep..............Û 1/0...Î ...........³ .............³ ..........................þ
'²3>úEmpty Directories.Û 1/0...Î ...........³ .............³ ..........................þ
'²4>úSummury...........Û 0/1/2 Î 0 : Never..³ 1 : Allways..³ 2 : When any file deleted.þ
COLOR kop, kop2
LOCATE 15 + qq, 25
PRINT "Beep :         "
COLOR 14, 1
LOCATE 16 + qq, 25: PRINT BEP$
COLOR kop, kop2
LOCATE 17 + qq, 25
PRINT "Clear empty Directories ? :      "
COLOR 14, 1
LOCATE 18 + qq, 25: PRINT ept$
COLOR kop, kop2
LOCATE 19 + qq, 25
PRINT "Display Summurry :       "
COLOR 14, 1
LOCATE 20 + qq, 25: PRINT sum$
COLOR , 1
COLOR 12
LINE (186, 303)-(624, 303), 10
LOCATE 21 + qq, 25
PRINT "F1 = Help"
RETURN

BEGIN:     '<=====================================Begin
LOCATE , , 0
FOR x = 1 TO 80
FOR y = 1 TO 23
ON ERROR RESUME NEXT
bh$(x, y) = CHR$(SCREEN(y, x))
bc(x, y) = SCREEN(y, x, 5)
NEXT: NEXT
ON ERROR GOTO rrr
OPEN "support.exe" FOR INPUT AS #1
CLOSE
FILES "bgi\*.chr"
FILES "bgi\*.bgi"
ON ERROR RESUME NEXT
LOCATE 22, 1
COLOR 10, 1
FOR x = 1 TO 25
PRINT SPACE$(80)
SOUND 0, .3
NEXT
SCREEN 12
cadr 0, 0, 640, 480, 0, 10
COLOR 14
ctr 14, "CleanUp's 4th Edition Ver 1.1"
COLOR 15
ctr 15, " Loading! Please Wait... "
COLOR 15, 1
LOCATE 12, 51, 1
GOSUB seldrv
LOCATE 1, 1, 0
ln = 1
'if s[1]='Textcolorlogoÿ' then i[3]:=1;
'if s[1]='Setcolordrvÿ' then BEGIN;S[2]:=PARAMSTR(2);i[3]:=2;END;
'if s[1]='Textbackgroundhelpÿ' then i[3]:=3;
'if s[1]='Gotoxyaboutÿ' then i[3]:=4;
SCREEN 0
SHELL "SUPPORT Textcolorlogoÿ"
SCREEN 12
GOSUB ref
RETURN
ende:      '<=====================================End
COLOR 7, 0: CLS
FOR x = 1 TO 80
FOR y = 1 TO 23
tc = bc(x, y)
IF tc > 31 THEN
tc = 7
END IF
COLOR tc, 0
LOCATE y, x
PRINT bh$(x, y)
NEXT: NEXT
RETURN
seldrv:     '<=====================================Select drives
SHELL "cd > pth.sav"
OPEN "pth.sav" FOR INPUT AS #2
INPUT #2, drv2$
CLOSE #2
KILL "PTH.SAV"
CHDIR (drv2$)
ffl = 0
last = .1
draws 300, 170, 22, 10
FOR ty = 0 TO 25
LINE (300, 170)-(SIN(6.28) * 22 + 300, COS(6.28) * 22 + 170), 12
drv$ = CHR$(65 + ty)
ON ERROR GOTO rr
IF drv$ = "B" THEN GOTO 5
CHDIR drv$ + ":\"
drvn$ = drvn$ + drv$
ffl = ffl + 1
IF INSTR(drvn$, "C") <> 0 THEN seld$ = "C"
IF INSTR(drvn$, "CDE") <> 0 THEN seld$ = "CDE"
IF seld$ = "" THEN IF INSTR(drvn$, "HI") <> 0 THEN seld$ = "HI"
IF seld$ = "" THEN IF INSTR(drvn$, "H") <> 0 THEN seld$ = "H"
IF seld$ <> "CDE" AND seld$ <> "HI" AND seld$ <> "H" THEN IF ffl > 1 THEN seld$ = drv$
5
ON ERROR RESUME NEXT
ty2 = ((ty) * 2 * 3.1416) / 25
ty2 = ty2
delay (1000)
LINE (300, 170)-(SIN(last) * 20 + 300, COS(last) * 20 + 170), 0
LINE (SIN(last) * 20 + 300, COS(last) * 20 + 170)-(SIN(last - .1) * 15 + 300, COS(last - .1) * 15 + 170), 0
LINE (SIN(last) * 20 + 300, COS(last) * 20 + 170)-(SIN(last + .1) * 15 + 300, COS(last + .1) * 15 + 170), 0

LINE (300, 170)-(SIN(ty2) * 20 + 300, COS(ty2) * 20 + 170), 10
LINE (SIN(ty2) * 20 + 300, COS(ty2) * 20 + 170)-(SIN(ty2 - .1) * 15 + 300, COS(ty2 - .1) * 15 + 170)
LINE (SIN(ty2) * 20 + 300, COS(ty2) * 20 + 170)-(SIN(ty2 + .1) * 15 + 300, COS(ty2 + .1) * 15 + 170)
last = ty2

NEXT
CLS
CHDIR drv2$
'GOSUB ref
IF seld$ = "" AND INSTR(drvn$, "A") <> 0 THEN seld$ = "A"
IF seld$ = "" THEN errl1
RETURN

drvsel:     '<=====================================Drive Select
lin = 1
col = 1
GOSUB ref
LINE (0, 0)-(178, 240), 0, BF
refd
GOSUB ffr
seldo$ = seld$
cadr 0, 0, 178, 70, 0, 11
cadr 0, 70, 178, 140, 0, 11
DO
j$ = INKEY$
IF j$ <> "" THEN
refd
GOSUB ffr
END IF
IF j$ = CHR$(27) THEN seld$ = seldo$: EXIT DO
IF j$ = CHR$(13) THEN EXIT DO
IF j$ = CHR$(0) + CHR$(72) THEN lin = lin - 1
IF j$ = CHR$(0) + CHR$(80) THEN lin = lin + 1
IF j$ = CHR$(0) + CHR$(75) THEN col = col - 1
IF j$ = CHR$(0) + CHR$(77) THEN col = col + 1
IF lin < 1 THEN lin = 3
IF col < 1 THEN col = 9
IF lin > 3 THEN lin = 1
IF col > 9 THEN col = 1
IF lin = 3 THEN IF col > 8 THEN col = 1
LOCATE lin + 5, col * 2
xx = ((col * 2) * 8) - 4
yy = (lin + 4) * 16 - 2
gg = 15
vv = 13 + 5
is$ = CHR$(SCREEN(lin + 5, col * 2 + 1))
LINE (xx, yy)-(xx + gg, yy + vv), 13, B

COLOR 7: ofg = 0
IF INSTR(drvn$, is$) <> 0 THEN COLOR 10: ofg = 0
IF INSTR(seld$, is$) <> 0 THEN COLOR 14: ofg = 1
IF is$ = "B" THEN COLOR 8, 4: ofg = 0
LOCATE lin + 5, col * 2 + 1
PRINT is$
COLOR 11
seldf$ = ""
IF j$ = CHR$(32) THEN
IF ofg = 1 THEN
FOR lng = 1 TO LEN(seld$)
are$ = MID$(seld$, lng, 1)
IF is$ <> are$ THEN seldf$ = seldf$ + are$
NEXT
seld$ = seldf$
END IF
IF INSTR(drvn$, is$) <> 0 THEN
IF ofg = 0 THEN
seld$ = seld$ + is$
END IF
END IF
refd
GOSUB ffr
END IF
LOOP
COLOR , 1: CLS
RETURN
ffr: '<++++++++++++++++++++++++++++++
FOR xx = 1 TO 9
FOR yy = 6 TO 8
is0$ = CHR$(SCREEN(yy, xx * 2 + 1))
IF INSTR(drvn$, is0$) <> 0 THEN COLOR 10 ELSE COLOR 7
IF INSTR(seld$, is0$) <> 0 THEN COLOR 14
IF is0$ = CHR$(8) THEN COLOR 8
IF is0$ = "B" THEN COLOR 8
LOCATE yy, xx * 2 + 1: PRINT is0$
COLOR 15, 1
COLOR 15, 1
NEXT
NEXT
FOR xx = 1 TO 9
FOR yy = 9 TO 11
is0$ = CHR$(SCREEN(yy, xx * 2 + 1))
IF INSTR(seld$, is0$) <> 0 THEN COLOR 14, 1: LOCATE yy, xx * 2 + 1: PRINT is0$: COLOR 15, 1
IF is0$ = CHR$(8) THEN COLOR 11: LOCATE yy, xx * 2 + 1: PRINT is0$: COLOR 15, 1
IF is0$ = "B" THEN COLOR 8: LOCATE yy, xx * 2 + 1: PRINT is0$: COLOR 15, 1
COLOR 15, 1
NEXT
NEXT
RETURN
'*****************************************************************************

start:     '<=====================================Start
OPO = 1
IF seld$ = "" THEN
COLOR 12, 4
LOCATE 2, 7, 1: PRINT "No Drives found"; : PLAY "g20": ad$ = INPUT$(1): LOCATE , , 0
GOTO nos
END IF
dll = 0
KILL "summ*.asp"
COLOR , 1: CLS
remenu
gh2 = LEN(seld$)
FOR gh = 1 TO LEN(seld$)
'COLOR , 1: CLS
drive$ = MID$(seld$, gh, 1) + ":\"
LOCATE 6 - 3, 17
FOR bb = 1 TO LEN(seld$)
dz$ = MID$(seld$, bb, 1)
IF dz$ = LEFT$(drive$, 1) THEN COLOR 14, 1 ELSE COLOR 6, 1
PRINT dz$; " ";
NEXT
COLOR 13
last = 0
LOCATE 7 - 3, 17: PRINT "Scanning...";
ON ERROR RESUME NEXT
CLOSE #1
KILL "asp_tree.sav"
SHELL "dir " + drive$ + " /a:d /b /s > asp_tree.sav"
OPEN "ASP_TREE.SAV" FOR APPEND AS #12
PRINT #12, LEFT$(drive$, 2)
CLOSE #12
remenu
'END
LOCATE 6, 17
FOR bb = 1 TO LEN(seld$)
dz$ = MID$(seld$, bb, 1)
IF dz$ = LEFT$(drive$, 1) THEN COLOR 14, 1 ELSE COLOR 6, 1
PRINT dz$; " ";
NEXT
COLOR 13
LOCATE 7 - 3, 17: PRINT "Initializing...";
OPEN "asp_tree.sav" FOR INPUT AS #1
iz = 0
jz = 0
DO WHILE NOT EOF(1)
ON ERROR RESUME NEXT
iz = iz + 1
iz$ = LTRIM$(RTRIM$(STR$(iz)))
COLOR 11: LOCATE 11 - 3, 17: PRINT iz$
INPUT #1, temp$
LOOP
CLOSE #1
remenu
LOCATE 6 - 3, 17
FOR bb = 1 TO LEN(seld$)
dz$ = MID$(seld$, bb, 1)
IF dz$ = LEFT$(drive$, 1) THEN COLOR 14, 1 ELSE COLOR 6, 1
PRINT dz$; " ";
NEXT
LOCATE 7 - 3, 17
COLOR 13: PRINT "Cleanning... ";
LOCATE 11, 17: PRINT "      "
OPEN "asp_tree.sav" FOR INPUT AS #1
tim$ = time2$
dat$ = date2$
COLOR 12: LOCATE 13 - 3, 17: PRINT tim$
'drive$ = Drive
'iz$    = Directories
'tim$   = Begin Time
'dat$   = Date
'ern    = Errors No.
'et$    = Elapsed time
tz = 0
minn! = 0
horn! = 0
amin! = INT(TIMER)
tec! = amin!
COLOR 4
LOCATE 21, 2
FOR td = 1 TO 78
COLOR 12
PRINT "±";
NEXT
COLOR 14, 4
ctr 22, " Stop "

COLOR 15, 1
DO WHILE NOT EOF(1)
ke$ = INKEY$
IF (ke$ = CHR$(27)) OR (ke$ = CHR$(13)) THEN
IF ogay = 1 THEN
FOR ff = 1 TO 17
CLOSE #ff:
NEXT
KILL "summ*.*"
KILL "asp_tree.sav"
GOTO ghes
ELSE
END IF
'NEXT
COLOR 15, 1
'END IF
END IF
INPUT #1, di$
tec2% = TIMER - tec!
minn! = tec2% \ 60
horn! = minn! \ 60
secn! = tec2% MOD 60
minn$ = LTRIM$(RTRIM$(STR$(minn!)))
horn$ = LTRIM$(RTRIM$(STR$(horn!)))
secn$ = LTRIM$(RTRIM$(STR$(secn!)))
IF LEN(minn$) = 0 THEN minn$ = "00"
IF LEN(minn$) = 1 THEN minn$ = "0" + minn$
IF LEN(secn$) = 0 THEN secn$ = "0"
IF LEN(secn$) = 1 THEN secn$ = "0" + secn$
IF LEN(horn$) = 0 THEN horn$ = "0"
IF LEN(horn$) = 1 THEN horn$ = "0" + horn$
et$ = horn$ + ":" + minn$ + ":" + secn$
COLOR 12
LOCATE 18 - 3 - 6, 17: PRINT et$
COLOR 14
LOCATE 18 - 3, 17: PRINT LTRIM$(c$)
di$ = LTRIM$(RTRIM$(UCASE$(di$)))
di2$ = di$
olp = 0
IF LEN(di$) > 78 - 18 THEN di2$ = "..." + RIGHT$(di$, 78 - 21): olp = 1
jzc% = (TIMER - jzb!) * ABS(iz - jz)
minn! = jzc% \ 60
horn! = minn! \ 60
secn! = jzc% MOD 60
minn$ = LTRIM$(RTRIM$(STR$(minn!)))
horn$ = LTRIM$(RTRIM$(STR$(horn!)))
secn$ = LTRIM$(RTRIM$(STR$(secn!)))
IF LEN(minn$) = 0 THEN minn$ = "00"
IF LEN(minn$) = 1 THEN minn$ = "0" + minn$
IF LEN(secn$) = 0 THEN secn$ = "0"
IF LEN(secn$) = 1 THEN secn$ = "0" + secn$
IF LEN(horn$) = 0 THEN horn$ = "0"
IF LEN(horn$) = 1 THEN horn$ = "0" + horn$
etm$ = horn$ + ":" + minn$ + ":" + secn$
COLOR 14
IF ABS(jzc% - lst%) > 1 THEN LOCATE 18 - 2, 17: PRINT etm$; " "
lst% = jzc%
jz = jz + 1
jzb! = TIMER
jz$ = LTRIM$(RTRIM$(STR$(jz)))
tte = 0
tte = (100 * jz) \ iz
ty = jz
LINE (8, 320)-(10 + (tte * (620 / 100)), 335), 11, BF
LINE (myx, myy)-(SIN(6.28) * myre + myx, COS(6.28) * myre + myy), 12
ty2 = ((ty) * 2 * 3.1416) / iz
ty2 = ty2
LINE (myx, myy)-(SIN(last) * (myre - 2) + myx, COS(last) * (myre - 2) + myy), 0
LINE (myx, myy)-(SIN(ty2) * (myre - 2) + myx, COS(ty2) * (myre - 2) + myy), 10
last = ty2
COLOR 12
LOCATE
LOCATE 8 - 3, 17: PRINT SPACE$((79 - 18) + 1)
LOCATE 11 - 3, 17: COLOR 11: PRINT jz$ + " / " + iz$ + " ( Remained : "; LTRIM$(RTRIM$(STR$(ABS(iz - jz)))); " )  "
COLOR 10: LOCATE 8 - 3, 17: PRINT di2$
don$ = LTRIM$(RTRIM$(STR$((100 * jz) \ iz)))
COLOR 12: LOCATE 14 - 3, 17: PRINT time2$
IF LEN(don$) < 2 THEN don$ = "ú" + don$
COLOR 13
LOCATE 10 - 3, 17: PRINT don$
IF c = 3 THEN COLOR 13: LOCATE 9 - 3, 17: PRINT "Attribbing..."; SPACE$(20): LOCATE 24, 2: SHELL "attrib " + di$ + "\*.* -r -s -h +a >nul"
IF INT(TIMER * 10) MOD 10 = 0 THEN
zxb = RND * (640)
zyb = RND * (480 - 370) + 370
zcb = RND * 13 + 1
CIRCLE (zxb, zyb), RND * 3, zcb
PAINT (zxb, zyb), RND * 15, zcb
END IF
FOR z = 1 TO 38
IF f$(z) = "" THEN EXIT FOR
'amin$

ON ERROR GOTO dllp1

COLOR 13
LOCATE 9 - 3, 17: PRINT "*." + f$(z);
IF c = 2 THEN PRINT " ( + Attribbing it)"; : LOCATE 24, 2: SHELL "attrib " + di$ + "\*." + f$(z)
PRINT SPACE$(20)
KILL di$ + "\*." + f$(z)
dll = 1
dlp1:
NEXT
FOR z = 1 TO 20
IF cas$(z) = "" THEN EXIT FOR
ON ERROR GOTO dllp2
COLOR 13
LOCATE 9 - 3, 17: PRINT cas$(z);
IF c = 2 THEN PRINT " ( + Attribbing it)"; : LOCATE 24, 2: SHELL "attrib " + di$ + "\" + cas$(z)
PRINT SPACE$(20)
KILL di$ + "\" + cas$(z)
dll = 1
dlp2:
NEXT
ON ERROR RESUME NEXT
IF ept = 1 THEN COLOR 13: LOCATE 9 - 3, 17: PRINT "Empty Directories    "; SPACE$(20): RMDIR di$
COLOR 13: LOCATE 9 - 3, 17: PRINT "Nothing"; SPACE$(13)
LOOP
remenu
'LOCATE 21, 79: COLOR 10, 1: PRINT CHR$(8)
'LOCATE 21, 80: COLOR 11, 1: PRINT "³": COLOR 15, 1
CLOSE #1
KILL "asp_tree.sav"
IF BEP = 3 THEN PLAY "MBO0L16>CCCE": PLAY "MBO0L16>CCCE"
IF BEP = 1 THEN PLAY "T160O0L32EFGEFDC": PLAY "T160O0L32EFGEFDC"
IF BEP = 2 THEN PLAY "P24P8L8FFFL2D"
IF BEP = 4 THEN PLAY "a60b60g60d60c60>>a60<g60<": PLAY "<b60>>g60<a60>a60<"
PLAY "MF a60 MF"
COLOR , 1: CLS
remenu
gh$ = LTRIM$(RTRIM$(STR$(gh)))
OPEN "summ" + gh$ + ".asp" FOR OUTPUT AS #6
PRINT #6, "ÛÛ ASP soft Cleanup Ver IV - 1.1 ÛÛ"
PRINT #6, "þ Drive        : " + drive$
PRINT #6, "þ Elapsed Time : " + et$
PRINT #6, "þ Begin Time   : " + tim$
PRINT #6, "þ End Time     : " + time2$
PRINT #6, "þ Date         : " + date2$
PRINT #6, "þ Directories  : " + iz$
IF c = 1 THEN c2$ = "Normal"
IF c = 2 THEN c2$ = "Maximume"
IF c = 3 THEN c2$ = "Attriber"
PRINT #6, "þ Cleaner mode : " + c2$
PRINT #6, "þ No Error(s)    "
CLOSE #6
NEXT
IF BEP = 3 THEN PLAY "mf T160O1>L20CDEDCDL10ECC< T150L40 mf": PLAY "mf >>t160l10CDL10ECC<< mf"
IF BEP = 1 THEN
    PLAY "t120o1l16b9n0baan0bn0bn0baaan0b9n0baan0b"
    PLAY "o2l16e-9n0e-d-d-n0e-n0e-n0e-d-d-d-n0e-9n0e-d-d-n0e-"
    PLAY "o2l16g-9n0g-een0g-n0g-n0g-eeen0g-9n0g-een0g-"
    PLAY "o2l16b9n0baan0g-n0g-n0g-eeen0o1b9n0baan0b"
END IF
IF BEP = 2 THEN
PLAY "MF T180o2P2P8L8GGGL2E-P24P8L8FFFL2D"
PLAY "MF T180o2P2P8L8GGGL2E-P24P8L8FFFL2D"
END IF
IF BEP = 3 THEN
PLAY "T160O1>L20CDEDCDL10ECC"
PLAY "T160O1>L20CDEDCDL10ECC"
END IF
IF BEP = 4 THEN
FElise$ = "o3 L8 E D+ E D+ E o2 B o3 D C L2 o2 A": PLAY "MF X" + VARPTR$(FElise$)
END IF
IF BEP = 0 THEN PLAY ">a20b20<"
FOR rrt1 = 1 TO 300
asun$ = INKEY$
NEXT


IF (sum = 1) OR (sum = 2 AND dll = 1) THEN

FOR tu = 1 TO gh - 1
 COLOR 14, 1: CLS
cadr 60, 100, 600, 250, 0, 10
 COLOR 11
COLOR 10
'gh$ = LTRIM$(RTRIM$(STR$(xx)))
CLOSE #6
gh3$ = LTRIM$(RTRIM$(STR$(gh2)))
gh2$ = LTRIM$(RTRIM$(STR$(tu)))
COLOR 14: ctr 6, "Summurry : " + gh2$ + " / " + gh3$
OPEN "summ" + gh2$ + ".asp" FOR INPUT AS #6
FOR tx = 1 TO 9
INPUT #6, a$
COLOR 10
IF tx > 1 THEN
LOCATE 6 + tx, 12
PRINT a$
ELSE
COLOR 12: ctr 7, "Cleanup Completed !"
END IF
NEXT
COLOR 14, 4
LOCATE 5, 1: PRINT SPACE$(80)
ctr 5, "ASP soft Cleanup IV Ver. 1.1"
CLOSE #6
COLOR 13, 13
ctr 17, " ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ» ": COLOR 8, 1: PRINT " ": COLOR 13, 13
ctr 18, " ºPress any key to Countinue º ": COLOR 8, 1: PRINT "Û": COLOR 13, 13
ctr 19, " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ ": COLOR 8, 1: PRINT "Û"
ctr 20, "  ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß"
LOCATE 18, 52, 1
as$ = INPUT$(1)
LOCATE , , 0
NEXT
END IF
ghes:
nos:
RETURN

'#############################################################################

rr:        '<=====================================Error at Select drive
RESUME 5
rr1:        '<====================Error at Start Progeam for Writing into Path
 COLOR 12, 0
 ctr 8, "             "
LOCATE 1, 1
COLOR 12, 1
 ctr 9, " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ ": COLOR , 0: PRINT " ": COLOR 12, 1
ctr 10, " ³ Sorry ! you Can't run this Program in this Path/Drive ³ ": COLOR , 0: PRINT " ": COLOR 12, 1
ctr 11, " ³ Becuase the drive is full or path is locked for you!! ³ ": COLOR , 0: PRINT " ": COLOR 12, 1
ctr 12, " ³  Copy this program & 'cleanup4.dat' file (if exist!)  ³ ": COLOR , 0: PRINT " ": COLOR 12, 1
ctr 13, " ³ to another Path/Drive and Run CleanUp.Exe ... Thanks. ³ ": COLOR , 0: PRINT " ": COLOR 12, 1
ctr 14, " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ ": COLOR , 0: PRINT " ": COLOR 12, 1
 COLOR 12, 0
 ctr 16, "              "
  oops = 0
  COLOR 8, 0: LOCATE 9, 8 + oops
  PRINT " Ü": LOCATE , 8 + oops
  PRINT " Û": LOCATE , 8 + oops
  PRINT " Û": LOCATE , 8 + oops
  PRINT " Û": LOCATE , 8 + oops
  PRINT " Û": LOCATE , 8 + oops
  PRINT " Û": LOCATE , 8 + oops
  PRINT " ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß"
LOCATE , , 1
COLOR 10, 1
ctr 14, " Press any key to Exit..."
COLOR 7, 0
PLAY "mf d05g05 mf"
asz$ = INPUT$(1)
COLOR 12, 1
ctr 14, " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ ": COLOR , 0: PRINT " ": COLOR 12, 1
COLOR 7, 0
LOCATE 23, 1, 0
END
dllp1:
RESUME dlp1
dllp2:
RESUME dlp2
rrr:        '<====================Error at Start Progeam for missed files
 COLOR 12, 0
' ctr 8, "             "
LOCATE 1, 1
COLOR 12, 1
 ctr 9, " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ ": COLOR , 0: PRINT " ": COLOR 12, 1
ctr 10, " ³ Sorry ! you Can't run this Program without is's files ³ ": COLOR , 0: PRINT " ": COLOR 12, 1
ctr 11, " ³CleanUp's files not found : Support.exe , Bgi\*.chr,bgi³ ": COLOR , 0: PRINT " ": COLOR 12, 1
ctr 12, " ³ If you want to Run CleanUp Restore original files...  ³ ": COLOR , 0: PRINT " ": COLOR 12, 1
ctr 13, " ³þCleanUp.exe needs 2.5MB of free space to work...Thanks³ ": COLOR , 0: PRINT " ": COLOR 12, 1
ctr 14, " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ ": COLOR , 0: PRINT " ": COLOR 12, 1
 COLOR 12, 0
 ctr 16, "              "
  oops = 0
  COLOR 8, 0: LOCATE 9, 8 + oops
  PRINT " Ü": LOCATE , 8 + oops
  PRINT " Û": LOCATE , 8 + oops
  PRINT " Û": LOCATE , 8 + oops
  PRINT " Û": LOCATE , 8 + oops
  PRINT " Û": LOCATE , 8 + oops
  PRINT " Û": LOCATE , 8 + oops
  PRINT " ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß "
LOCATE , , 1
COLOR 10, 1
ctr 14, " Press any key to Exit..."
COLOR 7, 0
PLAY "mf d05g05 mf"
asz$ = INPUT$(1)
COLOR 12, 1
ctr 14, " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ ": COLOR , 0: PRINT " ": COLOR 12, 1
COLOR 7, 0
LOCATE 23, 1, 0
END

DEFSNG A-Z
SUB bbp (BEP!)
ON ERROR RESUME NEXT
B = BEP
IF BEP = 1 THEN BEP$ = "Gorilla"
IF BEP = 2 THEN BEP$ = "Bethoven"
IF BEP = 3 THEN BEP$ = "Nibbles"
IF BEP = 4 THEN BEP$ = "ASP"
IF BEP = 0 THEN BEP$ = "Off"
GOSUB somr
ln = 1
c = BEP
cc = B
cadr 0, 160, 179, 280, 0, 10
DO
a$ = INKEY$
IF a$ <> "" THEN GOSUB somr
IF a$ = CHR$(0) + CHR$(72) THEN ln = ln - 1
IF a$ = CHR$(0) + CHR$(80) THEN ln = ln + 1
IF a$ = CHR$(27) THEN GOTO endsubr
IF a$ = CHR$(13) THEN
IF ln < 6 THEN cc = ln: cc$ = LTRIM$(RTRIM$(STR$(cc))): GOTO passedr ELSE GOTO endsubr
END IF
IF ln > 6 THEN ln = 1
IF ln < 1 THEN ln = 6
COLOR 10, 4
LOCATE ln + 11, 2
PRINT " " + LTRIM$(RTRIM$(STR$(ln))) + " ";
COLOR 12
LOCATE ln + 11, 2
LOCATE , 5
IF ln = BEP THEN PRINT "û";  ELSE IF ln <> 5 THEN PRINT " ";
IF BEP = 0 THEN
IF ln = 5 THEN LOCATE ln + 11, 5: PRINT "û";
ELSE
IF ln = 5 THEN PRINT " ";
END IF
COLOR 14
IF ln = 1 THEN PRINT "Gorilla"
IF ln = 2 THEN PRINT "Bethoven"
IF ln = 3 THEN PRINT "Nibbles"
IF ln = 4 THEN PRINT "ASP"
IF ln = 5 THEN PRINT "Off"
IF ln = 6 THEN PRINT "Cancle"

LOOP
END
GOTO endsubr
somr:
LOCATE 12, 2
COLOR 15, 1
'²1>úCleaner...........Û 1/2/3 Î 1 : Normal.³ 2 : Maximume.³ 3 : Attriber..............þ
'²2>úBeep..............Û 1/0...Î ...........³ .............³ ..........................þ
'²3>úEmpty Directories.Û 1/0...Î ...........³ .............³ ..........................þ
'²4>úSummury...........Û 0/1/2 Î 0 : Never..³ 1 : Allways..³ 2 : When any file deleted.þ
'IF ln = 1 THEN PRINT "Gorilla"
'IF ln = 2 THEN PRINT "Bethoven"
'IF ln = 3 THEN PRINT "Nibbles"
'IF ln = 4 THEN PRINT "ASP"
'IF ln = 5 THEN PRINT "Off"
'IF ln = 6 THEN PRINT "Cancle"
PRINT " 1  Gorilla   ": LOCATE , 2
PRINT " 2  Bethoven   ": LOCATE , 2
PRINT " 3  Nibbles  ": LOCATE , 2
PRINT " 4  ASP   ": LOCATE , 2
PRINT " 5  Off   ": LOCATE , 2
PRINT " 6  Cancle   ": LOCATE , 2

COLOR 12
IF BEP = 0 THEN BEP = 5
LOCATE BEP + 11, 5
PRINT "û"
IF BEP = 5 THEN BEP = 0
COLOR 15
RETURN

passedr:
B$ = LTRIM$(RTRIM$(STR$(B)))
OPEN "cleanup4.dat" FOR INPUT AS #4
OPEN "what.led" FOR OUTPUT AS #5
zx = 0
DO WHILE NOT EOF(4)
zx = zx + 1
INPUT #4, a$
IF cc$ = "5" THEN cc$ = "0"
IF zx <> 2 THEN PRINT #5, a$ ELSE PRINT #5, cc$
LOOP
CLOSE #4, #5
KILL "cleanup4.dat"
NAME "what.led" AS "cleanup4.dat"
endsubr:
END SUB

SUB cadr (x1, y1, x2, y2, dly, colr)
x1 = x1 + 6
y1 = y1 + 6
x2 = x2 - 6
y2 = y2 - 6
LINE (x1, y1)-(x2, y2), colr, B
FOR i = 1 TO 5
j = i
IF colr = 10 THEN setrgbpalette j, 0, INT(i * 6.3), 0
IF colr = 11 THEN setrgbpalette j, 0, INT(i * 6.3), INT(i * 6.3)
IF colr = 12 THEN setrgbpalette j, INT(i * 6.3), 0, 0
IF colr = 13 THEN setrgbpalette j, INT(i * 6.3), 0, INT(i * 6.3)
IF colr = 14 THEN setrgbpalette j, INT(i * 6.3), INT(i * 6.3), 0
IF colr = 15 THEN setrgbpalette j, INT(i * 6.3), INT(i * 6.3), INT(i * 6.3)
LINE (x1 - i - 1, y1 - i - 1)-(x2 + i + 1, y2 + i + 1), j, B
NEXT
END SUB

DEFINT A-Z
SUB cadrs
cadr 0, 103, 180, 240, 0, 10
cadr 0, 0, 180, 103, 0, 10
draws 80, 200, 20, 13
CIRCLE (80, 178), 2, 14
PAINT (80, 178), 14, 14
cadr 180, 0, 630, 330, 0, 10
CIRCLE (100, 200), 1, 15
CIRCLE (80, 220), 1, 15
'cadr 0, 110, 180, 240, 0, 10
CIRCLE (60, 200), 1, 15
END SUB

DEFSNG A-Z
SUB cadrs2
cadr 0, 0, 180, 160, 0, 10
cadr 180, 0, 630, 330, 0, 10
END SUB

SUB CIRCLEs (x, y, r, c, dly)
CIRCLE (x, y), r, c
FOR t = 1 TO 4 STEP 1
IF c = 10 THEN setrgbpalette t, 0, INT(t * 6.3 * 2), 0
IF c = 11 THEN setrgbpalette t, 0, INT(t * 6.3 * 2), INT(t * 6.3 * 2)
IF c = 12 THEN setrgbpalette t, INT(t * 6.3 * 2), 0, 0
IF c = 13 THEN setrgbpalette t, INT(t * 6.3 * 2), 0, INT(t * 6.3 * 2)
IF c = 14 THEN setrgbpalette t, INT(t * 6.3 * 2), INT(t * 6.3 * 2), 0
IF c = 15 THEN setrgbpalette t, INT(t * 6.3 * 2), INT(t * 6.3 * 2), INT(t * 6.3 * 2)
CIRCLE (x, y), r + t + 1, t
delay (dly)
NEXT
END SUB

SUB cln (c)
ON ERROR RESUME NEXT
GOSUB som1
ln = 1
cc = c
cadr 0, 160, 179, 257, 0, 10
DO
a$ = INKEY$
IF a$ <> "" THEN GOSUB som1
IF a$ = CHR$(0) + CHR$(72) THEN ln = ln - 1
IF a$ = CHR$(0) + CHR$(80) THEN ln = ln + 1
IF a$ = CHR$(27) THEN GOTO endsub1
IF a$ = CHR$(13) THEN
IF ln < 4 THEN cc = ln: cc$ = LTRIM$(RTRIM$(STR$(cc))): GOTO pressed1 ELSE GOTO endsub1
END IF
IF ln > 4 THEN ln = 1
IF ln < 1 THEN ln = 4
COLOR 10, 4
LOCATE ln + 11, 2
PRINT " " + LTRIM$(RTRIM$(STR$(ln))) + " ";
COLOR 12
LOCATE , 5
IF ln = c THEN PRINT "û";  ELSE PRINT " ";
COLOR 14
IF ln = 1 THEN PRINT "Normal      "
IF ln = 2 THEN PRINT "Maximume      "
IF ln = 3 THEN PRINT "Attriber      "
IF ln = 4 THEN PRINT "Cancle      "
LOOP
END
GOTO endsub1
som1:
LOCATE 12, 2
COLOR 15, 1
'²1>úCleaner...........Û 1/2/3 Î 1 : Normal.³ 2 : Maximume.³ 3 : Attriber..............þ
'²2>úBeep..............Û 1/0...Î ...........³ .............³ ..........................þ
'²3>úEmpty Directories.Û 1/0...Î ...........³ .............³ ..........................þ
'²4>úSummury...........Û 0/1/2 Î 0 : Never..³ 1 : Allways..³ 2 : When any file deleted.þ
PRINT " 1  Normal   ": LOCATE , 2
PRINT " 2  Maximume   ": LOCATE , 2
PRINT " 3  Attriber   ": LOCATE , 2
PRINT " 4  Cancle   ": LOCATE , 2
COLOR 12
LOCATE c + 11, 5
PRINT "û"
COLOR 15
RETURN

pressed1:
OPEN "cleanup4.dat" FOR INPUT AS #4
OPEN "what.led" FOR OUTPUT AS #5
zx = 0
DO WHILE NOT EOF(4)
zx = zx + 1
INPUT #4, a$
IF zx <> 1 THEN PRINT #5, a$ ELSE PRINT #5, cc$
LOOP
CLOSE #4, #5
KILL "cleanup4.dat"
NAME "what.led" AS "cleanup4.dat"
GOTO endsub1

endsub1:
END SUB

SUB ctr (y, TEXT$)
ON ERROR RESUME NEXT
N = LEN(TEXT$)
IF N > 78 THEN TEXT$ = LEFT$(TEXT$, 78): N = LEN(TEXT$)
M = (80 - N) \ 2
LOCATE y, 1: FOR g = 1 TO M
x = x + 1
NEXT g
LOCATE y, x
PRINT TEXT$;
END SUB

FUNCTION date2$
mounth = VAL(DATE$)
tgt = INSTR(DATE$, "-")
day = VAL(MID$(DATE$, tgt + 1))
tgt2 = INSTR(MID$(DATE$, tgt + 1), "-")
year = VAL(MID$(MID$(DATE$, tgt + 1), tgt2 + 1))
date2$ = str2$(year) + "/" + str2$(mounth) + "/" + str2$(day)
END FUNCTION

SUB defg
ON ERROR RESUME NEXT
DIM f$(38)
GOSUB opt
ln = 1
cadr 0, 160, 179, 250, 0, 10
DO
LOCATE ln + 11, 2
COLOR 10, 4: PRINT ln;
LOCATE ln + 11, 6
COLOR 14, 4
IF ln = 1 THEN PRINT " Add new Group  "
IF ln = 2 THEN PRINT " Delete a Group "
IF ln = 3 THEN PRINT " Restore Defult "
IF ln = 4 THEN PRINT " Exit       "
COLOR 15, 1
k$ = INKEY$
IF k$ = CHR$(13) THEN GOSUB self
IF k$ = CHR$(27) THEN EXIT DO
IF k$ = CHR$(0) + CHR$(72) THEN ln = ln - 1
IF k$ = CHR$(0) + CHR$(80) THEN ln = ln + 1
IF ln > 4 THEN ln = 1
IF ln < 1 THEN ln = 4
IF k$ <> "" THEN GOSUB opt
LOOP
GOTO endo
self:
IF ln < 3 THEN
LOCATE ln + 11, 7
COLOR 11, 4
'      67890123456789
xv = 9
PRINT "Enter Group:"; : COLOR 8: PRINT "úúú"
stri$ = ""
NN$ = "* []:;|\/><.,+" + CHR$(34)
LOCATE ln + 11, 10 + xv, 1
DO
14
IF k$ = CHR$(8) THEN
COLOR 8
LOCATE ln + 11, 10 + xv, 1
IF xv <> 9 THEN PRINT "úúú" ELSE PRINT "úúú"
COLOR 10
LOCATE ln + 11, 10 + xv, 1
PRINT stri$;
END IF
k$ = INKEY$
IF k$ = "" THEN GOTO 14
k$ = UCASE$(k$)
IF INSTR(NN$, k$) <> 0 THEN PLAY "a60": GOTO 14
IF k$ = CHR$(13) THEN EXIT DO
IF k$ = CHR$(27) THEN LOCATE , , 0: k$ = "": GOSUB opt: GOTO endoo
IF LEN(k$) > 1 THEN PLAY "g60": GOTO 14
IF k$ = CHR$(8) AND LEN(stri$) > 1 THEN stri$ = LEFT$(stri$, LEN(stri$) - 1): GOTO 14
IF k$ = CHR$(8) AND LEN(stri$) > 0 AND LEN(stri$) < 2 THEN stri$ = "": GOTO 14
IF k$ = CHR$(8) AND LEN(stri$) = 0 THEN PLAY "b60": GOTO 14
IF k$ = CHR$(9) THEN PLAY "f60": GOTO 14
IF LEN(stri$) = 3 THEN PLAY "c60": GOTO 14
IF stri$ = "??" AND k$ = "?" THEN PLAY "e60": GOTO 14
stri$ = stri$ + k$
COLOR 8
LOCATE ln + 11, 10 + xv, 1
IF xv <> 9 THEN PRINT "úúú" ELSE PRINT "úúú"
COLOR 10
LOCATE ln + 11, 10 + xv, 1
PRINT stri$;
LOOP
ext = 0
FOR z = 1 TO 38
IF stri$ = f$(z) THEN ext = 1
NEXT
LOCATE , , 0
END IF
IF ln = 1 THEN GOSUB add
IF ln = 2 THEN GOSUB del
IF ln = 3 THEN GOSUB stor
IF ln = 4 THEN GOTO endo
GOSUB opt
endoo:
RETURN
opt:
LOCATE 12, 2
COLOR 15, 1
PRINT " 1   Add new Group  ": LOCATE , 2
PRINT " 2   Delete a Group ": LOCATE , 2
PRINT " 3   Restore Defult ": LOCATE , 2
PRINT " 4   Exit    ": LOCATE , 2
OPEN "cleanup4.dat" FOR APPEND AS #1
CLOSE #1
OPEN "cleanup4.dat" FOR INPUT AS #1
FOR gx = 1 TO 4
IF EOF(1) THEN EXIT FOR
INPUT #1, c$
NEXT
z = 4
DO WHILE NOT EOF(1)
z = z + 1
ON ERROR RESUME NEXT
IF z >= 5 THEN
INPUT #1, a$
a$ = LTRIM$(RTRIM$(UCASE$(a$)))
IF a$ = "==>" THEN kf$ = "1": EXIT DO:  ELSE kf$ = ""
IF LEFT$(a$, 2) = "*." THEN a$ = MID$(a$, 3)
IF LEN(a$) > 3 THEN a$ = LEFT$(a$, 3)
f$(z - 4) = a$
END IF
LOOP
zz = 0
hj = 0
CLOSE
COLOR 15, 1
LOCATE 3, 25: PRINT SPACE$(52)
LOCATE 4, 25: PRINT SPACE$(52)
LOCATE 5, 25: PRINT SPACE$(52)
COLOR 14, 1
LOCATE 3, 25
FOR z = 1 TO 13
o$ = f$(z)
IF LEN(o$) = 1 THEN o$ = o$ + "  "
IF LEN(o$) = 2 THEN o$ = o$ + " "
PRINT o$; " ";
NEXT
LOCATE 4, 25
FOR z = 14 TO 13 + 13
o$ = f$(z)
IF LEN(o$) = 1 THEN o$ = o$ + "  "
IF LEN(o$) = 2 THEN o$ = o$ + " "
PRINT o$; " ";
NEXT
LOCATE 5, 25
FOR z = 26 TO 38
o$ = f$(z)
IF LEN(o$) = 1 THEN o$ = o$ + "  "
IF LEN(o$) = 2 THEN o$ = o$ + " "
PRINT o$; " ";
NEXT
RETURN

add:
IF f$(38) <> "" THEN
COLOR 12, 4
LOCATE ln + 11, 7, 1: PRINT "More than 38 !!"; : PLAY "g20": ad$ = INPUT$(1): LOCATE , , 0
GOTO endff
END IF
IF ext = 1 THEN
sf$ = stri$
IF LEN(stri$) = 0 THEN sf$ = sf$ + "   "
IF LEN(stri$) = 1 THEN sf$ = sf$ + "  "
IF LEN(stri$) = 2 THEN sf$ = sf$ + " "
COLOR 12, 4
LOCATE ln + 11, 7, 1: PRINT sf$ + " Exist!!!   "; : PLAY "g20": ad$ = INPUT$(1): LOCATE , , 0
GOTO endff
END IF                         'Enter Group:..."
FOR zxc = 1 TO 38
f$(zxc) = ""
NEXT
OPEN "cleanup4.dat" FOR INPUT AS #11
OPEN "what.led" FOR OUTPUT AS #12
FOR tg = 1 TO 4
IF EOF(11) THEN GOTO endfff
INPUT #11, s1$
PRINT #12, s1$
NEXT
PRINT #12, "*." + stri$
DO WHILE NOT EOF(11)
INPUT #11, s1$
PRINT #12, s1$
LOOP
endfff:
CLOSE #11, #12
KILL "cleanup4.dat"
NAME "what.led" AS "cleanup4.dat"
endff:
RETURN

del:
IF ext = 0 THEN
COLOR 12, 4
'                            Enter Group:...
'                            ... not exist!
sf$ = stri$
IF LEN(stri$) = 0 THEN sf$ = sf$ + "   "
IF LEN(stri$) = 1 THEN sf$ = sf$ + "  "
IF LEN(stri$) = 2 THEN sf$ = sf$ + " "
LOCATE ln + 11, 7, 1: PRINT sf$ + " Not Exist!!"; : PLAY "g20": ad$ = INPUT$(1): LOCATE , , 0
GOTO endooo
END IF
FOR zxc = 1 TO 38
f$(zxc) = ""
NEXT
OPEN "wHAT.led" FOR OUTPUT AS #12
OPEN "cleanup4.dat" FOR INPUT AS #13
hold = 0
DO WHILE NOT EOF(13)
INPUT #13, a1$
IF hold = 0 THEN a1$ = LTRIM$(RTRIM$(UCASE$(a1$)))
IF hold = 1 THEN a1$ = LTRIM$(RTRIM$(a1$))
GRP$ = "*." + stri$
IF a1$ = "==>" THEN hold = 1
IF a1$ <> GRP$ THEN PRINT #12, a1$
LOOP
FOR zx1 = 1 TO 38
f$(zx1) = ""
NEXT
CLOSE : CLOSE :
ON ERROR RESUME NEXT
KILL "cleanup4.dat"
NAME "WHAT.LED" AS "cleanup4.dat"
endooo:
RETURN
stor:
IF sure(11, 1) = 1 THEN
FOR zxc = 1 TO 38
f$(zxc) = ""
NEXT
'FOR zxc = 1 TO 20
'cas$(zxc) = ""
'ww$(zxc) = ""
'NEXT
OPEN "cleanup4.dat" FOR INPUT AS #11
OPEN "what.led" FOR OUTPUT AS #12
FOR tg = 1 TO 4
IF EOF(11) THEN GOTO endi
INPUT #11, s1$
PRINT #12, s1$
NEXT
endi:
PRINT #12, "*.SWP,*.TMP,*.$$$,*.BAK,*.NCD,*.OBJ,*.DSK,*.PSM,*.DPR,*.TC"
DO WHILE NOT EOF(11)
INPUT #11, s1$
IF s1$ = "==>" THEN
PRINT #12, "==>"
DO WHILE NOT EOF(11)
INPUT #11, s1$
PRINT #12, s1$
LOOP
END IF
LOOP
CLOSE : CLOSE
KILL "cleanup4.dat"
NAME "what.led" AS "cleanup4.dat"
LOCATE 6, 24
COLOR 11, 0
'PRINT " ASP soft Selected files 1 (Maximume : 20 Files) :   "
'LOCATE 7, 25
'COLOR 14, 1
'PRINT "$$RAR$$.*"
COLOR 15, 1
END IF
RETURN
endo:
LOCATE , , 0
END SUB

SUB defo
ON ERROR RESUME NEXT
DIM cas$(20)
DIM ww$(20)
GOSUB opti
ln = 1
cadr 0, 160, 179, 270, 0, 10
DO
LOCATE ln + 11, 2, 0
COLOR 10, 4: PRINT ln;
LOCATE , 5
COLOR 14, 4
IF ln = 1 THEN PRINT " Change Name    "
IF ln = 2 THEN PRINT " Add new file   "
IF ln = 3 THEN PRINT " Delete a file  "
IF ln = 4 THEN PRINT " Restore Defult "
IF ln = 5 THEN PRINT " Exit       "
COLOR 15, 1
k$ = INKEY$
IF k$ = CHR$(13) THEN GOSUB self2
IF k$ = CHR$(27) THEN EXIT DO
IF k$ = CHR$(0) + CHR$(72) THEN ln = ln - 1
IF k$ = CHR$(0) + CHR$(80) THEN ln = ln + 1
IF ln > 5 THEN ln = 1
IF ln < 1 THEN ln = 5
IF k$ <> "" THEN GOSUB opti
LOOP
GOTO endg

self2:                                                                      'More than 38 !!
IF ln = 2 THEN IF cas$(20) <> "" THEN PLAY "g60": LOCATE ln + 11, 6: COLOR 12, 4: PRINT "More than 20 !!!": o$ = INPUT$(1): COLOR 15, 1: GOTO ghos
IF ln < 4 AND ln > 1 THEN
LOCATE ln + 11, 6
COLOR 11, 4
'      67890123456789
PRINT "Is:"; : COLOR 8: PRINT ",,,,,,,,"; : COLOR 15: PRINT "."; : COLOR 8: PRINT ",,,"
stri$ = ""
NN$ = " []:;|\/><,+" + CHR$(34)
LOCATE ln + 11, 10 + xv, 1
vx = 3
xv = 0
vx2 = 8
stri$ = ""
k$ = ""
j = 0

DO
140
IF k$ = CHR$(8) THEN
COLOR 8
LOCATE ln + 11, 9 + xv, 1
IF xv <> 9 THEN PRINT ",,,,,,,,"; : COLOR 15: PRINT "."; : COLOR 8: PRINT ",,," ELSE PRINT ",,,"
COLOR 10
LOCATE ln + 11, 9 + xv, 1
PRINT stri$;
END IF
k$ = INKEY$
IF j = 0 THEN j = 1: LOCATE ln + 11, 10
IF k$ = "" THEN GOTO 140
k$ = UCASE$(k$)
IF INSTR(NN$, k$) <> 0 THEN PLAY "a60": GOTO 140
IF k$ = CHR$(13) THEN EXIT DO
IF k$ = CHR$(27) THEN k$ = "": LOCATE , , 0: GOSUB opti: GOTO endoo2
IF LEN(k$) > 1 THEN PLAY "g60": GOTO 140
IF k$ = "*" AND xv = 0 THEN PLAY "a60": GOTO 140
IF k$ = CHR$(8) AND LEN(stri$) <= 1 AND vx2 = 3 THEN
LOCATE ln + 11, 9 + xv, 1
COLOR 8, 4
IF xv <> 9 THEN PRINT ",,,,,,,,"; : COLOR 15: PRINT "."; : COLOR 8: PRINT ",,," ELSE PRINT ",,,"
COLOR 10, 4
vx = 3
xv = 0
vx2 = 8: k$ = ""
hop$ = LEFT$(hop$, LEN(hop$) - 1)
'LOCATE 1, 1: PRINT hop$
LOCATE ln + 11, 10 + LEN(hop$)
stri$ = hop$
GOTO 140
END IF
IF k$ = CHR$(8) AND LEN(stri$) > 1 THEN stri$ = LEFT$(stri$, LEN(stri$) - 1): GOTO 140
IF k$ = CHR$(8) AND LEN(stri$) > 0 AND LEN(stri$) < 2 THEN stri$ = "": GOTO 140
IF k$ = CHR$(8) AND LEN(stri$) = 0 THEN PLAY "b60": GOTO 140
IF k$ = CHR$(9) THEN PLAY "f60": GOTO 140
IF k$ = "." THEN IF xv = 0 THEN k$ = "": xv = 9: vx2 = 3: hop$ = stri$ + ".": stri$ = "" ELSE PLAY "c60": GOTO 140
IF LEN(stri$) = vx2 THEN IF xv = 0 THEN xv = 9: vx2 = 3: hop$ = stri$ + ".": stri$ = "" ELSE PLAY "c60": GOTO 140
IF stri$ = "???????" AND k$ = "?" THEN PLAY "e60": GOTO 140
stri$ = stri$ + k$
COLOR 8
LOCATE ln + 11, 9 + xv, 1
IF xv <> 9 THEN PRINT ",,,,,,,,"; : COLOR 15: PRINT "."; : COLOR 8: PRINT ",,," ELSE PRINT ",,,"
COLOR 10
LOCATE ln + 11, 9 + xv, 1
PRINT stri$;
LOOP
stri$ = ""
LOCATE 1, 1
FOR tt = 0 TO 11
colort = SCREEN(ln + 11, 8 + tt, 1)
wh$ = CHR$(SCREEN(ln + 11, 9 + tt))
IF wh$ <> "," THEN stri$ = stri$ + LTRIM$(RTRIM$(wh$))
NEXT
LOCATE , 0
COLOR 15, 1
ext = 0
FOR z = 1 TO 20
IF stri$ = cas$(z) THEN ext = 1
NEXT
LOCATE , , 0
END IF
LOCATE ln + 11, 7: COLOR 15, 4:  COLOR 15, 1
IF ln = 1 THEN GOSUB chn
IF ln = 2 THEN GOSUB add1
IF ln = 3 THEN GOSUB del1
IF ln = 4 THEN GOSUB stor1
IF ln = 5 THEN GOTO endg
ghos:
GOSUB opti
endoo2:
RETURN

opti:
LOCATE 12, 2, 0
COLOR 15, 1
PRINT " 1  Change Name    ": LOCATE , 2
PRINT " 2  Add new file    ": LOCATE , 2
PRINT " 3  Delete a file   ": LOCATE , 2
PRINT " 4  Restore Defult  ": LOCATE , 2
PRINT " 5  Exit     ": LOCATE , 2

OPEN "cleanup4.dat" FOR APPEND AS #1
CLOSE #1
OPEN "cleanup4.dat" FOR INPUT AS #1
z = 4
FOR gx = 1 TO 4
IF EOF(1) THEN EXIT FOR
INPUT #1, c$
NEXT
DO WHILE NOT EOF(1)
z = z + 1
ON ERROR RESUME NEXT
IF z >= 5 THEN
INPUT #1, a$
IF a$ = "==>" THEN kf$ = "1": EXIT DO
IF EOF(1) THEN GOTO endg
END IF
LOOP
zz = 0
hj = 0
DO WHILE NOT EOF(1)
IF kf$ = "" THEN INPUT #1, cus$ ELSE cus$ = "==>"
IF cus$ = "==>" THEN
IF EOF(1) THEN EXIT DO
INPUT #1, name$
IF EOF(1) THEN EXIT DO
DO WHILE NOT EOF(1)
zz = zz + 1
IF zz > 20 THEN hj = 1: EXIT DO
INPUT #1, cass$
cass$ = LTRIM$(RTRIM$(UCASE$(cass$)))
     IF LEFT$(cass$, 2) <> "*." THEN
     cas$(zz) = cass$: ww$(zz) = cass$
    END IF
LOOP
END IF
IF EOF(1) OR hj = 1 THEN EXIT DO
LOOP
CLOSE
FOR cc = zz + 1 TO 20
cas$(cc) = ""
 ww$(cc) = ""
NEXT
FOR ox = 1 TO 6
IF ox = 1 THEN COLOR 11, 0 ELSE COLOR 15, 1
LOCATE 5 + ox, 25: PRINT SPACE$(52)
NEXT
COLOR 11, 0
LOCATE 6, 25
name$ = LTRIM$(RTRIM$(name$))
nam$ = name$
IF LEN(nam$) > 52 - 26 THEN nam$ = LEFT$(nam$, 52 - 28) + "..."
NN$ = nam$ + " (Maximume : 20 Files) : "
IF LEN(NN$) < 52 THEN
FOR zxc = 1 TO 53 - LEN(NN$)
NN$ = NN$ + " "
NEXT
END IF


PRINT NN$
COLOR 14, 1
LOCATE 7, 25
FOR zz = 1 TO 20
PRINT cas$(zz);
FOR gg = 1 TO 12 - LEN(cas$(zz))
PRINT " ";
NEXT
PRINT " ";
IF zz MOD 4 = 0 THEN PRINT : LOCATE , 25
NEXT

RETURN
add1:
IF ext = 1 THEN
COLOR 12, 4
LOCATE ln + 11, 6, 1: PRINT "Exist!!!     "; : PLAY "g20": ad$ = INPUT$(1): LOCATE , , 0
GOTO endf1
END IF                         'Enter Group:..."
FOR zxc = 1 TO 20
cas$(zxc) = ""
NEXT
OPEN "cleanup4.dat" FOR INPUT AS #14
OPEN "what.led" FOR OUTPUT AS #15
DO WHILE NOT EOF(14)
INPUT #14, ne$
IF ne$ = "==>" THEN EXIT DO
PRINT #15, ne$
LOOP
IF ne$ = "==>" THEN
IF EOF(14) THEN GOTO noth
INPUT #14, name$
noth:
PRINT #15, "==>"
PRINT #15, name$
PRINT #15, stri$
DO WHILE NOT EOF(14)
INPUT #14, ne$
PRINT #15, ne$
LOOP
END IF
CLOSE #14, #15
KILL "cleanup4.dat"
NAME "what.led" AS "cleanup4.dat"
endf1:
RETURN

del1:
IF ext = 0 THEN
COLOR 12, 4
LOCATE ln + 11, 6, 1: PRINT "Not Exist!!!   "; : PLAY "g20": ad$ = INPUT$(1): LOCATE , , 0
GOTO endf2
END IF                         'Enter Group:..."
FOR zxc = 1 TO 20
cas$(zxc) = ""
NEXT
OPEN "cleanup4.dat" FOR INPUT AS #14
OPEN "what.led" FOR OUTPUT AS #15
DO WHILE NOT EOF(14)
INPUT #14, ne$
IF ne$ = "==>" THEN EXIT DO
PRINT #15, ne$
LOOP
IF ne$ = "==>" THEN
IF EOF(14) THEN GOTO noth2
INPUT #14, name$
noth2:
PRINT #15, "==>"
PRINT #15, name$
DO WHILE NOT EOF(14)
INPUT #14, ne$
ne$ = UCASE$(LTRIM$(RTRIM$(ne$)))
IF ne$ <> stri$ THEN PRINT #15, ne$
LOOP
END IF
CLOSE #14, #15
KILL "cleanup4.dat"
NAME "what.led" AS "cleanup4.dat"
endf2:
RETURN

stor1:
IF sure(11, 1) = 0 THEN GOTO endf3
FOR zxc = 1 TO 20
cas$(zxc) = ""
NEXT
OPEN "cleanup4.dat" FOR INPUT AS #14
OPEN "what.led" FOR OUTPUT AS #15
DO WHILE NOT EOF(14)
INPUT #14, ne$
IF ne$ = "==>" THEN EXIT DO
PRINT #15, ne$
LOOP
IF ne$ = "==>" THEN
PRINT #15, "==>"
PRINT #15, "ASP soft's Selected files 1"
PRINT #15, "$$RAR$$.*"
END IF
CLOSE #14, #15
KILL "cleanup4.dat"
NAME "what.led" AS "cleanup4.dat"
endf3:
RETURN

chn:
newn$ = inp$(name$)
IF newn$ = "," THEN GOTO endf4
IF newn$ = name$ THEN
COLOR 12, 4
LOCATE ln + 11, 6, 1: PRINT "Last Name was!!"; : PLAY "g20": ad$ = INPUT$(1): LOCATE , , 0
GOTO endf4
END IF                         'Enter Group:..."
name$ = newn$
OPEN "cleanup4.dat" FOR INPUT AS #14
OPEN "what.led" FOR OUTPUT AS #15
DO WHILE NOT EOF(14)
INPUT #14, ne$
IF ne$ = "==>" THEN EXIT DO
PRINT #15, ne$
LOOP
IF ne$ = "==>" THEN
IF EOF(14) THEN GOTO noth4
INPUT #14, name$
noth4:
PRINT #15, "==>"
PRINT #15, newn$
DO WHILE NOT EOF(14)
INPUT #14, ne$
ne$ = UCASE$(LTRIM$(RTRIM$(ne$)))
PRINT #15, ne$
LOOP
END IF
CLOSE #14, #15
KILL "cleanup4.dat"
NAME "what.led" AS "cleanup4.dat"
endf4:
RETURN

endg:
END SUB

SUB delay (x)
SOUND 0, (x / 3600)
END SUB

DEFINT A-Z
SUB draws (x, y, z, c)
CIRCLEs INT(x), INT(y), INT(z), INT(c), 2
END SUB

SUB drvinfo (drvs$)
LINE (0, 0)-(178, 400), 0, BF
cadr 0, 0, 178, 50, 0, 10
COLOR 11
LOCATE 2, 2: PRINT "Push Drive letter :";
LOCATE 2, 21: PRINT "_"
drive$ = INPUT$(1)
IF LEN(drive$) > 1 OR drive$ = CHR$(9) THEN BEEP: GOTO notu
IF drive$ = CHR$(27) THEN PLAY "<g60>": GOTO notu
LOCATE 2, 21: PRINT drive$
drive$ = UCASE$(drive$)
IF INSTR(drvs$, drive$) = 0 THEN BEEP: GOTO notu
SCREEN 0
SHELL "support Setcolordrv " + " " + drive$
SCREEN 12
notu:
END SUB

DEFSNG A-Z
SUB emp (e)
ON ERROR RESUME NEXT
B$ = LTRIM$(RTRIM$(STR$(e)))
OPEN "cleanup4.dat" FOR INPUT AS #4
OPEN "what.led" FOR OUTPUT AS #5
zx = 0
DO WHILE NOT EOF(4)
zx = zx + 1
INPUT #4, a$
IF zx <> 3 THEN PRINT #5, a$ ELSE PRINT #5, B$
LOOP
CLOSE #4, #5
KILL "cleanup4.dat"
NAME "what.led" AS "cleanup4.dat"
END SUB

SUB errl1
ON ERROR RESUME NEXT
CLS
COLOR 12, 0
ctr 8, "            "
LOCATE 1, 1
COLOR 12, 1
 ctr 9, " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ ": COLOR , 0: PRINT " ": COLOR 12, 1
ctr 10, " ³ Sorry! you Can't run this Program with this Hard disk ³ ": COLOR , 0: PRINT " ": COLOR 12, 1
ctr 11, " ³ Becuase CleanUp Can't find any Drive to Clean it run  ³ ": COLOR , 0: PRINT " ": COLOR 12, 1
ctr 12, " ³ CleanUp when you have a good fixed/phisycal Drive !!! ³ ": COLOR , 0: PRINT " ": COLOR 12, 1
ctr 13, " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ ": COLOR , 0: PRINT " ": COLOR 12, 1
 COLOR 12, 0
 ctr 16, "              "
  oops = 0
  COLOR 8, 0: LOCATE 10, 8 + oops
  PRINT " Û": LOCATE , 8 + oops
  PRINT " Û": LOCATE , 8 + oops
  PRINT " Û": LOCATE , 8 + oops
  PRINT " Û": LOCATE , 8 + oops
  PRINT " ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß"
LOCATE , , 1
COLOR 10, 1
ctr 13, " Press any key to Exit..."
PLAY "mf d05g05 mf"
asz$ = INPUT$(1)
COLOR 12, 1
ctr 13, " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ ": COLOR , 0: PRINT " ": COLOR 12, 1
COLOR 7, 0
LOCATE 23, 1, 0
END

END SUB

SUB help
SCREEN 0
SHELL "SUPPORT Textbackgroundhelpÿ"
SCREEN 12
END SUB

FUNCTION inp$ (t2$)
t$ = t2$
LOCATE 12, 2
COLOR 15, 1
PRINT " 1  "; : COLOR , 6: LOCATE , 6: PRINT "Enter new name"; CHR$(24)
LOCATE 6, 25
COLOR 8, 4
FOR tt = 1 TO 52
PRINT "ú";
NEXT
COLOR 13, 4
s = 6: d = 25
LOCATE s, d, 1
COLOR 13, 4
PRINT t$;
COLOR 13, 4
LOCATE s, d
COLOR 8, 4
FOR tt = 1 TO 52
PRINT "ú";
NEXT
COLOR 13, 4
LOCATE s, d
PRINT t$;
DO
rese:
IF a$ <> "" THEN
LOCATE s, d, 1
COLOR 8, 4
FOR tt = 1 TO 52
PRINT "ú";
NEXT
COLOR 13, 4
LOCATE s, d, 1
COLOR 13, 4
'IF LEN(t$) > 1 THEN t$ = UCASE$(RIGHT$(t$, 1)) + MID$(t$, 2)
'IF LEN(t$) = 1 THEN t$ = UCASE$(t$)
PRINT t$;
END IF

a$ = INKEY$
IF a$ = "" THEN GOTO rese
IF LEN(a$) > 1 THEN PLAY "f60": GOTO rese
'IF LEN(t$) > 1 THEN t$ = UCASE$(RIGHT$(t$, 1)) + MID$(t$, 2)
'IF LEN(t$) = 1 THEN t$ = UCASE$(t$)
IF a$ = CHR$(13) THEN EXIT DO
IF a$ = "," THEN PLAY "d60": GOTO rese
IF a$ = CHR$(9) THEN PLAY "b60": GOTO rese
IF a$ = CHR$(27) THEN t$ = ",": EXIT DO
IF a$ = CHR$(8) AND LEN(t$) > 0 THEN t$ = LEFT$(t$, LEN(t$) - 1): GOTO rese
IF a$ = CHR$(8) AND LEN(t$) < 1 THEN t$ = "": GOTO rese
IF a$ <> CHR$(13) AND a$ <> CHR$(27) AND a$ <> CHR$(8) THEN
IF LEN(t$) < 52 THEN t$ = t$ + a$ ELSE PLAY "c60"
END IF
IF a$ <> "" THEN
LOCATE s, d, 1
COLOR 8, 4
FOR tt = 1 TO 52
PRINT "ú";
NEXT
COLOR 13, 4
LOCATE s, d, 1
COLOR 13, 4
'IF LEN(t$) > 1 THEN t$ = UCASE$(RIGHT$(t$, 1)) + MID$(t$, 2)
'IF LEN(t$) = 1 THEN t$ = UCASE$(t$)
PRINT t$;
END IF
LOOP
showu:
60
PRINT
COLOR 11, 0
LOCATE s, d, 0
PRINT SPACE$(52)
COLOR 15, 1
inp$ = t$
'PRINT inp$
END FUNCTION

FUNCTION keypressed
IF INKEY$ <> "" THEN keypressed = 1
END FUNCTION

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

DEFINT A-Z
SUB offs
FOR t = 1 TO 15
FOR g = 0 TO 63 STEP 5
gg = INT(63 - g)
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

DEFSNG A-Z
FUNCTION ogay
ON ERROR RESUME NEXT
x = 23
y = 1
LOCATE 24, 2
COLOR 11, 1
x = 2
y = 24
 PRINT "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ» ": LOCATE , x
 PRINT "ºTerminate Cleaning?º": LOCATE , x
 PRINT "ÇÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄ¶ ": LOCATE , x
 PRINT "º   Yes!  ³   No    º ": LOCATE , x
 PRINT "ÈÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍ¼ ": LOCATE , x
kl = -1
DO
a$ = INKEY$
IF a$ <> "" THEN
 COLOR 11, 1
 LOCATE 27, 2
 PRINT "º   Yes!  ³    No   º ": LOCATE , x
END IF
COLOR 14, 4
IF a$ = CHR$(27) THEN ogay = 0: GOTO endu2
IF a$ = CHR$(13) THEN IF kl = 1 THEN ogay = 1: GOTO endu2 ELSE ogay = 0: GOTO endu2
IF a$ = CHR$(0) + CHR$(77) THEN kl = -kl
IF a$ = CHR$(0) + CHR$(75) THEN kl = -kl
 IF kl = 1 THEN LOCATE y + 3, x + 1: PRINT "   Yes!  "
IF kl = -1 THEN LOCATE y + 3, x + 11: PRINT "    No   "
LOOP
'ÑÏÇ¶Ä
endu2:
LINE (10, 370)-(200, 470), 0, BF

END FUNCTION

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

SUB Options
ON ERROR RESUME NEXT
DIM cas$(20)
DIM ww$(20)
DIM f$(38)
qq = 1
COLOR 15, 1
CLS
GOSUB zog
ln = 1
onn 15, 7
cadrs2
DO
LOCATE 1, 1
LOCATE ln + 1, 2
COLOR 10, 4: PRINT " "; str2$(ln);
'LOCATE ln + 1, 4
'COLOR 15: PRINT "þ";
LOCATE ln + 1, 6
COLOR 14, 4
IF ln = 1 THEN PRINT "Define Groups  "
IF ln = 2 THEN PRINT "Define other   "
IF ln = 3 THEN PRINT "Cleaner      "
IF ln = 4 THEN
COLOR 14
PRINT "Beep       "
END IF
IF ln = 5 THEN
COLOR 13
LOCATE ln + 1, 5
IF ept = 1 THEN PRINT "û";  ELSE COLOR 13: PRINT "X";
COLOR 14
PRINT "Empty Directory"
END IF
IF ln = 6 THEN PRINT "Summurry      "
IF ln = 7 THEN PRINT "Restore All    "
IF ln = 8 THEN PRINT "Exit      "
COLOR 15, 1
k$ = INKEY$
IF k$ = CHR$(13) THEN GOSUB selt: GOSUB zog: cadrs2
IF k$ = CHR$(27) THEN EXIT DO
IF k$ = CHR$(0) + CHR$(72) THEN ln = ln - 1
IF k$ = CHR$(0) + CHR$(80) THEN ln = ln + 1
IF ln > 8 THEN ln = 1
IF ln < 1 THEN ln = 8
IF k$ <> "" THEN GOSUB zog
LOOP
GOTO endf
fard:
OPEN "cleanup4.dat" FOR APPEND AS #1
CLOSE #1
OPEN "cleanup4.dat" FOR INPUT AS #1
z = 0
DO WHILE NOT EOF(1)
z = z + 1
ON ERROR RESUME NEXT
IF z = 1 THEN
INPUT #1, c$
c = VAL(c$)
IF c = 1 THEN c$ = " Normal   (Does not Change attribute) "
IF c = 2 THEN c$ = " Maximume (Change attribute of Deletable files) "
IF c = 3 THEN c$ = " Attriber (Change attribute of ALL files) "
END IF
IF z = 2 THEN
INPUT #1, BEP$
BEP = VAL(BEP$)
IF BEP = 1 THEN BEP$ = "Gorilla"
IF BEP = 2 THEN BEP$ = "Bethoven"
IF BEP = 3 THEN BEP$ = "Nibbles"
IF BEP = 4 THEN BEP$ = "ASP"
IF BEP = 0 THEN BEP$ = "Off"
END IF
IF z = 3 THEN
INPUT #1, ept$
ept = VAL(ept$)
IF ept = 1 THEN ept$ = "Yes"
IF ept = 0 THEN ept$ = "No"
END IF
IF z = 4 THEN
INPUT #1, sum$
sum = VAL(sum$)
IF sum = 0 THEN sum$ = "Never"
IF sum = 1 THEN sum$ = "Allways"
IF sum = 2 THEN sum$ = "When any file was Deleted"
END IF
IF z >= 5 THEN
INPUT #1, a$
a$ = LTRIM$(RTRIM$(UCASE$(a$)))
IF a$ = "==>" THEN kf$ = "1": EXIT DO:  ELSE kf$ = ""
IF LEFT$(a$, 2) = "*." THEN a$ = MID$(a$, 3)
IF LEN(a$) > 3 THEN a$ = LEFT$(a$, 3)
f$(z - 4) = a$
END IF
LOOP
zz = 0
hj = 0
DO WHILE NOT EOF(1)
IF kf$ = "" THEN INPUT #1, cus$ ELSE cus$ = "==>"

 IF cus$ = "==>" THEN

  IF EOF(1) THEN EXIT DO
   INPUT #1, name$
   IF EOF(1) THEN EXIT DO

    DO WHILE NOT EOF(1)
     zz = zz + 1
     IF zz > 20 THEN hj = 1: EXIT DO
     INPUT #1, cass$
     cass$ = LTRIM$(RTRIM$(UCASE$(cass$)))
     IF LEN(cass$) > 12 THEN cass$ = LEFT$(cass$, 12)
     IF LEFT$(cass$, 2) <> "*." THEN
     cas$(zz) = cass$: ww$(zz) = cass$
    END IF
    LOOP

  END IF

  IF EOF(1) OR hj = 1 THEN EXIT DO
 LOOP
FOR cc = z - 4 TO 38
f$(cc) = ""
NEXT
FOR zz = 1 TO 20
FOR xx = 1 TO 12 - LEN(ww$(zz))
ww$(zz) = ww$(zz) + " "
NEXT
NEXT

CLOSE
FOR cc = zz + 1 TO 20
cas$(cc) = ""
 ww$(cc) = ""
NEXT
RETURN
zog:
GOSUB fard
COLOR 15, 1
LOCATE 2, 2
PRINT " 1 -Define Groups  ": LOCATE , 2
PRINT " 2 -Define other   ": LOCATE , 2
PRINT " 3 -Cleaner   ": LOCATE , 2
PRINT " 4 -Beep    ": LOCATE , 2
PRINT " 5 -Empty Directory": LOCATE , 2
PRINT " 6 -Summurry   ": LOCATE , 2
PRINT " 7 -Restore All  ": LOCATE , 2
PRINT " 8 -Exit    ": LOCATE , 2
COLOR 12
'LOCATE 5, 5
'IF bep = 1 THEN PRINT "û"
LOCATE 6, 5
IF ept = 1 THEN PRINT "û" ELSE PRINT "X"
COLOR 14
LOCATE 3, 25
FOR z = 1 TO 13
o$ = f$(z)
IF LEN(o$) = 1 THEN o$ = o$ + "  "
IF LEN(o$) = 2 THEN o$ = o$ + " "
PRINT o$; " ";
NEXT
LOCATE 4, 25
FOR z = 14 TO 13 + 13
o$ = f$(z)
IF LEN(o$) = 1 THEN o$ = o$ + "  "
IF LEN(o$) = 2 THEN o$ = o$ + " "
PRINT o$; " ";
NEXT
LOCATE 5, 25
FOR z = 26 TO 38
o$ = f$(z)
IF LEN(o$) = 1 THEN o$ = o$ + "  "
IF LEN(o$) = 2 THEN o$ = o$ + " "
PRINT o$; " ";
NEXT
COLOR 11, 0
LOCATE 2, 25, 0
PRINT "Group of Files to Delete (Maximume : 38 Groups) :   "
LOCATE 6, 25
name$ = LTRIM$(RTRIM$(name$))
nam$ = name$
IF LEN(nam$) > 52 - 26 THEN nam$ = LEFT$(nam$, 52 - 28) + "..."
NN$ = nam$ + " (Maximume : 20 Files) : "
IF LEN(NN$) < 52 THEN
FOR zxc = 1 TO 53 - LEN(NN$)
NN$ = NN$ + " "
NEXT
END IF
PRINT NN$
LOCATE 7, 25
COLOR 14, 1
LOCATE 7, 25
FOR zz = 1 TO 20
PRINT ww$(zz); " ";
IF zz MOD 4 = 0 THEN PRINT : LOCATE , 25
NEXT
COLOR 11, 0
LOCATE 11 + qq, 25
PRINT "Cleaner :        "
COLOR 14, 1
LOCATE 12 + qq, 25: PRINT LTRIM$(c$)
COLOR 11, 0
LOCATE 13 + qq, 25
PRINT "Beep :         "
COLOR 14, 1
LOCATE 14 + qq, 25: PRINT BEP$
COLOR 11, 0
LOCATE 15 + qq, 25
PRINT "Clear empty Directories ? :      "
COLOR 14, 1
LOCATE 16 + qq, 25: PRINT ept$; " "
COLOR 11, 0
LOCATE 17 + qq, 25
PRINT "Display Summurry :       "
COLOR 14, 1
LOCATE 18 + qq, 25: PRINT sum$
COLOR , 1
'LINE (186, 303.5)-(624, 303.5), 10
RETURN
selt:
GOSUB zog
IF ln = 1 THEN defg
IF ln = 2 THEN defo
IF ln = 3 THEN cln (c)
IF ln = 4 THEN
aol:
bbp (BEP)
END IF
IF ln = 5 THEN
IF ept = 0 THEN ept = 1: GOTO aol2
IF ept = 1 THEN ept = 0
aol2:
emp (ept)
END IF
IF ln = 6 THEN summ (sum)
IF ln = 7 THEN res
IF ln = 8 THEN GOTO endf
CLS
FOR zxc = 1 TO 38
f$(zxc) = ""
NEXT
FOR zxc = 1 TO 20
ww$(zxc) = ""
cas$(zxc) = ""
NEXT
GOSUB zog
RETURN
endf:
offf 15, 7
END SUB

SUB refd
ON ERROR RESUME NEXT
LINE (10, 78)-(170, 130), 0, BF
COLOR 15
LOCATE 6, 3
COLOR 7: PRINT "A "; : COLOR 8: PRINT "B"; : COLOR 7: PRINT " C D E F G H I ": LOCATE , 3
COLOR 7
PRINT "J K L M N O P Q R ": LOCATE , 3
COLOR 7
PRINT "S T U V W X Y Z "; : COLOR 8: PRINT CHR$(8)
COLOR 11
LOCATE 2, 3
PRINT "Space = [Un] Select": LOCATE , 3
PRINT "Enter = Ok!": LOCATE , 3
PRINT "Esc = Cancle": LOCATE , 3

END SUB

SUB remenu
COLOR , 1: CLS
COLOR 11, 1
LOCATE 5, 1
'                    \  |  /
'      1            18 \|/
LOCATE 4, 1:
COLOR 11, 2
PRINT SPACE$(80)
ctr 1, "CleanUp's Work Station Ver.IV - 1.1 | ASP soft Co."
COLOR 11
LOCATE 3, 2
PRINT " Drive           ": LOCATE , 2
PRINT " Info.           ": LOCATE , 2
PRINT " Path           ": LOCATE , 2
PRINT " Deleting          ": LOCATE , 2
PRINT " Done          "; : COLOR 13: PRINT "úú %"; : COLOR 11: PRINT "      ": LOCATE , 2
PRINT " Directories          ": LOCATE , 2
PRINT " Elapsed Time  "; : COLOR 12: PRINT "00:00:00"; : COLOR 11: PRINT "     ": LOCATE , 2
PRINT " Begin Time    "; : COLOR 12: PRINT "00:00:00"; : COLOR 11: PRINT "     ": LOCATE , 2
PRINT " Time          "; : COLOR 12: PRINT "00:00:00"; : COLOR 11: PRINT "     ": LOCATE , 2
PRINT " Date          "; : COLOR 11: PRINT date2$; : COLOR 11: PRINT "       ": LOCATE , 2
PRINT " Ver.          "; : COLOR 14: PRINT "IV - 1.1"; : COLOR 11: PRINT "     ": LOCATE , 2
PRINT " Writer        "; : COLOR 10: PRINT "Amin Safinezhad (ASP soft Co.)"; : COLOR 11: PRINT "   ": LOCATE , 2
PRINT " Cleaner Mode          ": LOCATE , 2
PRINT " Remained time                      ": LOCATE , 2
COLOR 11, 1
cadr 0, 20, 640, 480, 0, 10
cadr 0, 310, 640, 360, 0, 10
draws 320, 400, 22, 10
LINE (120, 26)-(120, 310), 10
END SUB

SUB res
ON ERROR RESUME NEXT
IF sure(1, 1) = 1 THEN
OPEN "cleanup4.dat" FOR OUTPUT AS #12
PRINT #12, "1,1,1,1,*.SWP,*.TMP,*.$$$,*.BAK,*.NCD,*.OBJ,*.DSK,*.PSM,*.DPR,*.TC,==>,ASP soft's Selected files 1,$$RAR$$.*"
CLOSE
END IF
COLOR , 1
CLS
END SUB

SUB rtr (c, cc)
FOR i = c TO cc
setrgbpalette i, 0, 0, 0
NEXT
END SUB

SUB setrgbpalette (x, r, g, B)
PALETTE x, B * 65536 + g * 256 + r
END SUB

FUNCTION str2$ (x)
str2$ = LTRIM$(RTRIM$(STR$(x)))
END FUNCTION

SUB summ (s)
ON ERROR RESUME NEXT
GOSUB som
ln = 1
ss = s
cadr 0, 160, 179, 250, 0, 10
DO
a$ = INKEY$
IF a$ <> "" THEN GOSUB som
IF a$ = CHR$(0) + CHR$(72) THEN ln = ln - 1
IF a$ = CHR$(0) + CHR$(80) THEN ln = ln + 1
IF a$ = CHR$(27) THEN GOTO endsub0
IF a$ = CHR$(13) THEN
IF ln < 4 THEN ss = ln - 1: ss$ = LTRIM$(RTRIM$(STR$(ss))): GOTO pressed ELSE GOTO endsub0
END IF
IF ln > 4 THEN ln = 1
IF ln < 1 THEN ln = 4
COLOR 10, 4
LOCATE ln + 11, 2
PRINT " " + LTRIM$(RTRIM$(STR$(ln))) + " ";
LOCATE ln + 11, 5
COLOR 12
IF ln = s + 1 THEN PRINT "û";  ELSE PRINT " ";
COLOR 14
IF ln = 1 THEN PRINT "Never      "
IF ln = 2 THEN PRINT "Allways      "
IF ln = 3 THEN PRINT "At delete files"
IF ln = 4 THEN PRINT "Cancle      "
LOOP
END
GOTO endsub0
som:
LOCATE 12, 2
COLOR 11, 1
PRINT " 1  Never     ": LOCATE , 2
PRINT " 2  Allways     ": LOCATE , 2
PRINT " 3  At delete files": LOCATE , 2
PRINT " 4  Cancle     ": LOCATE , 2
COLOR 12
LOCATE s + 12, 5
PRINT "û"
COLOR 11
RETURN
pressed:
OPEN "cleanup4.dat" FOR INPUT AS #4
OPEN "what.led" FOR OUTPUT AS #5
zx = 0
DO WHILE NOT EOF(4)
zx = zx + 1
INPUT #4, a$
IF zx <> 4 THEN PRINT #5, a$ ELSE PRINT #5, ss$
LOOP
CLOSE #4, #5
KILL "cleanup4.dat"
NAME "what.led" AS "cleanup4.dat"
GOTO endsub0
endsub0:
END SUB

FUNCTION sure (y, x)
ON ERROR RESUME NEXT
LOCATE 23, 1
COLOR 10, 1
 PRINT " ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ» "
 PRINT " º  Are you Sure ?!  º "
 PRINT " ÇÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄ¶ "
 PRINT " ºÿÿÿYes!ÿÿ³ÿÿÿÿNoÿÿÿº "
 PRINT " ÈÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍ¼ "
y = 23
x = 2
kl = 1
DO
a$ = INKEY$
IF a$ <> "" THEN
 COLOR 10, 1
 LOCATE 23 + 3, x
 PRINT "ºÿÿÿYes!ÿÿ³ÿÿÿÿNoÿÿÿºÿ": LOCATE , x
END IF
COLOR 14, 4
IF a$ = CHR$(27) THEN sure = 0: GOTO endu
IF a$ = CHR$(13) THEN IF kl = 1 THEN sure = 1: GOTO endu ELSE sure = 0: GOTO endu
IF a$ = CHR$(0) + CHR$(77) THEN kl = -kl
IF a$ = CHR$(0) + CHR$(75) THEN kl = -kl
 IF kl = 1 THEN LOCATE 23 + 3, x + 1: PRINT "ÿÿÿYes!ÿ"
IF kl = -1 THEN LOCATE y + 3, x + 11: PRINT "ÿÿÿÿNoÿÿÿ"
LOOP
'ÑÏÇ¶Ä
endu:
LOCATE 23, 1
COLOR 10, 1
 PRINT "         "
 PRINT "         "
 PRINT "         "
 PRINT "         "
 PRINT "         "

END FUNCTION

FUNCTION time2$
hour = VAL(TIME$)
IF hour > 12 THEN hour = hour - 12: apm$ = "PM" ELSE apm$ = "AM"
IF hour = 12 THEN apm$ = "PM"
tgt = INSTR(TIME$, ":")
time2$ = LTRIM$(RTRIM$(STR$(hour))) + MID$(TIME$, tgt) + " " + apm$
END FUNCTION

SUB TP (s$)
FOR t = 1 TO LEN(s$)
ss$ = MID$(s$, t, 1)
IF ss$ <> " " THEN SOUND 0, .1
PRINT ss$;
NEXT
PRINT
END SUB

