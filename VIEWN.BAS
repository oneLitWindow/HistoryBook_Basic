DECLARE SUB showd (h AS LONG, x!)
DECLARE FUNCTION str3$ (x AS LONG)
DECLARE FUNCTION inpt$ ()
DECLARE SUB smprint (tx$)
DECLARE FUNCTION time2$ ()
DECLARE FUNCTION date2$ ()
DECLARE FUNCTION str2$ (x!)
DECLARE SUB getkeys ()
DECLARE SUB chrerr ()
DECLARE FUNCTION errormes$ ()
DECLARE SUB warp (maxlin!, asd!, v$, vb$, kopp!)
DECLARE SUB explore (maxlin!, asd!, v$, kopp!)
DECLARE SUB helpex (x!)
DECLARE SUB helpu ()
DECLARE SUB ctr (x$)
DECLARE SUB helpwarp (x!)
DECLARE SUB ctr2 (t$)
DECLARE SUB help ()
DECLARE SUB errors ()
DECLARE SUB delay (x!)
vx$ = COMMAND$
IF INSTR(vx$, "/") <> 0 THEN
v$ = LEFT$(vx$, INSTR(vx$, "/") - 1)
ELSE v$ = vx$
END IF
v$ = LTRIM$(RTRIM$(v$))
maxlin = 25
COLOR 10, 0
PRINT : PRINT
COLOR 7, 0
CLS
comm$ = LTRIM$(RTRIM$(COMMAND$))
fcc = 10
COLOR fcc, 1
LOCATE 9, 1
ctr "ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป": COLOR 8, 0: PRINT "Ü": COLOR fcc, 9
ctr "บ File Viewer 3Rd Edition By Amin Safinejad - Release 2: 2002 บ": COLOR 8, 0: PRINT "Û": COLOR fcc, 9
ctr "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ": COLOR 8, 0: PRINT "Û"
ctr " ฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿"
COLOR 7, 0
IF INSTR(comm$, "/?") <> 0 THEN helpu: getkeys: END
IF comm$ = "" OR v$ = "" THEN
FOR t = 1 TO 2
PLAY "mf <<a50"
delay .5
PLAY "a50>>"
delay 2
NEXT
comm$ = "/?": helpu: getkeys: END
END IF
COLOR 14, 4
ctr2 "þ F4 to abort"
'delay 5
COLOR 7, 0
tnt = 0
IF LEN(v$) > 15 THEN
vop$ = "..." + RIGHT$(v$, 12)
ELSE
vop$ = v$
END IF
COLOR 9
ctr2 v$
COLOR 11
ctr "þFile to View: ณ" + vop$ + "ณ Checking, Scanning Lines: ณ"
x = POS(0)
ON ERROR RESUME NEXT
LOCATE , x - 2
COLOR 15
'LOCATE 14, 64
'delay 40
ON ERROR GOTO notf
OPEN v$ FOR INPUT AS #1
CLOSE
ON ERROR GOTO report
asd = 0
comm$ = COMMAND$
wamper = 0
exper = 0
IF INSTR(comm$, "/W") <> 0 THEN h = 1
vir$ = v$
'TYPE ss
'sr AS STRING * 80
'END TYPE
lokagom = 1
DIM now$(11)
now$(1) = CHR$(7)
now$(2) = CHR$(9)
now$(3) = CHR$(10)
now$(4) = CHR$(11)
now$(5) = CHR$(12)
now$(6) = CHR$(13)
now$(7) = CHR$(29)
now$(8) = CHR$(30)
now$(9) = CHR$(31)
now$(10) = CHR$(255)
now$(11) = CHR$(26)

honololo:
ON KEY(4) GOSUB merch
KEY(4) ON
IF h = 1 THEN
IF wamper = 0 THEN
wamper = 1
vb$ = v$
OPEN "read.tsp" FOR OUTPUT AS #3

PRINT #3, STRING$(81, "ÿ")
OPEN v$ FOR INPUT AS #2
xxc = POS(0)
yyc = CSRLIN
COLOR 10
IF INSTR(COMMAND$, "/N") <> 0 THEN ilu = 1 ELSE ilu = 0
yy4:
DO WHILE NOT EOF(2)
IF INKEY$ = CHR$(0) + CHR$(62) THEN GOSUB merch
kt = kt + 1'INT(TIMER * 10) MOD 4
IF asd MOD 1000 = 0 THEN
LOCATE yyc, xxc: PRINT " " + LTRIM$(RTRIM$(STR$(asd))); : COLOR 10 + 16: PRINT "..."; : COLOR 10: PRINT " "
END IF
LINE INPUT #2, acc$
FOR mypi = 1 TO 11
101010
'IF INSTR(acc$, "ÿ") <> 0 THEN
'END
'acc$ = LEFT$(acc$, INSTR(acc$, "ÿ") - 1) + " " + MID$(acc$, INSTR(acc$, "ÿ") + 1)
'GOTO 101010
'END IF
IF INSTR(acc$, now$(mypi)) <> 0 THEN
acc$ = LEFT$(acc$, INSTR(acc$, now$(mypi)) - 1) + " " + MID$(acc$, INSTR(acc$, now$(mypi)) + LEN(now$(mypi)))
GOTO 101010
END IF
'WIDTH 80, 25: chrerr
NEXT
nui:
asd = asd + 1
cas = 0

IF LEN(acc$) < 80 THEN
acc$ = acc$ + STRING$(80 - LEN(acc$), "ÿ")
END IF

IF LEN(acc$) > 80 THEN
cas = 1
acc2$ = MID$(acc$, 80)
IF ilu = 1 THEN acc$ = LEFT$(acc$, 80) + CHR$(2)
IF ilu = 0 THEN acc$ = LEFT$(acc$, 79) + "ÿ" + CHR$(3)
IF ilu = 1 THEN cas = 2
END IF
IF cas = 0 THEN acc$ = acc$ + "ÿ"
PRINT #3, acc$
'IF LEN(acc$) <> 81 THEN PRINT acc$; LEN(acc$): END
'IF cas = 1 THEN
'PRINT #3, CHR$(3)
'ELSE
'PRINT #3, "ÿ"
'END IF
IF cas = 1 THEN acc$ = acc2$: GOTO nui
LOOP
asd = asd + 1
CLOSE #2
CLOSE #3
asdw = asd
END IF
asd = asdw
v$ = "read.tsp"
'delay 5
LOCATE , , 0
COLOR , 1
ELSE
v$ = vir$
ON KEY(4) GOSUB merch
KEY(4) ON
IF exper = 0 THEN
asd = 0
OPEN v$ FOR INPUT AS #2
xxc = POS(0)
yyc = CSRLIN
DO WHILE NOT EOF(2)
IF INKEY$ = CHR$(0) + CHR$(62) THEN GOSUB merch
IF asd MOD 100 = 0 THEN
LOCATE yyc, xxc: PRINT " " + LTRIM$(RTRIM$(STR$(asd))); : COLOR 15 + 16: PRINT "..."; : COLOR 15: PRINT "  ";
END IF
LINE INPUT #2, acc$
'FOR mypi = 1 TO 10
'olips:
'IF INSTR(acc$, "ÿ") <> 0 THEN
'acc$ = LEFT$(acc$, INSTR(acc$, "ÿ") - 1) + " " + MID$(acc$, INSTR(acc$, "ÿ") + 1)
'GOTO olips
'END IF
'IF INSTR(acc$, now$(mypi)) <> 0 THEN
'acc$ = LEFT$(acc$, INSTR(acc$, now$(mypi)) - 1) + " " + MID$(acc$, INSTR(acc$, now$(mypi)) + LEN(now$(mypi)))
'GOTO olips
'END IF
'NEXT

asd = asd + 1
LOOP
CLOSE #2
asde = asd
exper = 1
END IF
asd = asde
END IF
KEY(4) STOP
lokagom = 0
manol:
IF INSTR(comm$, "/S") <> 0 THEN
WIDTH 80, 50: maxlin = 50
END IF

IF konchala = 0 THEN SCREEN 12: SCREEN 0
konchala = 1
IF mp = 25 AND maxlin = 50 THEN WIDTH 80, 50
VIEW PRINT 1 TO maxlin
IF h = 1 THEN
p = 1: warp maxlin, (asdw), v$, vb$, 0
ELSE
explore maxlin, (asde + 1), v$, 0: p = 0
END IF
IF p = 1 THEN h = 0 ELSE IF p = 0 THEN h = 1
asd = 0
IF wamper = 1 AND exper = 1 THEN GOTO foto
WIDTH 80, 25
COLOR 9, 0
CLS
mp = 25
              PRINT "ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ"
IF h = 1 THEN PRINT "ณ Exploring --> Wraping   ณ"
IF h = 0 THEN PRINT "ณ Wraping   --> Exploring ณ"
              PRINT "ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู"
COLOR 14, 4
PRINT "þ Press F4 to abort... ";
COLOR 15, 1
PRINT " File : "; vop$
COLOR 11, 0
PRINT "þ Changing Mode...Scanning Lines: |";
LOCATE 5, 34
'delay 50
SLEEP 1
foto:
getkeys
GOTO honololo
ON ERROR RESUME NEXT
COLOR 7, 0: CLS : WIDTH 80, 25
KILL "read.tsp"
getkeys
END
notf:
errors
CLEAR
ON ERROR RESUME NEXT
COLOR 7, 0: CLS : WIDTH 80, 25
KILL "read.tsp"
getkeys
END
report:
spin = 0
IF ERR = 6 OR ERR = 7 OR ERR = 64 THEN COLOR 7, 0: WIDTH 80, 25: CLS : COLOR 12: PRINT "Memory Error!": BEEP: SYSTEM
SELECT CASE ERR
CASE 50 TO 54, 57 TO 59, 67, 69: spin = 1
CASE 71, 72, 75, 76: spin = 2
END SELECT
IF spin = 1 THEN COLOR 7, 0: WIDTH 80, 25: CLS : COLOR 12: PRINT "I/O file Error!": BEEP: SYSTEM
IF spin = 2 THEN COLOR 7, 0: WIDTH 80, 25: CLS : COLOR 12: PRINT "I/O disk Error!": BEEP: SYSTEM
ss$ = errormes$
SCREEN 12: SCREEN 0
COLOR 12
err$ = LTRIM$(RTRIM$(STR$(ERR)))
PRINT "Error! 'no.";
PRINT ss$;
PRINT "' Please Report this..."
RESUME repr
ON ERROR RESUME NEXT
KILL "read.tsp"
getkeys
END
repr:
'delay (20)
ON ERROR RESUME NEXT
PLAY "<<<g60..b60..c60..>>>"
PRINT
PLAY "<<<a60..f60..d60>>>"
OPEN "report.err" FOR APPEND AS #4
PRINT #4, "Error no. "; ss$
CLOSE #4
delay (100)
DO WHILE INKEY$ <> ""
LOOP
CLEAR
ON ERROR RESUME NEXT
KILL "read.tsp"
getkeys
END
merch:
'IF lokagom = 1 THEN WIDTH 80, 25: COLOR 7, 0: CLS : getkeys:end
'IF lokagom = 0 THEN GOTO manol'WIDTH 80, 25: COLOR 7, 0: CLS : getkeys:end
WIDTH 80, 25: COLOR 7, 0: CLS
'CLEAR
ON ERROR RESUME NEXT
KILL "read.tsp"
getkeys
END
RETURN

SUB chrerr
COLOR 12
PRINT
ctr "Not A Real Text File!!!"
BEEP
delay (50)
COLOR 7, 0: CLS
DO WHILE INKEY$ <> ""
LOOP
getkeys
END
END SUB

SUB ctr (t$)
LOCATE , (80 - LEN(t$)) \ 2
PRINT t$;
END SUB

SUB ctr2 (t$)
LOCATE , (80 - LEN(t$)) \ 2
PRINT t$
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
getkeys
NEXT
END SUB

FUNCTION errormes$
 IF ERR = 1 THEN errmes$ = "1 : NEXT without FOR"
 IF ERR = 2 THEN errmes$ = "2 : Syntax error"
 IF ERR = 3 THEN errmes$ = "3 : RETURN without GOSUB"
 IF ERR = 4 THEN errmes$ = "4 : Out of DATA"
 IF ERR = 5 THEN errmes$ = "5 : Illegal function call"
 IF ERR = 6 THEN errmes$ = "6 : Overflow"
 IF ERR = 7 THEN errmes$ = "7 : Out of memory"
 IF ERR = 8 THEN errmes$ = "8 : Label not defined"
 IF ERR = 9 THEN errmes$ = "9 : Subscript out of range"
IF ERR = 10 THEN errmes$ = "10: Duplicate definition"
IF ERR = 11 THEN errmes$ = "11: Division by zero"
IF ERR = 12 THEN errmes$ = "12: Illegal in direct mode"
IF ERR = 13 THEN errmes$ = "13: Type mismatch"
IF ERR = 14 THEN errmes$ = "14: Out of string space"
IF ERR = 16 THEN errmes$ = "16: String formula too complex"
IF ERR = 17 THEN errmes$ = "17: Cannot continue"
IF ERR = 18 THEN errmes$ = "18: Function not defined"
IF ERR = 19 THEN errmes$ = "19: No RESUME"
IF ERR = 20 THEN errmes$ = "20: RESUME without error"
IF ERR = 24 THEN errmes$ = "24: Device timeout"
IF ERR = 25 THEN errmes$ = "25: Device fault"
IF ERR = 26 THEN errmes$ = "26: FOR without NEXT"
IF ERR = 27 THEN errmes$ = "27: Out of paper"
IF ERR = 29 THEN errmes$ = "29: WHILE without WEND"
IF ERR = 30 THEN errmes$ = "30: WEND without WHILE"
IF ERR = 33 THEN errmes$ = "33: Duplicate label"
IF ERR = 35 THEN errmes$ = "35: Subprogram not defined"
IF ERR = 37 THEN errmes$ = "37: Argument-count mismatch"
IF ERR = 38 THEN errmes$ = "38: Array not defined"
IF ERR = 40 THEN errmes$ = "40: Variable required"
IF ERR = 50 THEN errmes$ = "50: FIELD overflow"
IF ERR = 51 THEN errmes$ = "51: Internal error"
IF ERR = 52 THEN errmes$ = "52: Bad file name or number"
IF ERR = 53 THEN errmes$ = "53: File not found"
IF ERR = 54 THEN errmes$ = "54: Bad file mode"
IF ERR = 55 THEN errmes$ = "55: File already open"
IF ERR = 56 THEN errmes$ = "56: FIELD statement active"
IF ERR = 57 THEN errmes$ = "57: Device I/O error"
IF ERR = 58 THEN errmes$ = "58: File already exists"
IF ERR = 59 THEN errmes$ = "59: Bad record length"
IF ERR = 61 THEN errmes$ = "61: Disk full"
IF ERR = 62 THEN errmes$ = "62: Input past end of file"
IF ERR = 63 THEN errmes$ = "63: Bad record number"
IF ERR = 64 THEN errmes$ = "64: Bad file name"
IF ERR = 67 THEN errmes$ = "67: Too many files"
IF ERR = 68 THEN errmes$ = "68: Device unavailable"
IF ERR = 69 THEN errmes$ = "69: Communication-buffer overflow"
IF ERR = 70 THEN errmes$ = "70: Permission denied"
IF ERR = 71 THEN errmes$ = "71: Disk not ready"
IF ERR = 72 THEN errmes$ = "72: Disk-media error"
IF ERR = 73 THEN errmes$ = "73: Feature unavailable"
IF ERR = 74 THEN errmes$ = "74: Rename across disks"
IF ERR = 75 THEN errmes$ = "75: Path/File access error"
IF ERR = 76 THEN errmes$ = "76: Path not found"
errormes$ = errmes$
END FUNCTION

SUB errors
COLOR 12
PRINT
ctr "Error! File not found!"
FOR t = 1 TO 2
PLAY "mf <<a50"
delay .5
PLAY "a50>>"
delay 2
NEXT
delay 50
DO WHILE INKEY$ <> ""
LOOP
PRINT : PRINT
END SUB

SUB explore (maxlin, asd, v$, kopp)
DIM honor AS LONG
sm = -1
'jk = o / 0
betch = 1
'delay 5
'COLOR 11: PRINT "Check done.";
'LOCATE , , 1
'COLOR 15: PRINT "  Wait...";
'delay 20
LOCATE , , 0
COLOR , 1
CLS
VIEW PRINT 1 TO maxlin
b = 1
melten:
organ = 0
COLOR , 7
LOCATE 1, 1
PRINT SPACE$(78)
LOCATE maxlin, 1
IF kgg$ <> "" THEN COLOR , 2
PRINT SPACE$(80);
COLOR , 1
DO
huc = 0
loki:
OPEN v$ FOR INPUT AS #1
FOR a = 1 TO b - 1
IF EOF(1) THEN EXIT FOR
LINE INPUT #1, temp$
a$ = INKEY$
IF a$ <> "" THEN tnt = tnt + 1
NEXT
t = 0
'FOR gg = 2 TO maxlin - 1
'LOCATE gg, 1: PRINT SPACE$(80)
'NEXT
COLOR , 1
'CLS
IF kgg$ = "" THEN
COLOR 1, 7
LOCATE maxlin, 17
PRINT "Tab,^";
PRINT CHR$(24); CHR$(25); CHR$(26); CHR$(27);
PRINT " ,^PgUp/Dn,Home,End,F1;2;7;10,Esc ";
COLOR 0
PRINT "ณ";
COLOR 11, 5
IF sm = 1 THEN PRINT "๛";  ELSE PRINT "X";
COLOR 0, 7: PRINT "ณ";
COLOR 15, 7
PRINT "By Amin Safinejad";
COLOR 0
IF LEN(v$) > 15 THEN vop$ = "..." + RIGHT$(v$, 12) ELSE vop$ = v$
vobvob2:
IF LEN(vop$) < 15 THEN vop$ = vop$ + " ": GOTO vobvob2
LOCATE maxlin, 1: PRINT vop$; "ณ";
END IF
COLOR 0, 7
LOCATE 1, 1: PRINT "Lines : "; b$; " / "; asd$; " ["; da$; "%] ณ ";
COLOR 15, 7
LOCATE 1, 48
'PRINT "Viewer By Amin Safinejad";
REDIM now$(11)
now$(1) = CHR$(7)
now$(2) = CHR$(9)
now$(3) = CHR$(10)
now$(4) = CHR$(11)
now$(5) = CHR$(12)
now$(6) = CHR$(13)
now$(7) = CHR$(29)
now$(8) = CHR$(30)
now$(9) = CHR$(31)
now$(10) = CHR$(255)
now$(11) = CHR$(26)

COLOR 15
LOCATE 1, 69
PRINT " Wrap Off ";
COLOR 15, 7
LOCATE 2, 1
COLOR 15, 1
LOCATE 2, 1
huco = huc
hj = 0
'IF b <> bo THEN huc = 0
FOR a = 1 TO maxlin '- 1
t = t + 1
IF t >= maxlin - 1 THEN EXIT FOR
IF EOF(1) THEN EXIT FOR
LINE INPUT #1, a$
FOR mypi = 1 TO 11
olips2:
'IF INSTR(a$, "ÿ") <> 0 THEN
'a$ = LEFT$(a$, INSTR(a$, "ÿ") - 1) + " " + MID$(a$, INSTR(a$, "ÿ") + 1)
'GOTO olips2
'END IF
IF INSTR(a$, now$(mypi)) <> 0 THEN
a$ = LEFT$(a$, INSTR(a$, now$(mypi)) - 1) + " " + MID$(a$, INSTR(a$, now$(mypi)) + LEN(now$(mypi)))
GOTO olips2
END IF
NEXT

IF huc - 79 < 1 THEN huc = 80
IF LEN(a$) > huc THEN huc = LEN(a$)
IF betch <= 0 THEN betch = 1
IF betch <= LEN(a$) THEN a$ = MID$(a$, betch) ELSE a$ = ""
IF LEN(a$) > 80 THEN a$ = LEFT$(a$, 80)
'LOCATE , 1
'PRINT SPACE$(80);
LOCATE , 1
IF LEN(a$) < 80 THEN a$ = a$ + STRING$(80 - LEN(a$), "ÿ")
IF sm = -1 THEN PRINT a$ ELSE smprint a$ + "|"
IF NOT t >= maxline - 3 THEN PRINT
NEXT
IF betch > huc - 79 AND kgg$ = "" THEN betch = huc - 79: CLOSE #1: GOTO loki
hj = hj + 1
'IF hj < 2 THEN GOTO loki
bo = b
IF tnt > 200 THEN
DO WHILE INKEY$ <> ""
LOOP
tnt = 0
END IF
yme = CSRLIN
FOR hme = yme TO maxlin - 1
LOCATE hme, 1: PRINT STRING$(80, "ÿ")
NEXT
DO
IF kgg$ = "" THEN honor = 0
IF kgg$ <> "" AND organ = 1 THEN CLOSE : GOTO melten
IF kgg$ <> "" THEN
IF INKEY$ = CHR$(27) THEN kgg$ = "": a$ = "": getkeys: CLOSE : showd honor, maxlin: CLOSE : GOTO melten: EXIT DO
COLOR 14, 4
LOCATE 1, 79
IF betch > 1 THEN COLOR 14, 4:  PRINT "ฎ" ELSE PRINT "๐"
LOCATE 1, 80
IF betch < huc - 79 THEN COLOR 14, 4: PRINT "ฏ" ELSE PRINT "๐"

COLOR 15, 2
LOCATE maxlin, 1
PRINT "Wait...[Esc=Abort] "; " Found: "; honor; "   ";
'a$ = CHR$(0) + CHR$(81)
'LOCATE 1, 1: PRINT huc - 79; betch
IF NOT betch < huc - 79 THEN
a$ = CHR$(0) + CHR$(81): betch = 1
ELSE
a$ = CHR$(0) + CHR$(116)
END IF
FOR ys = 2 TO maxlin - 1
FOR xs = betch TO 80
dd$ = ""
IF 80 - xs >= LEN(kgg$) THEN
FOR mom = 0 TO LEN(kgg$) - 1
dd$ = dd$ + CHR$(SCREEN(ys, xs + mom))
IF SCREEN(ys, xs, 1) = 158 THEN EXIT FOR
IF INSTR(dd$, "ÿ") <> 0 THEN EXIT FOR
IF LEFT$(kgg$, LEN(dd$)) <> UCASE$(dd$) THEN dd$ = "": EXIT FOR
NEXT
'IF INSTR(kgg$, UCASE$(dd$)) = 0 THEN EXIT FOR
IF dd$ <> "" AND INSTR(dd$, "ÿ") <> 0 THEN EXIT FOR
IF UCASE$(dd$) = kgg$ THEN
LOCATE ys, xs, 1: COLOR 14, 5: PRINT dd$;
artio:
IF kasan = 0 THEN
getkeys
LOCATE maxlin, 1
COLOR 15, 2
PRINT SPACE$(80);
LOCATE maxlin, 1
PRINT "Esc To Escape , Enter to Next , Tab to Don't Wait... Found : "; str3$(honor + 1);
ami$ = INPUT$(1)
IF ami$ = CHR$(9) THEN kasan = 1
getkeys
IF ami$ = CHR$(27) THEN a$ = "": kgg$ = "": CLOSE : showd honor, maxlin: GOTO melten: GOTO nopi
IF ami$ <> CHR$(13) AND kasan = 0 THEN GOTO artio
LOCATE maxlin, 1
COLOR 15, 2
PRINT SPACE$(80);
LOCATE maxlin, 1
PRINT "Wait...[Esc=Abort]";
LOCATE maxlin, 1
COLOR 15, 2
PRINT SPACE$(80);
END IF
honor = honor + 1
COLOR 14 + 16, 1: LOCATE ys, xs: PRINT dd$
END IF
END IF
IF INKEY$ = CHR$(27) THEN kgg$ = "": a$ = "": getkeys: CLOSE : showd honor, maxlin: GOTO melten: EXIT DO
NEXT
IF INKEY$ = CHR$(27) THEN kgg$ = "": a$ = "": getkeys: CLOSE : showd honor, maxlin: GOTO melten: EXIT DO
NEXT
nopi:
LOCATE , , 0
END IF
nofello:
IF kgg$ = "" THEN kasan = 0
110
COLOR 14, 4
LOCATE 1, 79, 0
IF betch > 1 THEN COLOR 14, 4:  PRINT "ฎ" ELSE PRINT "๐"
LOCATE 1, 80
IF betch < huc - 79 THEN COLOR 14, 4: PRINT "ฏ" ELSE PRINT "๐"
IF a$ <> "" THEN GOTO oool
a$ = INKEY$
oool:
COLOR 6, 7
betch$ = LTRIM$(RTRIM$(STR$(betch + 79)))
huc$ = LTRIM$(RTRIM$(STR$(huc)))
IF VAL(betch$) > huc AND kgg$ = "" THEN betch$ = huc$
haha$ = LTRIM$(RTRIM$(STR$(INT(((betch + 79) / (huc)) * 100))))
IF VAL(haha$) > 100 THEN haha$ = "100"
IF betch + 79 > huc AND kgg$ = "" THEN betch$ = huc$
b$ = LTRIM$(RTRIM$(STR$(b + maxlin - 3)))
asd$ = LTRIM$(RTRIM$(STR$(asd - 1)))
da = ((b + maxlin - 2) * 100) \ asd
IF b + maxlin - 2 > asd AND asd > maxlin - 2 AND kgg$ = "" THEN b = (asd - maxlin) + 3: a$ = CHR$(0) + CHR$(72)
IF da < 0 THEN a$ = CHR$(0) + CHR$(80)
IF da > 100 THEN da = 100
da$ = LTRIM$(RTRIM$(STR$(da)))

be10:
IF LEN(betch$) < 4 THEN betch$ = "0" + betch$: GOTO be10
hu10:
IF LEN(huc$) < 4 THEN huc$ = "0" + huc$: GOTO hu10
ha10:
IF LEN(haha$) < 3 THEN haha$ = "0" + haha$: GOTO ha10
betch$ = LEFT$(betch$, 1) + "," + MID$(betch$, 2)
huc$ = LEFT$(huc$, 1) + "," + MID$(huc$, 2)
b10:
IF LEN(b$) < 7 THEN b$ = "0" + b$: GOTO b10
asd10:
IF LEN(asd$) < 7 THEN asd$ = "0" + asd$: GOTO asd10
da10:
IF LEN(da$) < 3 THEN da$ = "0" + da$: GOTO da10
gj$ = ""
FOR wxp = 1 TO LEN(b$)
gj$ = gj$ + MID$(b$, wxp, 1)
IF wxp MOD 3 = 1 THEN gj$ = gj$ + ","
NEXT
b$ = gj$
gj$ = ""
FOR wxp = 1 TO LEN(asd$)
gj$ = gj$ + MID$(asd$, wxp, 1)
IF wxp MOD 3 = 1 THEN gj$ = gj$ + ","
NEXT
asd$ = gj$
IF RIGHT$(b$, 1) = "," THEN b$ = LEFT$(b$, LEN(b$) - 1)
IF RIGHT$(asd$, 1) = "," THEN asd$ = LEFT$(asd$, LEN(asd$) - 1)
COLOR 0, 7
IF asd <= maxlin - 2 THEN b2$ = asd$ ELSE b2$ = b$
LOCATE 1, 1: PRINT "Lines : "; b2$; " / "; asd$; " ["; da$; "%] ณ ";
PRINT "Rows : "; betch$; " / "; huc$; " ["; haha$; "%] ณ";
IF a$ = "" THEN GOTO 110
tnt = tnt + 1
COLOR , 1
y = 0
IF a$ = CHR$(27) THEN
WIDTH 80, 25: COLOR 7, 0: CLS
ON ERROR RESUME NEXT
KILL "read.tsp"
getkeys
END
END IF
IF a$ = CHR$(0) + CHR$(65) THEN
LOCATE 1, 1: COLOR , 2: PRINT SPACE$(80)
LOCATE 1, 1: COLOR 15, 2: PRINT ":";
COLOR 14
kgg$ = inpt$
IF kgg$ <> "" THEN sm = -1: LOCATE maxlin, 1: COLOR , 2: PRINT SPACE$(80); : organ = 1: honor = 0
y = 1
END IF

IF (b + maxlin - 2) >= asd AND kgg$ <> "" AND a$ = CHR$(0) + CHR$(81) THEN kgg$ = "": showd honor, maxlin: CLOSE : GOTO melten

IF a$ = CHR$(9) THEN sm = -sm: y = 1
IF a$ = CHR$(0) + CHR$(116) THEN IF betch < huc - 79 THEN betch = betch + 80: y = 1
IF a$ = CHR$(0) + CHR$(115) THEN IF betch > 1 THEN betch = betch - 80: y = 1
IF betch > huc - 79 AND kgg$ = "" THEN betch = huc - 79
IF betch < 1 THEN betch = 1
IF a$ = CHR$(0) + CHR$(77) THEN IF b >= 1 AND betch < huc - 79 THEN betch = betch + 1: y = 1
IF a$ = CHR$(0) + CHR$(75) THEN IF b >= 1 AND betch > 1 THEN betch = betch - 1: y = 1

IF a$ = CHR$(0) + CHR$(79) THEN IF betch < huc - 79 THEN betch = huc - 79: y = 1
IF a$ = CHR$(0) + CHR$(71) THEN IF betch > 1 THEN betch = 1: y = 1


IF a$ = CHR$(0) + CHR$(72) THEN IF b > 1 THEN b = b - 1: y = 1
IF a$ = CHR$(0) + CHR$(80) AND da >= 100 THEN a$ = ""
IF a$ = CHR$(0) + CHR$(80) AND NOT EOF(1) AND b < asd THEN b = b + 1: y = 1
IF a$ = CHR$(0) + CHR$(60) THEN
y = 1
IF maxlin = 25 THEN
maxlin = 50
ELSE
IF maxlin = 50 THEN maxlin = 25: IF da = 100 THEN b = asd - maxlin: y = 1
END IF
WIDTH 80, maxlin
VIEW PRINT 1 TO maxlin
'73,81
a$ = ""
CLOSE
IF asd <= maxlin - 2 THEN b = 1
GOTO melten
END IF
IF a$ = CHR$(0) + CHR$(59) THEN helpex (maxlin): y = 1
IF a$ = CHR$(0) + CHR$(132) THEN b = 1: y = 1
IF a$ = CHR$(0) + CHR$(118) THEN b = asd - maxlin + 2: y = 1
IF a$ = CHR$(0) + "I" THEN
IF b > maxlin - 2 AND kgg$ = "" THEN b = b - (maxlin - 2): y = 1 ELSE b = 1: y = 1
END IF
IF kgg$ <> "" AND a$ = CHR$(0) + CHR$(81) THEN
IF b$ = asd$ THEN kgg$ = "": y = 1: showd honor, maxlin
END IF
IF a$ = CHR$(0) + "Q" AND NOT EOF(1) THEN
IF b + 2 < asd - maxlin * 2 THEN
b = b + (maxlin - 2): y = 1
ELSE
IF kgg$ = "" THEN b = asd - maxlin + 2: y = 1 ELSE b = b + (maxlin - 2): y = 1
END IF
END IF
'IF a$ = CHR$(13) THEN WIDTH 80, 25: COLOR 7: CLS : getkeys:end
IF a$ = CHR$(0) + CHR$(68) THEN GOTO warpi
IF y = 1 THEN EXIT DO
a$ = ""
LOOP
CLOSE #1
LOOP
warpi:
CLOSE
END SUB

SUB getkeys
DO WHILE INKEY$ <> ""
LOOP
END SUB

SUB helpex (x)
LOCATE (x \ 2) - 11
COLOR 14, 5
IF x <> 50 THEN PRINT
         ctr2 " ษอออออออออออออออออออออออออออป "
         ctr2 " บ Viewer By  Amin Safinejad บ "
ctr2 " ษExploreอผ 2002 3rd Edition , (C)(R) ศออModeอออป "
IF x = 50 THEN ctr2 " บ                                              บ "
ctr2 " บ F1 = Display Help screen (this Dialog Box)   บ "
ctr2 " บ F2 = Switch Screen 50x80 or 25x80            บ "
IF x = 50 THEN ctr2 " บ                                              บ "
ctr2 " บ Up/Down/Left/Right  = Scroll Lines , Rows    บ "
ctr2 " บ Ctrl    + Left/Right= Scroll Row Page        บ "
ctr2 " บ Page Up / Page Down = Scroll Pages           บ "
ctr2 " บ Home    / End       = Rows ; First/Last      บ "
ctr2 " บ Ctrl    + Page Up/Dn= Begin / End of File    บ "
ctr2 " บ Esc                 = Exit File Viewer       บ "
ctr2 " บ Tab                 = Toggle Smart Color turnบ "
IF x = 50 THEN ctr2 " บ                                              บ "
ctr2 " บ ฎฏ ๐ฏ ฎ๐ ๐๐ On Top of the screen means that  บ "
ctr2 " บ Can you Scroll Rows or not. Number of Max.   บ "
ctr2 " บ Rows in Screen are after The Number of Lines บ "
ctr2 " บ This Program Changes Number of Rows When you บ "
ctr2 " บ Scrolling Lines for Best feet.               บ "
ctr2 " บ F7                  = Search                 บ "
ctr2 " บ F10                 = Change Mode :          บ "
ctr2 " บ Wraper/Explore. Explore Mode is for non-huge บ "
ctr2 " บ files, in This mode you can Scroll Rows.     บ "
IF x = 50 THEN ctr2 " บ                                              บ "
ctr2 " บ Press Escape to exit, www.safinejad.com,2008 บ "
 ctr " ศอออออออออออออออออออออออ Use /? for Usage Help ผ "
reject:
DO
a$ = INKEY$
LOCATE 19 + (x \ 2) - 8
COLOR 11
IF x = 50 THEN PRINT : PRINT
ctr "Time: " + time2$ + "   Date: " + date2$
LOOP WHILE a$ = ""
DO
LOOP WHILE INKEY$ <> ""
IF a$ <> CHR$(27) THEN GOTO reject
COLOR 15, 1
END SUB

SUB helpu
LOCATE , 1
PRINT
COLOR 14
ctr2 "Hey! please run this program with a text file as parameter!"
COLOR 13
ctr2 "In dos mode or command prompt (cmd) use :"
COLOR 15
ctr2 "ViewN.exe <filename> [/S] [/W] [/N] "
COLOR 6
ctr2 "This program displays a text file for you."
COLOR 5
ctr2 "If you are in windows,"
ctr2 "you can drag and drop a text file on the program's icon "
COLOR 14: PRINT
ctr2 "  /S - Start in 80x50 Screen width Mode   "
COLOR 11
ctr2 "  /W - Start in Wraper Mode               "
COLOR 9
ctr2 "  /N - Hide Wraped Lines in Wraper Mode   "
COLOR 7
PRINT
SLEEP .2
getkeys
END SUB

SUB helpwarp (x)
'ศษบปผอ
page1:
LOCATE (x \ 2) - 8
COLOR 10, 5
         ctr2 " ษอออออออออออออออออออออออออออป "
         ctr2 " บ Viewer By  Amin Safinejad บ "
ctr2 " ษอWraperอผ    2002 - 3rd. Edition.   1ออModeออป "
ctr2 " บ                                             บ "
ctr2 " บ Up      / Down     = Scroll Lines           บ "
ctr2 " บ Page Up / Page Down= Scroll Pages           บ "
ctr2 " บ Ctrl    + Page Up  = Go to First line       บ "
ctr2 " บ Ctrl    + Page Down= Go to End              บ "
ctr2 " บ Esc                = Exit File Viewer       บ "
ctr2 " บ Tab                = Toggle Smart Color     บ "
ctr2 " บ F1 = Display Help screen (this Dialog Box)  บ "
ctr2 " บ F2 = Switch Screen 50x80 or 25x80           บ "
ctr2 " บ F7 = Search                                 บ "
ctr2 " บ F10 = Change Mode : Wrap / Explore          บ "
ctr2 " บ Wraper Mode is for huge Files; Fast Scroll. บ "
ctr2 " บ                                             บ "
ctr2 " บ      Hit Tab for Second Page or Esc...      บ "
ctr2 " บ                                             บ "
ctr2 " ศออออออออออออออออออออออ Use /? for Usage Help ผ "
reget:
DO
a$ = INKEY$
LOCATE 17 + (x \ 2) - 8
COLOR 15
ctr "Time: " + time2$ + "   Date: " + date2$
LOOP WHILE a$ = ""
DO
LOOP WHILE INKEY$ <> ""
IF a$ = CHR$(27) THEN GOTO endhelp
IF a$ <> CHR$(9) THEN GOTO reget

LOCATE (x \ 2) - 8
COLOR 10, 5
         ctr2 " ษอออออออออออออออออออออออออออป "
         ctr2 " บ Viewer By  Amin Safinejad บ "
ctr2 " ษอWraperอผ    2002 - 3rd. Edition.   2ออModeออป "
ctr2 " บ Screen Help:                                บ "
ctr2 " บTwo Symboles on Info bar(on top) In This modeบ "
ctr2 " บ Show that Wraped Lines are Hidden or not;   บ "
ctr2 " บ(ฏฎ = Yes).The last character of wraped-hidedบ "
ctr2 " บ Lines will be shown in a Different          บ "
ctr2 " บ Color.                                      บ "
ctr2 " บ                                             บ "
ctr2 " บ Non-Hided Wraped Lines has an Arrow in end  บ "
ctr2 " บ of line.                                    บ "
ctr2 " บ                                             บ "
ctr2 " บ You Can Hide or Un Hide Wraped Lines With   บ "
ctr2 " บ /N Usage (ViewN.exe <file> [/N] [/W] [/S])  บ "
ctr2 " บ                                             บ "
ctr2 " บ      Hit Tab for First  Page or Esc...      บ "
ctr2 " บ   Downloaded from www.safinejad.com, 2008   บ "
ctr2 " ศออออออออออออออออออออออ Use /? for Usage Help ผ "
reget1:
DO
a$ = INKEY$
LOCATE 17 + (x \ 2) - 8
COLOR 15
ctr "Time: " + time2$ + "   Date: " + date2$
LOOP WHILE a$ = ""
DO
LOOP WHILE INKEY$ <> ""
IF a$ = CHR$(27) THEN GOTO endhelp
IF a$ <> CHR$(9) THEN GOTO reget1
GOTO page1
endhelp:
COLOR 15, 1
END SUB

FUNCTION inpt$
DIM now1$(10)
now1$(1) = CHR$(7)
now1$(2) = CHR$(9)
now1$(3) = CHR$(10)
now1$(4) = CHR$(11)
now1$(5) = CHR$(12)
now1$(6) = CHR$(13)
now1$(7) = CHR$(29)
now1$(8) = CHR$(30)
now1$(9) = CHR$(31)
now1$(10) = CHR$(255)
getkeys
DO
dahdah:
IF LEN(bzb$) > 78 THEN LOCATE 1, 80 ELSE LOCATE 1, LEN(bzb$) + 2, 1
a$ = INKEY$
IF a$ = "" THEN GOTO dahdah
IF LEN(a$) > 1 THEN a$ = RIGHT$(a$, 1)
IF a$ = CHR$(13) THEN EXIT DO
FOR x = 1 TO 10
IF a$ = now1$(x) THEN a$ = "": GOTO dahdah
NEXT
a$ = UCASE$(a$)
IF a$ = CHR$(8) THEN
IF bzb$ <> "" THEN bzb$ = MID$(bzb$, 1, LEN(bzb$) - 1)
a$ = ""
END IF
IF a$ = CHR$(27) THEN bzb$ = "": EXIT DO
IF LEN(bzb$) < 79 THEN bzb$ = bzb$ + a$
LOCATE 1, 2: PRINT bzb$;
IF LEN(bzb$) < 79 THEN PRINT " "
LOOP
inpt$ = bzb$
LOCATE , , 0
END FUNCTION

SUB showd (h AS LONG, x)
COLOR 14, 4
LOCATE x, 1
PRINT SPACE$(80);
LOCATE x, 1
PRINT "Search Finished! "; " Total Found: "; h; "   ";
LOCATE (x \ 2) - 1
COLOR 13
hg$ = str3$(h)
reh10:
IF LEN(hg$) < 10 THEN hg$ = "0" + hg$: GOTO reh10
FOR y = 1 TO 10
hh$ = MID$(hg$, y, 1)
IF y MOD 3 = 2 THEN hc$ = hc$ + ","
hc$ = hc$ + hh$
NEXT
COLOR 11, 0
LOCATE (x \ 2) - 2
ctr2 "ฐฑฒÛฒฑฐฑฒÛฒฑฐฑฒÛฒฑฐฑฒÛฒฑฐ"
ctr2 "ฑ     Total Found:      ฑ"
ctr2 "ฒ     0,000,000,000     ฒ"
ctr2 "ÛฒฑฐฑฒÛฒฑฐฑฒÛฒฑฐฑฒÛฒฑฐฑฒÛ"
LOCATE ((x \ 2) - 2) + 2
COLOR 13
ctr2 hc$
getkeys
PLAY "mf >>>>c60a60b60g60<<<<"
a$ = INPUT$(1)
COLOR , 1
getkeys
END SUB

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

SUB warp (maxlin, asd, v$, vb$, kopp)
DIM honor AS LONG
smp = -1
b = 1
bo = 1
'PRINT v$
OPEN v$ FOR INPUT AS #1
'PRINT v$: END
'INPUT #1, aa$
ls = 1
COLOR , 1: CLS
melted:
COLOR 1, 7
LOCATE maxlin, 1, 0
PRINT SPACE$(80);
LOCATE maxlin, 17
PRINT "Tab,Up/Down,[^]PgUp/PgDown,F2,F1,Esc,F10,F7";
COLOR 0: PRINT "ณ";
COLOR 11, 13
IF smp = 1 THEN PRINT "๛";  ELSE PRINT "X";
COLOR 0, 7: PRINT "ณ";
COLOR 15: PRINT "By Amin Safinejad";
LOCATE 1, 1: COLOR , 7: PRINT SPACE$(80)
SEEK 1, 1
DO
IF kgg$ = "" THEN kasan = 0
COLOR 11, 13
IF kgg$ = "" THEN
LOCATE maxlin, 80 - 19
IF smp = 1 THEN PRINT "๛";  ELSE PRINT "X";
END IF
'FOR a = 1 TO b - 1
'IF EOF(1) THEN EXIT FOR
'LINE INPUT #1, temp$
'a$ = INKEY$
'IF a$ <> "" THEN tnt = tnt + 1
'NEXT
'IF b = bo THEN GOTO ams
'FOR bb = b TO bo

IF b > 0 THEN SEEK 1, b * 83 + 1 ELSE SEEK 1, 2

'INPUT #1, aa$
'aa$ = " "
'IF bo < b THEN SEEK 1, SEEK(1) + LEN(aa$): ls = SEEK(1) + LEN(aa$)
'IF bo > b THEN SEEK 1, SEEK(1) - LEN(aa$): ls = SEEK(1) - LEN(aa$)
'NEXT
'FOR bb = bo TO b
'SEEK 1, ls
'INPUT #1, aa$
'aa$ = " "
'IF bo < b THEN SEEK 1, SEEK(1) + LEN(aa$): ls = SEEK(1) + LEN(aa$)
'IF bo > b THEN SEEK 1, SEEK(1) - LEN(aa$): ls = SEEK(1) - LEN(aa$)
'NEXT
'ams:
t = 0
'CLS

'lll

'LOCATE 1, 70: PRINT aa$; " "
LOCATE 2, 1
COLOR 15, 1
'LOCATE 2, 1
LOCATE 1, 1
t = 0
LOCATE 2, 1
FOR a = 1 TO maxlin - 2
t = t + 1
IF t >= maxlin - 1 THEN EXIT FOR

IF a <= 2 THEN
yhx = POS(0)
yhy = CSRLIN
LOCATE 1, 1: COLOR 0, 7: PRINT "Li"
LOCATE yhy, yhx
COLOR 15, 1
END IF

IF EOF(1) THEN EXIT FOR

LINE INPUT #1, a$
IF RIGHT$(a$, 1) = CHR$(2) THEN ark = 1 ELSE ark = 0
casa = 0
IF RIGHT$(a$, 1) = CHR$(3) THEN a$ = LEFT$(a$, LEN(a$) - 2): casa = 1
IF LEN(a$) > 80 THEN a$ = LEFT$(a$, 80)
'IF LEN(a$) <> 80 THEN PRINT a$; LEN(a$); a; b: END
IF ark = 0 THEN
IF CSRLIN >= 2 THEN
IF smp = 1 THEN smprint a$ + ";" ELSE PRINT a$;
END IF
END IF
IF ark = 1 THEN
IF CSRLIN >= 2 THEN
IF smp = 1 THEN smprint LEFT$(a$, LEN(a$) - 1) + ";" ELSE PRINT LEFT$(a$, LEN(a$) - 1);
COLOR 9, 0
PRINT RIGHT$(a$, 1);
COLOR 15, 1
END IF
END IF
IF casa = 1 THEN
COLOR 14, 4
PRINT CHR$(25);
ELSE
COLOR , 1
PRINT '" "
END IF
COLOR 15, 1
'IF NOT t >= maxlin - 3 THEN PRINT
NEXT
bo = b
IF tnt > 100 THEN
DO WHILE INKEY$ <> ""
LOOP
tnt = 0
END IF
yme = CSRLIN
COLOR 0, 7
LOCATE 1, 1: PRINT "Lines : "; b$; " / "; asd$; " ["; da$; "%] ณ ";
COLOR 0, 7
LOCATE 1, 40
PRINT " Wraper Mode : Fast Scroll  ณ";
COLOR 15
PRINT " Wrap On! ";
COLOR , 1
FOR hme = yme TO maxlin - 1
IF hme > 1 THEN LOCATE hme, 1: PRINT STRING$(80, "ÿ");
NEXT
COLOR 14, 4
IF INSTR(COMMAND$, "/N") = 0 THEN LOCATE 1, 79: PRINT "๐๐";  ELSE LOCATE 1, 79: PRINT "ฏฎ";
DO

IF kgg$ = "" THEN honor = 0
IF kgg$ <> "" THEN
COLOR 15, 2
LOCATE maxlin, 1
PRINT "Wait...[Esc=Abort] "; " Found: "; honor; "   ";
'a$ = CHR$(0) + CHR$(81)
'LOCATE 1, 1: PRINT huc - 79; betch
'IF NOT betch < huc - 79 THEN
a$ = CHR$(0) + CHR$(81): betch = 1
'ELSE
'a$ = CHR$(0) + CHR$(116)
'END IF
FOR ys = 2 TO maxlin - 1
FOR xs = 1 TO 80
dd$ = ""
IF 80 - xs >= LEN(kgg$) THEN
FOR mom = 0 TO LEN(kgg$) - 1
dd$ = dd$ + CHR$(SCREEN(ys, xs + mom))
IF INSTR(dd$, "ÿ") <> 0 THEN EXIT FOR
IF LEFT$(kgg$, LEN(dd$)) <> UCASE$(dd$) THEN dd$ = "": EXIT FOR
NEXT
'IF INSTR(kgg$, UCASE$(dd$)) = 0 THEN EXIT FOR
IF dd$ <> "" AND INSTR(dd$, "ÿ") <> 0 THEN EXIT FOR
IF UCASE$(dd$) = kgg$ THEN
LOCATE ys, xs, 1: COLOR 14, 5: PRINT dd$;
honor = honor + 1
artion:
getkeys
IF kasan = 0 THEN
LOCATE maxlin, 1
COLOR 15, 2
PRINT SPACE$(80);
LOCATE maxlin, 1
PRINT "Esc To Escape , Enter to Next , Tab to Don't Wait... Found : "; str3$(honor);
ami$ = INPUT$(1)
getkeys
IF ami$ = CHR$(27) THEN a$ = "": kgg$ = "": showd honor, maxlin: GOTO melted
IF ami$ = CHR$(9) THEN kasan = 1
IF ami$ <> CHR$(13) AND kasan = 0 THEN GOTO artion
LOCATE maxlin, 1
COLOR 15, 2
PRINT SPACE$(80);
LOCATE maxlin, 1
PRINT "Wait...[Esc=Abort]";
END IF
COLOR 14 + 16, 1: LOCATE ys, xs: PRINT dd$
END IF
END IF
IF INKEY$ = CHR$(27) THEN kgg$ = "": a$ = "": showd honor, maxlin: GOTO melted
NEXT
IF INKEY$ = CHR$(27) THEN kgg$ = "": a$ = "": showd honor, maxlin: GOTO melted
NEXT
nopio:
LOCATE , , 0
END IF

COLOR 0, 7: LOCATE maxlin, 1
IF LEN(vb$) > 15 THEN vop$ = "..." + RIGHT$(vb$, 12) ELSE vop$ = vb$
vobvob:
IF LEN(vop$) < 15 THEN vop$ = vop$ + " ": GOTO vobvob
IF kgg$ = "" THEN PRINT vop$; "ณ";
10
IF a$ <> "" THEN GOTO ool
a$ = INKEY$
ool:
COLOR 6, 7
b$ = LTRIM$(RTRIM$(STR$(b + maxlin - 3)))
asd$ = LTRIM$(RTRIM$(STR$(asd)))
da = ((b + maxlin - 2) * 100) \ asd
IF b + maxlin - 2 > asd AND asd > maxlin - 2 AND kgg$ = "" THEN b = (asd - maxlin) + 3: a$ = CHR$(0) + CHR$(72)
IF da < 0 THEN a$ = CHR$(0) + CHR$(80)
IF da > 100 THEN da = 100
da$ = LTRIM$(RTRIM$(STR$(da)))
asd$ = str2$(VAL(asd$) - 1)
wb10:
IF LEN(b$) < 7 THEN b$ = "0" + b$: GOTO wb10
wasd10:
IF LEN(asd$) < 7 THEN asd$ = "0" + asd$: GOTO wasd10
wda10:
IF LEN(da$) < 3 THEN da$ = "0" + da$: GOTO wda10

gj$ = ""
FOR wxp = 1 TO LEN(b$)
gj$ = gj$ + MID$(b$, wxp, 1)
IF wxp MOD 3 = 1 THEN gj$ = gj$ + ","
NEXT
b$ = gj$
gj$ = ""
FOR wxp = 1 TO LEN(asd$)
gj$ = gj$ + MID$(asd$, wxp, 1)
IF wxp MOD 3 = 1 THEN gj$ = gj$ + ","
NEXT
asd$ = gj$
IF RIGHT$(b$, 1) = "," THEN b$ = LEFT$(b$, LEN(b$) - 1)
IF RIGHT$(asd$, 1) = "," THEN asd$ = LEFT$(asd$, LEN(asd$) - 1)
IF asd <= maxlin - 2 THEN b2$ = asd$ ELSE b2$ = b$
COLOR 0, 7
LOCATE 1, 1: PRINT "Lines : "; b2$; " / "; asd$; " ["; da$; "%] ณ "

IF a$ = "" THEN GOTO 10
tnt = tnt + 1
COLOR , 1
y = 0
IF a$ = CHR$(27) THEN
WIDTH 80, 25: COLOR 7, 0: CLS
ON ERROR RESUME NEXT
CLOSE
KILL "read.tsp"
getkeys
END
END IF
IF (b + maxlin - 2) >= asd AND kgg$ <> "" AND a$ = CHR$(0) + CHR$(81) THEN kgg$ = "": showd honor, maxlin: GOTO melted

IF a$ = CHR$(0) + CHR$(65) THEN
LOCATE 1, 1: COLOR , 2: PRINT SPACE$(80)
LOCATE 1, 1: COLOR 15, 2: PRINT ":";
COLOR 14
kgg$ = inpt$
IF kgg$ <> "" THEN smp = -1: LOCATE maxlin, 1: COLOR , 2: PRINT SPACE$(80);
y = 1
END IF

IF a$ = CHR$(0) + CHR$(132) THEN IF b > 1 THEN b = 1: y = 1
IF a$ = CHR$(0) + CHR$(118) THEN IF b < (asd - maxlin) + 2 THEN b = (asd - maxlin) + 2: y = 1

IF a$ = CHR$(9) THEN smp = -smp: y = 1

IF a$ = CHR$(0) + CHR$(72) THEN IF b > 1 THEN b = b - 1: y = 1
IF a$ = CHR$(0) + CHR$(80) AND da >= 100 THEN a$ = ""
IF a$ = CHR$(0) + CHR$(80) AND NOT EOF(1) AND b <= asd THEN b = b + 1: y = 1
IF a$ = CHR$(0) + CHR$(60) THEN
y = 1
IF maxlin = 25 THEN
maxlin = 50
ELSE
IF maxlin = 50 THEN maxlin = 25: IF da = 100 THEN b = asd - maxlin: y = 1
END IF
WIDTH 80, maxlin
VIEW PRINT 1 TO maxlin
IF asd <= maxlin - 2 THEN b = 1
GOTO melted
'73,81
END IF
IF a$ = CHR$(0) + "I" THEN
IF b > maxlin - 2 AND kgg$ = "" THEN b = b - (maxlin - 2): y = 1 ELSE b = 1: y = 1
END IF
IF a$ = CHR$(0) + "Q" AND NOT EOF(1) THEN
IF b + 2 < asd - maxlin * 2 THEN
'IF b = asd - maxlin + 2 AND kgg$ <> "" THEN a$ = "": kgg$ = "": showd honor, maxlin: GOTO melted

b = b + (maxlin - 2): y = 1
ELSE
IF kgg$ = "" THEN b = asd - maxlin + 2: y = 1 ELSE b = b + (maxlin - 2): y = 1
'IF kgg$ <> "" THEN IF VAL(b$) > VAL(asd$) THEN kgg$ = "": a$ = "": showd honor, maxlin: GOTO melted
END IF
END IF
'IF a$ = CHR$(13) THEN WIDTH 80, 25: COLOR 7: CLS : getkeys:end
IF a$ = CHR$(0) + CHR$(59) THEN helpwarp (maxlin): GOTO melted
IF y = 1 THEN EXIT DO
IF a$ = CHR$(0) + CHR$(68) THEN GOTO expi
a$ = ""
LOOP
LOOP
expi:
CLOSE #1

END SUB

