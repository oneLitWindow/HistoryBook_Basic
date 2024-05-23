DECLARE FUNCTION ohno$ ()
DECLARE SUB notdone ()
DECLARE SUB drawadamak (vv!)
DECLARE SUB ender ()
DECLARE SUB cadr (x1!, y1!, x2!, y2!, dly!, colr!)
DECLARE SUB setrgbpalette (x!, R!, g!, b!)
DECLARE FUNCTION str2$ (x!)
DECLARE SUB delay (x!)
DECLARE FUNCTION time2$ ()
DECLARE FUNCTION date2$ ()
DECLARE SUB getkeys ()
DECLARE SUB ctr (y!, TEXT$)
DECLARE SUB DEMO ()
CLEAR
'DEF SEG = 0
'POKE 1047, 96
SCREEN 12
WIDTH 80, 60
VIEW PRINT 1 TO 60
ON ERROR RESUME NEXT
OAS = 6
live = 1
l$ = ""
reter = 1
adapt = 0
rerun:
kki = 20
WIDTH 80, 60
VIEW PRINT 1 TO 60
ON ERROR RESUME NEXT
CLS
IF reter = 1 AND adapt = 0 THEN PLAY "mB<<<E40....F40.....B20.............B25..........A25.................F25..A15>>>"
reter = 0

LOCATE 1, 1, 0
IF LO$ <> "A" THEN

DO WHILE INKEY$ <> "": LOOP
PRINT
PRINT
COLOR 7
ctr 3, "Wait 20 seconds for Demo or:"
COLOR 10
PRINT "        Press a Level (1-8) or A to Automatic Change level or E to Exit: "; : COLOR 10: PRINT "_"
COLOR 10
PRINT "                     This Game has 15 levels...so it's better to Press 'A'"
PRINT : PRINT
COLOR 14
PRINT "                    Give your friend :  "; : COLOR 10 + 16: PRINT CHR$(2)
COLOR 14, 0
PRINT "                    You are          :  "; : COLOR 10, 9: PRINT CHR$(1)
COLOR 12, 0
PRINT
PRINT "                    Keep away from   :  "; : COLOR 14: PRINT "л O "; : COLOR 13: PRINT "? } { "; : COLOR 11: PRINT "м п "; : COLOR 12: PRINT "*"; : COLOR 10: PRINT " К";
PRINT
COLOR 15, 0
PRINT "                    In Auto mod      :  4 x "; : COLOR 12 + 16, 0: PRINT CHR$(3); : COLOR 15, 0: PRINT " = "; : COLOR 10, 9: PRINT CHR$(1): COLOR 10, 0
COLOR 10
ctr 15, "Program By:  Amin Safinezhad (ASP)"
rtr = INT(TIMER)
COLOR 0
redp = 1
greenp = 1
bluep = 1
GREEN = 30
BLUE = 0
RED = 50
plp = 5
redo:
setrgbpalette 0, 0, 0, 20
LOCATE 4, 2
COLOR 1
PRINT " Press a Level (1-8) or A or Enter to Automatic Change level or E to Exit: "; : COLOR 10 + 16: PRINT "_  "
LOCATE , 2
COLOR 13
PRINT SPACE$(78)
ctr 6, "This Game has 15 levels...so it's better to use Auto Leveling"

IF arab = 0 THEN drawadamak 1 ELSE arab = 0
cadr 0, 0, 640, 135, 0, 10
cadr 0, 133, 640, 480, 0, 10
DO
    IF RED + plp > 63 THEN redp = -plp
      IF RED - plp < 0 THEN redp = plp
IF GREEN + plp > 63 THEN greenp = -plp
  IF GREEN - plp < 0 THEN greenp = plp
  IF BLUE + plp > 63 THEN bluep = -plp
    IF BLUE - plp < 0 THEN bluep = plp
RED = RED + redp
GREEN = GREEN + greenp
BLUE = BLUE + bluep
l$ = INKEY$
l$ = UCASE$(l$)
SOUND 0, 1
PALETTE 1, RED + 256 * GREEN + 256 ^ 2 * BLUE
  IF INT(TIMER * 10) MOD 2 = 0 THEN LOCATE 4, 77: PRINT " " ELSE LOCATE 4, 77: PRINT "_"
  COLOR 15
leet$ = LTRIM$(RTRIM$(STR$(INT(kki - (TIMER - rtr)))))
IF LEN(leet$) < 2 THEN leet$ = "њ" + leet$
LOCATE 3, 31: PRINT leet$; " "
IF INT(kki - (TIMER - rtr)) <= 0 THEN DEMO: GOTO rerun
LOOP WHILE l$ = ""
IF l$ = CHR$(13) THEN l$ = "A"
IF l$ = CHR$(27) THEN l$ = "E"
oz$ = ""
getkeys
re = 0: live = 1
IF VAL(l$) > 8 THEN l$ = "*"
IF l$ = "A" THEN

PLAY "A30B30G30D30"
COLOR 15, 9
IF ASC(l$) <> 0 THEN LOCATE 4, 77: PRINT l$: oz$ = ""
LS = 1: LO$ = "A": LOCATE 4, 77: PRINT l$: oz$ = "": SLEEP 1
WIDTH 80, 30
VIEW PRINT 1 TO 30
COLOR , 0: CLS : GOTO 1
END IF

IF l$ = "E" THEN ender: SYSTEM
COLOR 15, 0
IF VAL(l$) <> 0 THEN
WIDTH 80, 30
VIEW PRINT 1 TO 30
END IF
IF l$ = "1" THEN COLOR , 0: CLS : GOTO 1
IF l$ = "2" THEN COLOR , 0: CLS : GOTO 2
IF l$ = "3" THEN COLOR , 0: CLS : GOTO 3
IF l$ = "4" THEN COLOR , 0: CLS : GOTO 4
IF l$ = "5" THEN COLOR , 0: CLS : GOTO 5
IF l$ = "6" THEN COLOR , 0: CLS : GOTO 6
IF l$ = "7" THEN COLOR , 0: CLS : GOTO 7
IF l$ = "8" THEN COLOR , 0: CLS : GOTO 8
'PRINT "                       ";
'PLAY "MB <G60A60C60> MB"
LOCATE 4, 77
PRINT "*":
COLOR 12
ctr 5, "Error! > Choose right keys!!!": SLEEP 2: arab = 1: GOTO redo
arab = 0
getkeys
ELSE
IF LS <> 15 THEN
IF hold = 0 THEN
SCREEN 0: SCREEN 12
WIDTH 80, 50
drawadamak 2
LINE (0, 0)-(640 - 8, 150), 0, BF
setrgbpalette 1, 63, 0, 20
cadr 110, 45, 320 + 200, 100, 0, 9
pp = 1
iop = 0
PLAY "MF <<<a60>>>"
getkeys
DO
COLOR 11
ctr 5, " Ready For Next level! Press any key to Start"
PALETTE 11, 256 * iop + 256 ^ 2 * (63 - iop)
IF iop >= 63 THEN pp = -1
IF iop <= 0 THEN pp = 1
iop = iop + pp
SOUND 0, .01
LOOP WHILE INKEY$ = ""
getkeys
SCREEN 0: SCREEN 12
VIEW PRINT 1 TO 30
'setrgbpalette 11, 0, 63, 63
END IF
LS = LS + 1: l$ = LTRIM$(RTRIM$(STR$(LS)))
ELSE
COLOR 0, 0: CLS : GOTO rerun
END IF
END IF

COLOR , 0: CLS
COLOR 15, 9
WIDTH 80, 30
VIEW PRINT 1 TO 30
IF l$ = "1" THEN COLOR , 0: CLS : GOTO 1
IF l$ = "2" THEN COLOR , 0: CLS : GOTO 2
IF l$ = "3" THEN COLOR , 0: CLS : GOTO 3
IF l$ = "4" THEN COLOR , 0: CLS : GOTO 4
IF l$ = "5" THEN COLOR , 0: CLS : GOTO 5
IF l$ = "6" THEN COLOR , 0: CLS : GOTO 6
IF l$ = "7" THEN COLOR , 0: CLS : GOTO 7
IF l$ = "8" THEN COLOR , 0: CLS : GOTO 8
IF l$ = "9" THEN COLOR , 0: CLS : GOTO 9
IF l$ = "10" THEN COLOR , 0: CLS : GOTO 10
IF l$ = "11" THEN COLOR , 0: CLS : GOTO 11
IF l$ = "12" THEN COLOR , 0: CLS : GOTO 12
IF l$ = "13" THEN COLOR , 0: CLS : GOTO 13
IF l$ = "14" THEN COLOR , 0: CLS : GOTO 14
IF l$ = "15" THEN COLOR , 0: CLS : GOTO 15
1
PRINT "ллллллллллллллллллллллллллллллллллллллллллл"
PRINT "лА   Б            л                      Вл"
PRINT "л          л             л          Hello!л"
PRINT "ллллллллллллллллллллллллллллллллллллллллллл"
GOTO start'*************************************************************
2
PRINT "ллллллллллллллллллллллллллллллллллллллллллллллллллллллллл"
PRINT "ллВ!                                                   лл"
PRINT "ллллллллллллллллллллллллллллллллллллллллллллллллллллл  лл"
PRINT "лл                                         лл      лл  лл"
PRINT "лл  ллллллллллллллллллллллллллллллллллллл  лл  лл  лл  лл"
PRINT "лл           Б                        Алл      лл      лл"
PRINT "ллллллллллллллллллллллллллллллллллллллллллллллллллллллллл"
PRINT ""
GOTO start'*************************************************************
3
PRINT "лллллллллллллллллллллллллллллллллллллллллл"
PRINT "ллА            Б                        лл"
PRINT "лллллллллллллллллллллллллллллллллллллл  лл"
PRINT "ллВ!  лл                                лл"
PRINT "лллл  лл  лллллллллллллллллллллллллллллллл"
PRINT "лл    лл                                лл"
PRINT "лл  лллллллллллллллллллллллллллллллллл  лл"
PRINT "лл  лл      лл      лл      лл      лл  лл"
PRINT "лл  лл  лл  лл  лл  лл  лл  лл  лл  лл  лл"
PRINT "лл      лл      лл      лл      лл      лл"
PRINT "лллллллллллллллллллллллллллллллллллллллллл"
GOTO start'*************************************************************
4
PRINT "ллллллллллллллллллллллллллллллл"
PRINT "ллл ! л   л   л   л   мм л   лл"
PRINT "ллВ л   л   л   л   л пп   л лл"
PRINT "лллллллллллллллллллллононллл лл"
PRINT "ллАл   лБ    л   мм ллммлллл лл"
PRINT "лл л л ллллл л л пп   лл   л лл"
PRINT "лл л л л   л л л мм л лл л л лл"
PRINT "лл л л л л л л л пп л л  л л лл"
PRINT "лл л л л л   л л мм л л лл л лл"
PRINT "лл л л л л л л л пп л л лл л лл"
PRINT "лл   л   л л   л он л   лл   лл"
PRINT "ллллллллллллллллллллллллллллллл"
GOTO start'*************************************************************
5
PRINT "лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл"
PRINT "лл    он            л   л   лл2002 - Adamak(R) SE by Amin Safinezhadлл"
PRINT "лл  ллллллллллллллл л л л м лллллллллллллллллллллллллллллллллллллллллл"
PRINT "лл  л   л       л     л   п       л*лл < л   л   л   л   л   л   л  лл"
PRINT "лл  л л л ллл ллл ллллллллллллллл л лл л л л л л л л л л л л л л л  лл"
PRINT "лл  л л л л   л   л Бл          л л    л л л л л л л л л л л л л л  лл"
PRINT "лл  л л л л ллллллл ллллллллллл л лллллл л л л л л л л л л л л л л  лл"
PRINT "лл мл л л л л         л  л ?  лБл лппппп   л   л   л   л   л   л    лл"
PRINT "лл()л л л л л ллллл ллл ллллл л л л      лллллллллллллллллллллллллл лл"
PRINT "ллп л л л л л л   л лБл л   л л л л лллллл              Help!...В л лл"
PRINT "лл  л л л л л л л ллл л л л л л л л  он }   ллллллллллллллллллллллл лл"
PRINT "лл  л л л л л л л     л л л л л л л *Бл               л             лл"
PRINT "лл  л л л л л л ллллллл л л л л л лллллннлллллллллллл ллллллллллллл лл"
PRINT "лл  л л л л л л           л л л л л*     л  ??                    л лл"
PRINT "лл  л л л л л ллллллллллллл л л л ллллллллллллллллллллллллллллллллл лл"
PRINT "лл  л л л л л л           л л л л                                   лл"
PRINT "лл  л л л л л л ллллллллл л л л ллллллллллллллллллллллллллллллллллл лл"
PRINT "лл    л   л             л л л л                                     лл"
PRINT "ллллллллллллллллллллллл л л л ллллллллллллллллллллллллллллллллллллл лл"
PRINT "ллА>>>                  л   л                ??                     лл"
PRINT "лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл"
GOTO start'*************************************************************
6
PRINT "ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл"
PRINT "ллOO      OOOOOOOOOOOOOOOOOOOOOOO Help! OOOOOOOOOOOOOOOOOOOOOOOOOOOOOлл"
PRINT "лл0O0OOOO00000000         +     OOOВO0   O   O   O 0 O 0 O   O   O   лл"
PRINT "ллO000000OOOOOOOO OOOOOOOOOOOOO O O^0O O O O O O O O O O O O O O O O лл"
PRINT "лл  O O O00000O00 O  0    0    0O O^O0 O O O O O O O O O O O O O O O лл"
PRINT "лл  O O O O 0OOOOOOOOOOOOOOOOO0 O OOOOOO O O O O O O O O O O O O O O лл"
PRINT "лл O0 O O O O     00  O   0   O O OOOOOO   O   O 0 O   O   O   O   O лл"
PRINT "лл  O O O O O OOOOO O O 0OOO0 O O       OOOOOOOOOOOOOOOOOOOOOOOOOOO00лл"
PRINT "лл  O O O O O O   O O O O   O O O O OOO 0                          -Oлл"
PRINT "лл  O O O O O O O OOO O O O O O O OO + OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOлл"
PRINT "лл 0O O O O+O O O     O O O O O O    O                O             Oлл"
PRINT "лл  O O0O O+O OБOOOOOOO O O O O OOOOOOOOOOOOOOOOOOOOO OOOOOOOOOOOOO Oлл"
PRINT "лл  O O O O+O OO  OO    O O O O OБO     ((()))       ^            O Oлл"
PRINT "лл  O O O O O+Б+OOOOOOOOOOO O O O O OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO*Oлл"
PRINT "лл  O O O O O O ?         O O O O                                   Oлл"
PRINT "лл  O O O O O O OOOOOOOOO O O O 0OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO0Oлл"
PRINT "ллO0Б0O+0+O   O         O O O O                                     Oлл"
PRINT "ллOOOOOOOOOOOOOOOOOOOOO O O O 0OO0O0O0O0O0O0O0O0O0O0O0O0O0O0OO0O0OO0Oлл"
PRINT "ллА>>>                  O   O/\                                     Oлл"
PRINT "ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл"
GOTO start'*************************************************************
7
COLOR 10, 0
PRINT "OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO"
PRINT "OАO   O   O   O    O   O   O   O } O O O  O o 000000O0OOOм>Ha Ha Haм >Help! ВO"
PRINT "O   O   O   O   O  0 O 0 O 0 O 0 0БO O O  O O OOoOO000oOOп*********пOOOOO0000O"
PRINT "OOOOOOOOOOOOOOOOOOO!0 OOO O O O O O?O } 0 O O OOoOOOOOoooOOOOOOOOoOOO00oooOOOO"
PRINT "O0O O   O   O    OO0OO   O O O O*O O O O  O O OOOOOOOOOOoOoOOOOOoOOOO 0OOOOO O"
PRINT "O00   OБ  O  0 O    00    O   O   O   O O   O OOOOOOOOOoOoOoOoooOoOOO OOOOOO O"
PRINT "O0OOOOOOOOOOOO0OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO00OOOOOO O"
PRINT "O 0 O   O    O0OO   O   O   OO O 00 O    ??    O0   00   OOOOOOOOOOO0O0OOOO  O"
PRINT "OOO0O O O O  O00O O O O O O  O 0 O0 O OO OO O00O OO0OOOO OOOOOOOБOO00O0 OOO OO"
PRINT "O0O0O O O O  0O00 O O O    O O O 0O O OO OO OO O OO0OO00 O   O000O00    OOO OO"
PRINT "O0 0 0O   O  БO00 O   O OБ O 0 O O0 0 OO    O0000O00  O0   O 00O000OOO0     OO"
PRINT "OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO"
GOTO start'*************************************************************
8
PRINT "OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO"
PRINT "O*OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO Help! O"
PRINT "OАOO000O000O000O0000O000O00000000?O0000O0000000O000O000O000O00O0O00?O0000OВO"
PRINT "O0000O000O000O000O00O0O0O0OOOOO0O0O00O000O000O000O000O000O000O0000OO0OO0OO0O"
PRINT "O0OOO0OOO0OOO0OOO0O000O000O000O0O000OOOOOOOOOOOOOOOOOOOOOOOOO00OO0O00000OБ0O"
PRINT "O000OO000O000O00O000OБO0OOO0O0O0O0OO000O000O000O000O000O00O0O000O0O0OOOOO0OO"
PRINT "OO00000O000O000O00O00O0O000O0000O0000O000O000O000O000O0?0O0000000OO000O00Б0O"
PRINT "O0OOOOO0OOO0OБOOOO0OOOOOOOO0OOOO0OOOO0OOO0OOO0OOO0OOO0OOO0OOOOOOO0OOOO0OOO0O"
PRINT "O000000000000000O000O000O000O000O000O000O000O000O000O000O000O000O0O00000O00O"
PRINT "O000000000000OO000O000O000OБ00O000O000O000O000O000O000O000O000O000O?OOOO00OO"
PRINT "O00000000000000OOO0OOO0OOO0OOO0OOO0OOO0OOO0OOO0OOO0OOO0OOO0OOO0OO0OO000OO0OO"
PRINT "O}0?0000{0000O000000000000?000000000000000000000000000O0O0O0OOO0O000000000OO"
PRINT "OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO"
PRINT "OOOO It is Very good if you Came from first level Go on! , Can you ?!! OOOOO"
PRINT "OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO"
GOTO start'*************************************************************

9
PRINT "ЩЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЫЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЫЫЫЫЫЫЫЫЫЫЫЫЫЫЭЛ"
PRINT "КO00000000000000000000000000000000К00000000000000000000000000000ЬO0000000000000Й"
PRINT "К00ЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭ0КO0ЩЭЫЭЭЭЫЭЭЭЫЭЫЭЭЭЭЭЭЭЭЭЭЭЭЛ0Ь00OOOOOOOOO0Ю0Й"
PRINT "К00ЩЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЫЭЛ0К00К0К000К000К0К000000000000К0Ь0O00000O00O0Ю0Й"
PRINT "К0OК00000000000000000000000000КБК0К0OК0К0К0К0К0К0К0ЩЫЫЫЫЫЫЫЫЛ0К0Ь000O0O00O0O0Ю0Й"
PRINT "К0OК0OЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЛ0OК0К0К00К0К0К0К0К0К0К0КБ000000000К0ЬO0O0O0O0O0Ю0Ю0Й"
PRINT "К00К00000000000000000000000К00К0К0КO0К0К0К0К0К0К0К0ШЪЪЪЪЪЪЪЪЪЭМ0Ь00O00O0000Ю0Ю0Й"
PRINT "КO0ЬЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭ0OКO0К0К0КO0К0К0К0К0К0К0К00000000000000ШЪЪЪЪЪЪЪЪЮ0ЮOЮ0Й"
PRINT "К00К00000000000000000000000К00К0К0К00К0К0К0К0К0К0ЬЭЭЭЭЭЭЭЭЭЭЭЛO0000000000К0К0I0К"
PRINT "К0OКO0ЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЙ0OК0К0К0OК0К0К0К0К0К0К00ЩЭЭЭЭЭЭЭЭЪЭЭЭЭЭЭЭЭЭЛ0К0К0К0К"
PRINT "К00К00000000000000000000000К00К0К0К00К0К0К0К0К0К0К00К000000000000000000К0К0К0КOК"
PRINT "К0OЬЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭO0КO0К0К0КO00000К0К0К0К0К00К0ЩЭЭЭЭЭЭЭЭЭЭЭЭЭЭЛ0К0К0К0КOК"
PRINT "К00К00000000000000000000000К00К0К0ШЭЭЭЭЭЭМ0К0К0К0К00К0К00000000000000К0К0К0К0I0К"
PRINT "КO0К0OЩЫЫЫЫЫЫЫЫЫЭЭЭЭЭЭЭЭЭЭЭЙ00К0К0000000000К0К0К0К00К0К0ЩЭЭЭЭЭЭЭЭЭЭЛ0К0К0К0К0К0К"
PRINT "КO0К00000000000000000000000К00К0ШЭЭЭЭЭЭЭЭЭЭМ0К0К0К00К0К0К0 HELP!! ВК0К0К0К0К0К0К"
PRINT "К00ШЭЭЪЪЪЪЪЪЪЪЪЪЭЭЭЭЭЭЭЛ00OК0OК00000000000000К0К0К00К0К0К0ЭЭЭЭЭЭЭЭЭМ0К0К0К0К0I0К"
PRINT "КO000000000000000000000К0O0К00ШЭЭЭЭЭЭЭЭЭЭЭЭЭЭЙ0К0ШЭЭЙ0К0К000000000000К0К0К0КoК0К"
PRINT "ЬЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭ0К0O0К0000000000000000000!!!!АК0К0ШЭЭЭЭЭЭЭЭЭЭЭЭМ0К0К0К0К0К"
PRINT "К0000000000000000000000К0O0ШЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЙ0К0000000000000000К0К0К0КOК"
PRINT "Ь0ЫЭЫЭЭЭЫЭЭЭЫЭЭЭЫЭЭЭЫЭЭЙ0000000000000000000000000000К0ШЫЭЭЭЫЭЭЭЫЭЭЭЫЭЭЭЙ0К0К0I0К"
PRINT "К0К0К000К000К000К000К0OШЫЫЭЭЭЫЭЭЭЫЭЭЭЫЭЭЭЫЭЭЭЫЭЭЭЫЛ0К00К000К000К000К000К0К0К0К0К"
PRINT "К0КOК0К0К0К0К0К0К0К0К000ЬЙ000К000К000К000К000К000ЬЙ0ЬЛ0К0К0К0К0К0К0К0К0К0К0К0К0К"
PRINT "К0К000К000К000К000К000К0000К000К000К000К000К000К0000КК000К000К000К000К000К0К0К0К"
PRINT "К0Ь0ЫЭЪЭЭЭЪЭЭЭЪЭЭЭЪЭЭЭЪЭЭЭЭЪЭЭЭЪЭЭЭЪЭЭЭЪЭЭЭЪЭЭЭЪЭЭЭЭЪЪЭЭЭЪЭЭЭЪЭЭЭЪЭЭЭЪЭЭЭЙ0КOI0К"
PRINT "К0Ь0Й000O000O0000OOOO0000O000O000O0000000O0000000O00000OO000O000O000OO000Ю0Й0I0К"
PRINT "К0Ь000O000O000OO000000OO000O000O000OOOOO000OOOOO000OOO0000O000O000O0000O000Й0Ю0Й"
PRINT "Ь0ЮЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЮЮЮ0Ю0Й"
PRINT "К0000000000000000000000000000000000000000000000000000000000000000000000000000O0Й"
PRINT "ШЭЪЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЪЪЪЪЪЪМ"
GOTO start'*************************************************************

10
PRINT "O0000000000000000000000O0OOOOOO00000O00000O000000000000000O00000000000000O0000O"
PRINT "O0OOOOOOOOOOOOOOOOOOOO0O00000000OOO0O0O0O000OOOOOOO0OOOOO0O0O0O0O0OO00000000O0O"
PRINT "OБ0000000000000000OOOO0000000000OO00O0O0OOOOO00000O00O0000O0O00000OO0OO0O0OOO0O"
PRINT "OO0OOOOOOOOOOOOOO0O000OOOOOOOOOOOO0OO0O000000000000OOO0OO0O0O0O0O0O00O0000O0O0O"
PRINT "OO00000000000OOOO000O00000000000OO0OO000000000000O0O000000O0O0O000000O000O0000O"
PRINT "O0000000OOOOO000O0OOOOOOOOOOOOO0OO000OOOOOOOOOOO0O0O0OO0O0O0O0O0OO0O0000O00OOOO"
PRINT "O0OOOOOOO000O0O0O0OO00O0000OOOO0OO0O0000000000OO0O0O0OO000O0O00000000000O00O000"
PRINT "O0O00000O0O000O0O0OO0O00OO0OOO00OO0OOO0OO00OOO000O0O00O0OOO00OO0OOOOOO00O0O00O0"
PRINT "00O0O0O000OOOOO0O0OO0O0OOO000O0OOO0O000000O0000OOO0OO000OOOO0OO0O00000OO00O0OO0"
PRINT "0O0000OO0OO00000O0000O0OOOOO0O00OO0O000OOOO0O0O00O0O00O0OOO00OO0O0OOO00000O00O0"
PRINT "0O0OOOOO0OO0OOOOOOOOOO00OO000OO0OO0OOOO00000OOO0OO0O0OO0OOO0OOO0O00OO0OOOOOOOO0"
PRINT "0O0000OO0OO000000000OOO0OO0OOOO0OO000000O0OO00000O0O0OO0OOO00OO00O0OO0O00000000"
PRINT "O0OOO0OOOOOOOOOOOOO0OO00OO000O00OO0OOOOOO0OO00O00O0O000OOOOO0OO0O00OO0O0OOOOOO0"
PRINT "O0OOO0O000000OO00000OO0OOOOO0O0OOO00000000OO0O0O0O0OOO0OO0000OO0O0OOO0O0O000000"
PRINT "O000O0O0OOOO0OO0OOOOOO00OOOO0O000O0O0O00000O000O0OO0000OO0OOOOOOO00OO0O00OOO0OO"
PRINT "O0O000O0000O0OO0O0000OO0OO000O0OOO0O0OOOOOOOO0O00OO0OOOOO000000O0O0O00OO000O0OO"
PRINT "O0OOOOOOOO0O0OO000OO0OO0OO0OO00OOO0O0O00000000O00OO000000OOOOO0O0O0OO0O00OOO000"
PRINT "O0OА0000000O0OOOOOOO0OO0OO0OO0OOOO0O0O0OOOOOOOO0OOO0OOOO0OOO000O0O000OO0O000000"
PRINT "000OOOOOOOOO0O0000000OO0OO0OO00O000O0O0OOO0000O0000OO0000OOO0OOO00OO0OO0OOOO000"
PRINT "0O00000000000O0O0OOOO000OO000O0O0OOOOO000O0OOO0OOO0OO0OOOOOO00000O0O0OO0000O0OO"
PRINT "0OOOOOOOOOOOOO0O0O00O0OOOOOO0O0O00O000OO0O0O0000O000O0OO0000OOOO0O0O0OO0O0OO000"
PRINT "O00000000000000O000OO00OOO000O00O0O0OВOO0O000OO0O0O0O0OO0OO0OOOO0OOO0000O000OO0"
PRINT "O0OOOOOOOOOOOOOOOOOOOO0OOO0OOOO0O0O0OOOO0O0O0O00O0O0O0OO0O00OOOO000OOOOOOOO0OO0"
PRINT "00000000000000000000000OOO000O00O0O000000O0000O0O0O0O0OO0O0OOOOOOO0OO0O0000OOO0"
PRINT "0OOOOOOOOOOOOOOOOOOOOOOOOOOO0O0OO0OOOOOOOO0O0O00O0O0O0OO0O000000000OO0O00O00000"
PRINT "O0000000000000000000000000000O0OOOOOOOOOOOOOOOOOO0O0O0000OOOOOOOOOOOO0O0O0OOOOO"
PRINT "O0O0OOOOOOOOOOOOOOOOOOOOOOOOOO00000000000000000000O0OOOOO0000000000000O00000000"
PRINT "O0O00000000000000000000000000OOOOOOOOOOOOOOOOOOOOOOOO0OOOO0OOOO0OO0O000OOOOOOO0"
PRINT "O0000000000000000000000000Б0000000000000000000Б00000000000000000000000000000000"

GOTO start'*************************************************************

11
GOTO start'*************************************************************

12
GOTO start'*************************************************************

13
GOTO start'*************************************************************

14
GOTO start'*************************************************************

15
GOTO start'*************************************************************
'End of Levels.

start:
lx = 0
ly = 0
VIEW PRINT 1 TO 30
hold = 0
adapt = 1
getkeys
'PLAY "mf a60"
DIM s$(29, 81)
FOR a = 1 TO 29
FOR b = 1 TO 80
s$(a, b) = CHR$(SCREEN(a, b))
IF s$(a, b) = "А" THEN x = a: y = b
IF s$(a, b) = "Б" THEN s$(a, b) = CHR$(3): LOCATE a, b: PRINT CHR$(3);
IF s$(a, b) = "В" THEN s$(a, b) = CHR$(2): LOCATE a, b: PRINT CHR$(2);
NEXT b, a
COLOR , 0: CLS
a$ = CHR$(1)
COLOR 9
LOCATE 30, 1: PRINT "Wait to Read Screen...";
FOR a = 1 TO 29
FOR b = 1 TO 80
Q$ = s$(a, b)
COLOR 15, 0
COLOR 2
IF Q$ = "*" OR Q$ = "<" THEN COLOR 12
IF Q$ = ">" OR Q$ = "<" THEN COLOR 4
IF Q$ = "{" OR Q$ = "}" THEN COLOR 13
IF Q$ = "O" OR Q$ = "0" OR Q$ = "л" THEN COLOR 14
IF Q$ = "o" THEN COLOR 6
IF Q$ = "м" OR Q$ = "п" THEN COLOR 11
IF Q$ = "?" THEN COLOR 13
IF Q$ = "&" THEN COLOR 2
IF Q$ = "о" OR Q$ = "н" THEN COLOR 14
IF INSTR(ohno$, Q$) <> 0 THEN COLOR 10
IF l$ = "A" OR oz$ = "A" THEN
IF Q$ = CHR$(3) THEN
COLOR 12
IF PASS = 0 THEN COLOR 12
END IF
ELSE
IF Q$ = CHR$(3) THEN COLOR 2: live = 0
END IF
IF Q$ = CHR$(2) THEN COLOR 10 + 16: ax = a: by = b
LOCATE a, b: PRINT Q$;
NEXT b, a
getkeys
BGR:
AA:
IF l$ = "A" OR oz$ = "A" THEN
IF s$(x, y) <> CHR$(3) THEN
LOCATE 30, 40: PRINT "                 ";
IF lx <> 0 AND ly <> 0 THEN COLOR 12: LOCATE lx, ly: PRINT CHR$(3)
END IF
COLOR 15
IF s$(x, y) = CHR$(3) THEN
IF t = 0 AND live < OAS THEN
s$(x, y) = ""
live = live + .25: re = re + 1: t = 1: PLAY "MB B60D60 MB": IF live = INT(live) THEN PLAY "MB A60G60C60F60 MB": re = 0
ELSE
LOCATE 30, 40: PRINT "More lives!";
lx = x
ly = y
END IF
'END
END IF
END IF
IF x = 0 OR y = 0 OR x = 40 OR y = 81 THEN GOTO rerun
COLOR 11
LOCATE x, y: PRINT a$;

D$ = INKEY$
COLOR 10, 0
IF INT(TIMER * 17) MOD 7 = 0 THEN COLOR 9
IF INT(TIMER * 17) MOD 7 = 1 THEN COLOR 11
IF INT(TIMER * 17) MOD 7 = 2 THEN COLOR 12
IF INT(TIMER * 17) MOD 7 = 3 THEN COLOR 13
IF INT(TIMER * 17) MOD 7 = 4 THEN COLOR 14
IF INT(TIMER * 17) MOD 7 = 5 THEN COLOR 15
IF INT(TIMER * 17) MOD 7 = 6 THEN COLOR 2
LOCATE ax, by: PRINT CHR$(2)
IF UCASE$(D$) = "O" THEN LS = LS + 1: GOTO E: GOTO AA
COLOR 10, 9
IF D$ <> "" THEN t = 0
IF l$ = "A" OR oz$ = "A" THEN LOCATE 30, 1, 0: COLOR 15, 9: PRINT "Level"; LS; "(Auto)"; : COLOR 10: PRINT " Б "; : FOR Z = 1 TO live: COLOR 14: PRINT CHR$(1); : NEXT: FOR Z = INT(live) TO OAS - 1: COLOR 4: PRINT "ў"; : NEXT: FOR Z = OAS TO 5: COLOR _
 4: PRINT "ў"; : NEXT: COLOR 10, 9: PRINT " А "; : FOR SD = 1 TO re: COLOR 12: PRINT CHR$(3); : NEXT: FOR SSD = re TO 3: COLOR 4: PRINT "ў"; : NEXT: COLOR 10, 9: PRINT " "; : oz$ = "A"
IF oz$ <> "A" THEN LOCATE 30, 1, 0: COLOR 15, 9: PRINT "Level "; l$; "                      ";
IF D$ = "" THEN GOTO AA
LOCATE x, y: PRINT " "
IF D$ = CHR$(0) + CHR$(77) THEN y = y + 1: GOSUB chk: IF (s$(x, y) = "л" OR s$(x, y) = "O" OR s$(x, y) = "?" OR s$(x, y) = "м" OR s$(x, y) = "п" OR s$(x, y) = "*" OR s$(x, y) = "}" OR s$(x, y) = "{") THEN GOTO EE ELSE IF s$(x, y) = CHR$(2) THEN GOTO _
 E:  ELSE GOTO AAO
IF D$ = CHR$(0) + CHR$(75) THEN y = y - 1: GOSUB chk: IF (s$(x, y) = "л" OR s$(x, y) = "O" OR s$(x, y) = "?" OR s$(x, y) = "м" OR s$(x, y) = "п" OR s$(x, y) = "*" OR s$(x, y) = "}" OR s$(x, y) = "{") THEN GOTO EE ELSE IF s$(x, y) = CHR$(2) THEN GOTO _
 E:  ELSE GOTO AAO
IF D$ = CHR$(0) + CHR$(80) THEN x = x + 1: GOSUB chk: IF (s$(x, y) = "л" OR s$(x, y) = "O" OR s$(x, y) = "?" OR s$(x, y) = "м" OR s$(x, y) = "п" OR s$(x, y) = "*" OR s$(x, y) = "}" OR s$(x, y) = "{") THEN GOTO EE ELSE IF s$(x, y) = CHR$(2) THEN GOTO _
 E:  ELSE GOTO AAO
IF D$ = CHR$(0) + CHR$(72) THEN x = x - 1: GOSUB chk: IF (s$(x, y) = "л" OR s$(x, y) = "O" OR s$(x, y) = "?" OR s$(x, y) = "м" OR s$(x, y) = "п" OR s$(x, y) = "*" OR s$(x, y) = "}" OR s$(x, y) = "{") THEN GOTO EE ELSE IF s$(x, y) = CHR$(2) THEN GOTO _
 E:  ELSE GOTO AAO
AAO:
IF D$ = CHR$(27) THEN RUN'CLEAR : CLS : GOTO rerun
'LOCATE 30, 40: PRINT s$(X, Y);
IF INSTR(ohno$, s$(x, y)) <> 0 THEN GOTO EE

GOTO AA
QW:
SLEEP 1
GOTO rerun
OO:

SHELL "CLS": SYSTEM
EE:
PASS = 1
getkeys
PLAY "<<<<A30B30G30D30>>>>"
SLEEP 1
IF live < 1 THEN GOTO er:  ELSE PLAY "<<<A60C60B60G60>>>": live = INT(live - 1): LS = LS - 1: re = 0: hold = 1: GOTO rerun
er:
getkeys
notdone
CLEAR
RUN
EEE:
RETURN
E:
IF D$ = "N" THEN GOTO rerun
LOCATE x, y: PRINT a$;
COLOR , 0
CLS
COLOR 15
LOCATE x + 2, y
PRINT "Wait."
COLOR 12
LOCATE x + 1, y
PRINT CHR$(3); " "; CHR$(3); " "; CHR$(3)
LOCATE x - 1, y
PRINT CHR$(3); " "; CHR$(3); " "; CHR$(3)
LOCATE x, y
PRINT CHR$(3);
COLOR 11
PRINT CHR$(1);
COLOR 14
PRINT " "; CHR$(2);
COLOR 12: PRINT CHR$(3)
PLAY "<<<G10F10E10a>>>"
FOR rtr = 1 TO 4
SLEEP 1
getkeys
NEXT
GOSUB EEE
getkeys
GOTO rerun
chk:
IF x > 29 OR x < 1 OR y > 80 OR y < 1 THEN s$(x, y) = "O"
RETURN

SUB cadr (x1, y1, x2, y2, dly, colr)
x1 = x1 + 7
y1 = y1 + 7
x2 = x2 - 8
y2 = y2 - 8
LINE (x1, y1)-(x2, y2), colr, B
FOR i = 2 TO 5
j = i
gg = INT(i * 6.3 * 2)
IF colr = 9 THEN setrgbpalette j, 0, 0, gg
IF colr = 10 THEN setrgbpalette j, 0, gg, 0
IF colr = 11 THEN setrgbpalette j, 0, gg, gg
IF colr = 12 THEN setrgbpalette j, gg, 0, 0
IF colr = 13 THEN setrgbpalette j, gg, 0, gg
IF colr = 14 THEN setrgbpalette j, gg, gg, 0
IF colr = 15 THEN setrgbpalette j, gg, gg, gg
LINE (x1 - i - 1, y1 - i - 1)-(x2 + i + 1, y2 + i + 1), j, B
NEXT

END SUB

SUB CIRCLEs (x, y, R, c, dly)
CIRCLE (x, y), R, c
FOR t = 1 TO 4 STEP 1
IF c = 10 THEN setrgbpalette t, 0, INT(t * 6.3 * 2), 0
IF c = 11 THEN setrgbpalette t, 0, INT(t * 6.3 * 2), INT(t * 6.3 * 2)
IF c = 12 THEN setrgbpalette t, INT(t * 6.3 * 2), 0, 0
IF c = 13 THEN setrgbpalette t, INT(t * 6.3 * 2), 0, INT(t * 6.3 * 2)
IF c = 14 THEN setrgbpalette t, INT(t * 6.3 * 2), INT(t * 6.3 * 2), 0
IF c = 15 THEN setrgbpalette t, INT(t * 6.3 * 2), INT(t * 6.3 * 2), INT(t * 6.3 * 2)
CIRCLE (x, y), R + t + 1, t
delay (dly)
NEXT
END SUB

SUB ctr (y, TEXT$)
N = LEN(TEXT$)
IF N > 78 THEN TEXT$ = LEFT$(TEXT$, 78): N = LEN(TEXT$)
m = (80 - N) \ 2
LOCATE y, 1: FOR g = 1 TO m
x = x + 1
NEXT g
LOCATE y, x
PRINT TEXT$
END SUB

FUNCTION date2$
mounth = VAL(DATE$)
tgt = INSTR(DATE$, "-")
day = VAL(MID$(DATE$, tgt + 1))
tgt2 = INSTR(MID$(DATE$, tgt + 1), "-")
year = VAL(MID$(MID$(DATE$, tgt + 1), tgt2 + 1))
y$ = str2$(year)
m$ = str2$(mounth)
D$ = str2$(day)
IF LEN(D$) < 2 THEN D$ = "0" + D$
IF LEN(m$) < 2 THEN m$ = "0" + m$
date2$ = y$ + "/" + m$ + "/" + D$
END FUNCTION

SUB delay (x)
SOUND 0, x
END SUB

SUB DEMO
CLS
WIDTH 80, 30
VIEW PRINT 1 TO 30
LOCATE 30, 1
COLOR 10: PRINT "Wait for DEMO...";
PLAY " Mf a60b60g60d60 Mf"
DIM Ss$(29, 81)
CLS
live = 1
a:
x = 2
y = 2
LS = 1
PRINT "ллллллллллллллллллллллллллллллллллл?}*{л"
PRINT "л    "; CHR$(3); "                                "; CHR$(2); "л"
PRINT "лллллллллллллллллллллллллллллллллллллллл"
'LOCATE 23, 1: COLOR 15, 9: PRINT "Level 1 (Auto)"
GOTO startdemo '*************************************************************
b:
CLS
x = 4
y = 2
PRINT "лллллллллл}*?ллллллллллллллллллллллллллл"
PRINT "л"; CHR$(2); "                                    лл"
PRINT "лллллллллллллллллллллллллллллллллллл  лл"
PRINT "л           "; CHR$(3); "                         лл"
PRINT "лллллллллллллллллллллллллллллллллллллллл"
LS = 2
GOTO startdemo '*************************************************************
startdemo:
getkeys
FOR a = 1 TO 25
FOR b = 1 TO 80
Ss$(a, b) = CHR$(SCREEN(a, b))
NEXT b, a
COLOR , 0: CLS
a$ = CHR$(1)
FOR a = 1 TO 25
FOR b = 1 TO 80
Q$ = Ss$(a, b)
COLOR 15, 0
IF Q$ = "*" OR Q$ = "<" THEN COLOR 12
IF Q$ = ">" OR Q$ = "<" THEN COLOR 10
IF Q$ = "{" OR Q$ = "}" THEN COLOR 13
IF Q$ = "O" OR Q$ = "0" OR Q$ = "л" OR Q$ = "o" THEN COLOR 14
IF Q$ = "м" OR Q$ = "п" THEN COLOR 11
IF Q$ = "?" THEN COLOR 13
IF Q$ = ":" OR Q$ = "+" THEN COLOR 2
IF Q$ = "&" THEN COLOR 2
IF Q$ = "H" OR Q$ = "h" THEN COLOR 2
IF Q$ = "I" OR Q$ = "1" THEN COLOR 2
IF Q$ = "I" OR Q$ = "1" THEN COLOR 2
IF Q$ = "о" OR Q$ = "н" THEN COLOR 14
IF Q$ = CHR$(3) THEN COLOR 12
IF Q$ = CHR$(2) THEN COLOR 10
LOCATE a, b: PRINT Ss$(a, b);
NEXT b, a
IF LS = 1 THEN
FOR y = 2 TO 39
SOUND 0, 1
IF y = 6 THEN re = re + 1
IF CHR$(SCREEN(2, y)) = CHR$(3) AND t = 0 THEN live = live + .25: re = re + 1: t = 1: PLAY "MB B60D60 MB": IF live = INT(live) THEN PLAY "MB A60G60C60F60 MB": re = 0
LOCATE 30, 1
PRINT "Level"; LS; "(Demo)"; : COLOR 10: PRINT " Б "; : FOR Z = 1 TO live: COLOR 14: PRINT CHR$(1); : NEXT: FOR Z = INT(live) TO 5: COLOR 4: PRINT "ў"; : NEXT: COLOR 10, 9: PRINT " А "; : FOR SD = 1 TO re: COLOR 12: PRINT CHR$(3); : NEXT: FOR SSD =  _
re TO 3: COLOR 4: PRINT "ў"; : NEXT: COLOR 10, 9: PRINT " "; : oz$ = "A"
COLOR 10, 9
LOCATE 2, y: PRINT " "; CHR$(1)
COLOR 15, 9
IF Ss$(2, y) = CHR$(2) THEN GOTO ew
NEXT

ELSE

FOR y = 2 TO 36
SOUND 0, 1
IF y = 13 THEN re = re + 1: er = er + 1
LOCATE 30, 1
COLOR 15
PRINT "Level"; LS; "(Demo)"; : COLOR 10: PRINT " Б "; : FOR Z = 1 TO live: COLOR 14: PRINT CHR$(1); : NEXT: FOR Z = INT(live) TO 5: COLOR 4: PRINT "ў"; : NEXT: COLOR 10, 9: PRINT " А "; : FOR SD = 1 TO re: COLOR 12: PRINT CHR$(3); : NEXT: FOR SSD =  _
re TO 3: COLOR 4: PRINT "ў"; : NEXT: COLOR 10, 9: PRINT " "; : oz$ = "A"
COLOR 10, 9
LOCATE 4, y: PRINT " "; CHR$(1)
COLOR 15, 9
IF CHR$(SCREEN(x, y)) = CHR$(3) AND t = 0 THEN live = live + .25: re = re + 1: t = 1: PLAY "MB B60D60 MB": IF live = INT(live) THEN PLAY "MB A60G60C60F60 MB": re = 0
NEXT
xx = x
x = y
yo = 4
xo = x
SOUND 0, 2
FOR yk = 2 TO 5
COLOR 10
y = 7 - yk
LOCATE y, 37
PRINT CHR$(1)
LOCATE yo, 37: PRINT " "
LOCATE 5, 37: COLOR 14: PRINT "л"
yo = y
xo = x
SOUND 0, 2
'END
NEXT
x = xx
FOR y = 1 TO 37
LOCATE 30, 1
COLOR 15
PRINT "Level"; LS; "(Demo)"; : COLOR 10: PRINT " Б "; : FOR Z = 1 TO live: COLOR 14: PRINT CHR$(1); : NEXT: FOR Z = INT(live) TO 5: COLOR 4: PRINT "ў"; : NEXT: COLOR 10, 9: PRINT " А "; : FOR SD = 1 TO re: COLOR 12, 2: PRINT CHR$(3); : NEXT: FOR SSD _
 = re TO 3: COLOR 4: PRINT "ў"; : NEXT: COLOR 10, 9: PRINT " "; : oz$ = "A"
COLOR 10, 9: IF y <> 2 AND y <> 1 THEN LOCATE 2, 38 - y: PRINT CHR$(1); "  "
SOUND 0, 1
IF y = 2 THEN LOCATE 2, 37: PRINT CHR$(1);
IF CHR$(SCREEN(x, y)) = CHR$(3) AND t = 0 THEN live = live + .25: re = re + 1: t = 1: PLAY "MB B60D60 MB": IF live = INT(live) THEN PLAY "MB A60G60C60F60 MB": re = 0
IF 38 - y = 2 THEN GOTO ew
NEXT

END IF
ew:
IF LS = 1 THEN GOTO b ELSE SLEEP 1': GOTO rerun
getkeys
END SUB

SUB drawadamak (vv)
COLOR 1
LOCATE 1, 1: PRINT "Adamak!"
IF vv > 1 THEN
LOCATE 2, 2
IF vv = 2 THEN COLOR 12: PRINT " "; CHR$(3); CHR$(3); CHR$(3)
IF vv = 3 THEN COLOR 12: PRINT " !!!"
ELSE
LOCATE 3, 2
COLOR 12: PRINT " "; CHR$(3); CHR$(3); CHR$(3)
END IF
'IF vv <> 1 THEN END
IF vv = 1 THEN ky = 23 ELSE ky = 100
FOR x = 0 TO 70 'STEP .7
FOR y = 0 TO ky 'STEP .7
p = POINT(x, y)
IF p > 0 THEN
IF y <= 37 THEN
IF vv < 2 THEN CIRCLE (x * 10 + 50, y * 10 + 200), 7, p, , , 1.5: PAINT (x * 10 + 50, y * 10 + 200), p
IF vv >= 2 THEN CIRCLE (x * 10 + 50, y * 10 + 150), 7, p, , , 1.5: PAINT (x * 10 + 50, y * 10 + 150), p
END IF
'IF y > 7 THEN
'CIRCLE ((x * 10 + 30) - 5, (y * 10 + 200) - 5), 2, p
'CIRCLE ((x * 10 + 30) + 5, (y * 10 + 200) + 5), 2, p
'CIRCLE ((x * 10 + 30) + 5, (y * 10 + 200) - 5), 2, p
'CIRCLE ((x * 10 + 30) - 5, (y * 10 + 200) + 5), 2, p
'PAINT ((x * 10 + 30) - 5, (y * 10 + 200) - 5), p
'PAINT ((x * 10 + 30) + 5, (y * 10 + 200) + 5), p
'PAINT ((x * 10 + 30) + 5, (y * 10 + 200) - 5), p
'PAINT ((x * 10 + 30) - 5, (y * 10 + 200) + 5), p
'
'LINE ((x * 10 + 30) - 5, (y * 10 + 200) - 5)-((x * 10 + 30) + 5, (y * 10 + 200) + 5), p, BF
'END IF
END IF
NEXT
NEXT
LOCATE 1, 1: PRINT "       "
LOCATE 2, 1: PRINT "       "
LOCATE 3, 1: PRINT "       "
CIRCLE (320 + 220, 370), 70, 14, , , 1.4
PAINT (320 + 220, 370), 14
CIRCLE (100, 370), 70, 14, , , 1.4
FOR g = 1 TO 2
IF g = 2 THEN y = 440 ELSE y = 0
CIRCLE (y + 120, 330), 5, 9
CIRCLE (y + 80, 330), 5, 9
PAINT (y + 120, 330), 9
PAINT (y + 80, 330), 9
pi = 3.1415
CIRCLE (y + 100, 365), 15, 10, , , 2
PAINT (y + 100, 365), 10
FOR tt = 0 TO 4
IF vv <> 3 THEN
CIRCLE (y + 100, 390 + tt), 20, 12, pi * (8 / 8), pi * (0 / 8)
ELSE
CIRCLE (y + 100, 390 + tt + 25), 20, 12, pi * (0 / 8), pi * (8 / 8)
END IF
NEXT
NEXT
END SUB

SUB ender
SCREEN 12
SCREEN 0
VIEW PRINT 1 TO 25
COLOR 10
LOCATE 25, 1: PRINT "Please Wait for Exit...";
PLAY "MF >a20b20<"
CLS
COLOR 9
 ctr 9, "ллВВББААББВВлл"
ctr 10, "ВВ          ВВ"
ctr 11, "ББ          ББ"
ctr 12, "ААББВВллВВББАА"
COLOR 10
ctr 10, " Good Bye!"
COLOR 12
LOCATE , , 1
ctr 11, "Hit a key"
LOCATE 11, 44
getkeys
DO
kj = kj + 1
 COLOR 9
IF kj MOD 6 = 0 THEN
 ctr 9, "ллВВББААББВВлл"
ctr 10, "ВВ          ВВ"
ctr 11, "ББ          ББ"
ctr 12, "ААББВВллВВББАА"
END IF
IF kj MOD 6 = 1 THEN
 ctr 9, "ВВллВВББААББВВ"
ctr 10, "ББ          лл"
ctr 11, "АА          ВВ"
ctr 12, "ББВВллВВББААББ"
END IF
IF kj MOD 6 = 2 THEN
 ctr 9, "ББВВллВВББААББ"
ctr 10, "АА          ВВ"
ctr 11, "ББ          лл"
ctr 12, "ВВллВВББААББВВ"
END IF
IF kj MOD 6 = 3 THEN
 ctr 9, "ААББВВллВВББАА"
ctr 10, "ББ          ББ"
ctr 11, "ВВ          ВВ"
ctr 12, "ллВВББААББВВлл"
END IF
IF kj MOD 6 = 4 THEN
 ctr 9, "ББААББВВллВВББ"
ctr 10, "ВВ          АА"
ctr 11, "лл          ББ"
ctr 12, "ВВББААББВВллВВ"
END IF
IF kj MOD 6 = 5 THEN
 ctr 9, "ВВББААББВВллВВ"
ctr 10, "лл          ББ"
ctr 11, "ВВ          АА"
ctr 12, "ББААББВВллВВББ"
END IF
COLOR 10
ctr 10, " Good Bye!"
COLOR 12
LOCATE , , 1
ctr 11, "Hit a key"
LOCATE 11, 44
SOUND 0, 2
LOOP WHILE INKEY$ = ""
LOCATE , , 0
COLOR 7: CLS
SYSTEM
END SUB

SUB getkeys
DO WHILE INKEY$ <> "": LOOP
END SUB

SUB notdone
SCREEN 0: SCREEN 12
drawadamak 3
cadr 130, 50, 320 + 190, 95, 0, 12
ctr 5, " Game Over! Hit any key..."
FOR gg = -2 TO 5
LINE (35 + gg, 300)-(165 + gg, 450), 12
LINE (165 + gg, 300)-(35 + gg, 450), 12
NEXT
SOUND 100, 10
SOUND 40, 10
SOUND 100, 10
SOUND 50, 10
SOUND 100, 10
SOUND 80, 10
SOUND 70, 10
getkeys
a$ = INPUT$(1)
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

FUNCTION ohno$
ohno$ = "ГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯабвгдежзийк"
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

SUB setrgbpalette (x, R, g, b)
PALETTE x, b * 65536 + g * 256 + R
END SUB

FUNCTION str2$ (x)
str2$ = LTRIM$(RTRIM$(STR$(x)))
END FUNCTION

FUNCTION time2$
hour = VAL(TIME$)
IF hour > 12 THEN hour = hour - 12: apm$ = "PM" ELSE apm$ = "AM"
IF hour = 12 THEN apm$ = "PM"
tgt = INSTR(TIME$, ":")
time2$ = LTRIM$(RTRIM$(STR$(hour))) + MID$(TIME$, tgt) + " " + apm$
END FUNCTION

