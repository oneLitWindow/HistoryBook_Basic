DECLARE SUB ctr (Y%, text$)
DEFINT A-Z
DECLARE FUNCTION RandInt% (lower, Upper)
DECLARE SUB BoxInit ()
DECLARE SUB BubbleSort ()
DECLARE SUB CheckScreen ()
DECLARE SUB DrawFrame (TopSide, BottomSide, LeftSide, RightSide)
DECLARE SUB ElapsedTime (CurrentRow)
DECLARE SUB ExchangeSort ()
DECLARE SUB HeapSort ()
DECLARE SUB Initialize ()
DECLARE SUB InsertionSort ()
DECLARE SUB PercolateDown (MaxLevel)
DECLARE SUB PercolateUp (MaxLevel)
DECLARE SUB PrintOneBar (Row)
DECLARE SUB QuickSort (Low, High)
DECLARE SUB Reinitialize ()
DECLARE SUB ShellSort ()
DECLARE SUB SortMenu ()
DECLARE SUB SwapBars (Row1, Row2)
DECLARE SUB ToggleSound (Row, Column)
TYPE SortType
Length AS INTEGER

ColorVal AS INTEGER
BarString AS STRING * 43
END TYPE
CONST FALSE = 0, TRUE = NOT FALSE, LEFTCOLUMN = 49
CONST NUMOPTIONS = 11, NUMSORTS = 6
DIM SHARED SortArray(1 TO 43) AS SortType, SortBackup(1 TO 43) AS SortType
DIM SHARED OptionTitle(1 TO NUMOPTIONS) AS STRING * 12
DIM SHARED StartTime AS SINGLE
DIM SHARED foreground, Background, NoSound, Pause
DIM SHARED Selection, MaxRow, InitRow, maxcolors
LOCATE 1, 1, 1
CLS
COLOR 10
ctr 11, "Sorter By Amin Safinejhad"
COLOR 13
ctr 13, "Press any key to Start..."
WHILE INKEY$ = "": WEND
COLOR 15, 1
DATA Insertion, Bubble, Heap, Exchange, Shell, Quick,
DATA Toggle Sound, , A=All sorts,             ÿ
Initialize
SortMenu
WIDTH 80, InitRow
CLS
END
GetRow:
IF InitRow = 50 THEN
InitRow = 43
RESUME
ELSE
InitRow = 25
RESUME NEXT
END IF

MonoTrap:
maxcolors = 2
RESUME NEXT

RowTrap:
MaxRow = 25
RESUME

SUB BoxInit STATIC
DrawFrame 1, 22, LEFTCOLUMN - 3, 78

'  LOCATE 3, LEFTCOLUMN + 2: PRINT "SORTING DEMO";
COLOR 10 + 16, 1
LOCATE 3, LEFTCOLUMN + 2: PRINT "    Sorting Demo 2     ";
COLOR , 0
LOCATE 5
FOR I = 1 TO NUMOPTIONS - 1
LOCATE , LEFTCOLUMN: COLOR 14: PRINT OptionTitle(I)
NEXT I
IF Pause > 1 THEN LOCATE , LEFTCOLUMN: PRINT OptionTitle(NUMOPTIONS);
'NoSound = NOT NoSound
ToggleSound 12, LEFTCOLUMN + 12

LOCATE NUMOPTIONS + 6, LEFTCOLUMN
COLOR 11
PRINT "Type first character of"
LOCATE , LEFTCOLUMN
COLOR 10
PRINT "Choice ( I B H E S Q T A )"
LOCATE , LEFTCOLUMN
COLOR 13
PRINT "or ESC key to end program: ";
COLOR 14
END SUB

SUB BubbleSort STATIC
Limit = MaxRow
DO
Switch = FALSE
FOR Row = 1 TO (Limit - 1)
IF SortArray(Row).Length > SortArray(Row + 1).Length THEN
SWAP SortArray(Row), SortArray(Row + 1)
SwapBars Row, Row + 1
Switch = Row
END IF
NEXT Row
Limit = Switch
LOOP WHILE Switch
END SUB

SUB CheckScreen STATIC
InitRow = 50
ON ERROR GOTO GetRow
LOCATE InitRow, 1
maxcolors = 15
ON ERROR GOTO MonoTrap
SCREEN 1
SCREEN 0
MaxRow = 43
ON ERROR GOTO RowTrap
WIDTH 80, MaxRow
ON ERROR GOTO 0
END SUB

SUB ctr (Y, text$)
ON ERROR RESUME NEXT
N = LEN(text$)
IF N > 78 THEN text$ = LEFT$(text$, 78): N = LEN(text$)
M = (80 - N) \ 2
LOCATE Y, 1: FOR G = 1 TO M
X = X + 1
NEXT G
LOCATE Y, X
PRINT text$;
END SUB

SUB DrawFrame (TopSide, BottomSide, LeftSide, RightSide) STATIC
COLOR 15
CONST ULEFT = 201, URIGHT = 187, LLEFT = 200, LRIGHT = 188
CONST VERTICAL = 186, HORIZONTAL = 205

FrameWidth = RightSide - LeftSide - 1
LOCATE TopSide, LeftSide
PRINT CHR$(ULEFT); STRING$(FrameWidth, HORIZONTAL); CHR$(URIGHT);
FOR Row = TopSide + 1 TO BottomSide - 1
LOCATE Row, LeftSide
PRINT CHR$(VERTICAL); SPC(FrameWidth); CHR$(VERTICAL);
NEXT Row
LOCATE BottomSide, LeftSide
PRINT CHR$(LLEFT); STRING$(FrameWidth, HORIZONTAL); CHR$(LRIGHT);
END SUB

SUB ElapsedTime (CurrentRow) STATIC
IF INKEY$ = CHR$(27) THEN END
CONST FORMAT = "  &###.### seconds  "
IF foreground = 1 THEN foreground = 0
COLOR foreground, Background
LOCATE Selection + 4, LEFTCOLUMN - 2
PRINT USING FORMAT; OptionTitle(Selection); TIMER - StartTime;
IF NoSound THEN
SOUND 30000, Pause
ELSE
SOUND 60 * CurrentRow, Pause
END IF
IF maxcolors = 1 THEN maxcolors = 9
COLOR maxcolors, 0

END SUB

SUB ExchangeSort STATIC
FOR Row = 1 TO MaxRow
SmallestRow = Row
FOR J = Row + 1 TO MaxRow
IF SortArray(J).Length < SortArray(SmallestRow).Length THEN
SmallestRow = J
ElapsedTime J
END IF
NEXT J
IF SmallestRow > Row THEN
SWAP SortArray(Row), SortArray(SmallestRow)
SwapBars Row, SmallestRow
END IF
NEXT Row
END SUB

' =============================== HeapSort ===================================
'  The HeapSort procedure works by calling two other procedures - PercolateUp
'  and PercolateDown.  PercolateUp turns SortArray into a "heap," which has
'  the properties outlined in the diagram below:
'
'                               SortArray(1)
'                               /          \
'                    SortArray(2)           SortArray(3)
'                   /          \            /          \
'         SortArray(4)   SortArray(5)   SortArray(6)  SortArray(7)
'          /      \       /       \       /      \      /      \
'        ...      ...   ...       ...   ...      ...  ...      ...
'
'
'  where each "parent node" is greater than each of its "child nodes"; for
'  example, SortArray(1) is greater than SortArray(2) or SortArray(3),
'  SortArray(3) is greater than SortArray(6) or SortArray(7), and so forth.
'
'  Therefore, once the first FOR...NEXT loop in HeapSort is finished, the
'  largest element is in SortArray(1).
'
'  The second FOR...NEXT loop in HeapSort swaps the element in SortArray(1)
'  with the element in MaxRow, rebuilds the heap (with PercolateDown) for
'  MaxRow - 1, then swaps the element in SortArray(1) with the element in
'  MaxRow - 1, rebuilds the heap for MaxRow - 2, and continues in this way
'  until the array is sorted.
' ============================================================================
'
SUB HeapSort STATIC
FOR I = 2 TO MaxRow
PercolateUp I
NEXT I

FOR I = MaxRow TO 2 STEP -1
SWAP SortArray(1), SortArray(I)
SwapBars 1, I
PercolateDown I - 1
NEXT I
END SUB

SUB Initialize STATIC
DIM TempArray(1 TO 43)

CheckScreen

FOR I = 1 TO MaxRow
TempArray(I) = I
NEXT I

MaxIndex = MaxRow

RANDOMIZE TIMER
FOR I = 1 TO MaxRow

Index = RandInt%(1, MaxIndex)
BarLength = TempArray(Index)
TempArray(Index) = TempArray(MaxIndex)
MaxIndex = MaxIndex - 1
SortBackup(I).Length = BarLength
SortBackup(I).BarString = STRING$(BarLength, 223)
IF maxcolors > 2 THEN
SortBackup(I).ColorVal = (BarLength MOD maxcolors) + 1
ELSE
SortBackup(I).ColorVal = maxcolors
END IF
NEXT I

FOR I = 1 TO NUMOPTIONS
READ OptionTitle(I)
NEXT I
COLOR 15, 1
CLS
Reinitialize

NoSound = FALSE
Pause = 2
BoxInit

END SUB

SUB InsertionSort STATIC
DIM TempVal AS SortType
FOR Row = 2 TO MaxRow
TempVal = SortArray(Row)
TempLength = TempVal.Length
FOR J = Row TO 2 STEP -1

IF SortArray(J - 1).Length > TempLength THEN
SortArray(J) = SortArray(J - 1)
PrintOneBar J
ElapsedTime J

ELSE
EXIT FOR
END IF
NEXT J

SortArray(J) = TempVal
PrintOneBar J
ElapsedTime J
NEXT Row
END SUB

SUB PercolateDown (MaxLevel) STATIC
I = 1
DO
Child = 2 * I
IF Child > MaxLevel THEN EXIT DO
IF Child + 1 <= MaxLevel THEN
IF SortArray(Child + 1).Length > SortArray(Child).Length THEN
Child = Child + 1
END IF
END IF
IF SortArray(I).Length < SortArray(Child).Length THEN
SWAP SortArray(I), SortArray(Child)
SwapBars I, Child
I = Child
ELSE
EXIT DO
END IF
LOOP
END SUB

SUB PercolateUp (MaxLevel) STATIC
I = MaxLevel
DO UNTIL I = 1
Parent = I \ 2
IF SortArray(I).Length > SortArray(Parent).Length THEN
SWAP SortArray(Parent), SortArray(I)
SwapBars Parent, I
I = Parent
ELSE
EXIT DO
END IF
LOOP
END SUB

SUB PrintOneBar (Row) STATIC
LOCATE Row, 1
IF SortArray(Row).ColorVal <> 1 THEN COLOR SortArray(Row).ColorVal ELSE COLOR 0
COLOR , 1
PRINT SortArray(Row).BarString;
END SUB

' ============================== QuickSort ===================================
'   QuickSort works by picking a random "pivot" element in SortArray, then
'   moving every element that is bigger to one side of the pivot, and every
'   element that is smaller to the other side.  QuickSort is then called
'   recursively with the two subdivisions created by the pivot.  Once the
'   number of elements in a subdivision reaches two, the recursive calls end
'   and the array is sorted.
' ============================================================================
'
SUB QuickSort (Low, High)
IF Low < High THEN
IF High - Low = 1 THEN
IF SortArray(Low).Length > SortArray(High).Length THEN
SWAP SortArray(Low), SortArray(High)
SwapBars Low, High
END IF
ELSE
RandIndex = RandInt%(Low, High)
SWAP SortArray(High), SortArray(RandIndex)
SwapBars High, RandIndex
Partition = SortArray(High).Length
DO

I = Low: J = High
DO WHILE (I < J) AND (SortArray(I).Length <= Partition)
I = I + 1
LOOP
DO WHILE (J > I) AND (SortArray(J).Length >= Partition)
J = J - 1
LOOP

IF I < J THEN
SWAP SortArray(I), SortArray(J)
SwapBars I, J
END IF
LOOP WHILE I < J

SWAP SortArray(I), SortArray(High)
SwapBars I, High

IF (I - Low) < (High - I) THEN
QuickSort Low, I - 1
QuickSort I + 1, High
ELSE
QuickSort I + 1, High
QuickSort Low, I - 1
END IF
END IF
END IF
END SUB

FUNCTION RandInt% (lower, Upper) STATIC
RandInt% = INT(RND * (Upper - lower + 1)) + lower
END FUNCTION

SUB Reinitialize STATIC
FOR I = 1 TO MaxRow
SortArray(I) = SortBackup(I)
NEXT I

FOR I = 1 TO MaxRow
LOCATE I, 1
IF SortArray(I).ColorVal <> 1 THEN COLOR SortArray(I).ColorVal, 1 ELSE COLOR 0, 1
PRINT SortArray(I).BarString;
NEXT I

COLOR maxcolors, 0
END SUB

' =============================== ShellSort ==================================
'  The ShellSort procedure is similar to the BubbleSort procedure.  However,
'  ShellSort begins by comparing elements that are far apart (separated by
'  the value of the Offset variable, which is initially half the distance
'  between the first and last element), then comparing elements that are
'  closer together (when Offset is one, the last iteration of this procedure
'  is merely a bubble sort).
' ============================================================================
'
SUB ShellSort STATIC

Offset = MaxRow \ 2

DO WHILE Offset > 0
Limit = MaxRow - Offset
DO
Switch = FALSE

FOR Row = 1 TO Limit
IF SortArray(Row).Length > SortArray(Row + Offset).Length THEN
SWAP SortArray(Row), SortArray(Row + Offset)
SwapBars Row, Row + Offset
Switch = Row
END IF
NEXT Row

Limit = Switch - Offset
LOOP WHILE Switch

Offset = Offset \ 2
LOOP
END SUB

SUB SortMenu STATIC
myk = -1
2
Escape$ = CHR$(27)

Option$ = "IBHESQA<T" + Escape$
DO
LOCATE NUMOPTIONS + 8, LEFTCOLUMN + 27, 1
IF myk = -1 THEN choice$ = UCASE$(INPUT$(1)) ELSE choice$ = MID$("IBHESQ", (myk MOD 6) + 1, 1): myk = myk + 1
Selection = INSTR(Option$, choice$)
IF choice$ = "A" THEN myk = myk + 1
IF (Selection >= 1) AND (Selection <= NUMSORTS) THEN
Reinitialize
LOCATE , , 0
foreground = 14
Background = 1
StartTime = TIMER
END IF

SELECT CASE choice$
CASE "I"
InsertionSort
CASE "B"
BubbleSort
CASE "H"
HeapSort
CASE "E"
ExchangeSort
CASE "S"
ShellSort
CASE "Q"
QuickSort 1, MaxRow
'CASE ">"
'
'Pause = (2 * Pause) / 3
'BoxInit
'
'CASE "<"
'
'Pause = (3 * Pause) / 2
'BoxInit

CASE "T"
ToggleSound 12, LEFTCOLUMN + 12

CASE Escape$

EXIT DO

CASE ELSE

END SELECT

IF (Selection >= 1) AND (Selection <= NUMSORTS) THEN
foreground = maxcolors
Background = 0
ElapsedTime 0
END IF

LOOP
END SUB

SUB SwapBars (Row1, Row2) STATIC
PrintOneBar Row1
PrintOneBar Row2
ElapsedTime Row1
END SUB

SUB ToggleSound (Row, Column) STATIC
NoSound = NOT NoSound
LOCATE Row, Column
IF NoSound THEN
PRINT ": OFF";
ELSE
PRINT ": ON ";
END IF
END SUB

