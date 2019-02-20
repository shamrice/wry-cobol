       01  EPISODE-MENU-SCREEN.
           05 VALUE '                    Nukem Enterprises'.
           05 LINE + 1.
           05 LINE + 1.
           05 COL 1,
       VALUE '####              *####   #########      ###      ###'.
           05 LINE + 1.
           05 COL 1,
           VALUE '*####            *####   *###*****###    *###   *###'.
           05 LINE + 1.
           05 COL 1,
           VALUE ' *####          *####    *###    *###     *### *###'.
           05 LINE + 1.
           05 COL 1,
           VALUE '  *####        *####     *#########        *#####'.
           05 LINE + 1.
           05 COL 1,
           VALUE '   *####  *#  *####      *###    *###       *###'.
           05 LINE + 1.
           05 COL 1,
           VALUE '    *####*###*####       *###     *###     *###'.
           05 LINE + 1.
           05 COL 1,
           VALUE '     *#####*#####        *###      *###   *###'.
           05 LINE + 1.
           05 COL 1,
           VALUE '      *###  *###         ***       ***    ***'.
           05 LINE + 1.
           05 LINE + 1.
           05 COL 1,
           VALUE '                 C    O    B    O    L'.
           05 LINE + 2.
           05 COL 1
           VALUE '                   SELECT AN EPISODE'.
           05 LINE + 1.
           05 COL 1
           VALUE '                  1) Wry Humor'.
           05 LINE + 1.
           05 COL 1
           VALUE '                  2) A Spy Adventure'.
           05 LINE + 1.
           05 COL 1
           VALUE '                  3) Menal Condition'.
           05 LINE + 1.
           05 COL 1
           VALUE '                  4) Bewildered (UNLOCKED)'.
           05 LINE + 2.
           05 COL 20, VALUE 'Selection: '.
           05 RESPONSE-INPUT PIC 9 TO WS-EP-MENU-INPUT.
