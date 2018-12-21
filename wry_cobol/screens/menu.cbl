       01  MENU-SCREEN.
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
           05 LINE + 1.
           05 LINE + 1.
           05 COL 1
           VALUE '                      1) NEW GAME'.
           05 LINE + 1.
           05 COL 1
           VALUE '                      2) ABOUT'.
           05 LINE + 1.
           05 COL 1
           VALUE '                      3) QUIT'.
           05 LINE + 1.
           05 LINE + 1.
           05 COL 20, VALUE 'Selection: '.
           05 RESPONSE-INPUT PIC 9 TO WS-MENU-INPUT.
