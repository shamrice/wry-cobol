       01  STORY-SCREEN.
           05 LINE 1.
           05 STORY-OUTPUT PIC X(255) FROM WS-STORY-TEXT.
           05 LINE + 4.

           05 COL 4.
           05 CHOICE1-OUTPUT PIC X(255) FROM  WS-CHOICES-TEXT(1).
           05 LINE + 1.
           05 COL 4.
           05 CHOICE1-OUTPUT PIC X(255) FROM  WS-CHOICES-TEXT(2).
           05 LINE + 1.
           05 COL 4.
           05 CHOICE1-OUTPUT PIC X(255) FROM  WS-CHOICES-TEXT(3).
           05 LINE + 1.
           05 COL 4.
           05 CHOICE1-OUTPUT PIC X(255) FROM  WS-CHOICES-TEXT(4).

           05 LINE + 2.
           05 COL 2, VALUE 'Selection: '.
           05 RESPONSE-INPUT PIC 9(1) TO WS-STORY-INPUT.
