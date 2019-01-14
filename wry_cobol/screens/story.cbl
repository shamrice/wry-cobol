       01  STORY-SCREEN.
           05 LINE 1.

           05 COL 1, VALUE 'STORY START RECORD ID: '.
           05 START-ID PIC 9(3) FROM WS-STORY-START-STORY-ID.
           05 LINE + 1.

           05 COL 1, VALUE 'STORY START EPISODE RECORD ID: '.
           05 START-EPIOSDOE-ID PIC 9(1) FROM WS-STORY-START-EPISODE-ID.
           05 LINE + 1.



           05 COL 1, VALUE 'CURRENT EPISODE: '.
           05 EPISODE-ID PIC 9(1) FROM WS-EPISODE-ID.
           05 LINE + 1.

           05 COL 1, VALUE 'CURRENT RECORD: '.
           05 CURRENT-RECORD PIC 9(3) FROM WS-CURRENT-RECORD.
           05 LINE + 1.

           05 COL 1, VALUE 'STORY ID: '.
           05 STORY-ID  PIC 9(3) FROM WS-STORY-ID.
           05 LINE + 1.

           05 COL 1, VALUE 'STORY TEXT ID: '.
           05 STORY-TEXT-ID PIC 9(3) FROM WS-STORY-TEXT-ID.
           05 LINE + 1.



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
