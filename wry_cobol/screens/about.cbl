       01  ABOUT-SCREEN.
           05 LINE 1.
           05 COL 1, VALUE '                 ABOUT'.
           05 LINE + 1.
           05 COL 1, VALUE '                -------'.
           05 LINE + 1.
           05 COL 1,VALUE '   Wry is a game I originally wrote back in'.
           05 LINE + 1.
           05 COL 1, VALUE
           '1999 and was finished in early 2000. Episode one, '.
           05 LINE + 1.
           05 COL 1, VALUE
           'three and four were written by me and episode two '.
           05 LINE + 1.
           05 COL 1, VALUE
           'was written by my friend who went under the fake '.
           05 LINE + 1.
           05 COL 1, VALUE
           'company name of Sim Creations. None of the stories'.
           05 LINE + 1.
           05 COL 1, VALUE
           'are any good but I needed something to make a '.
           05 LINE + 1.
           05 COL 1, VALUE
           'COBOL application for and Wry seemed like the '.
           05 LINE + 1.
           05 COL 1, VALUE
           'perfect target. It was my first completed large '.
           05 LINE + 1.
           05 COL 1, VALUE
           'Qbasic project and I feel like its fitting that '.
           05 LINE + 1.
           05 COL 1, VALUE
           'it is also my first completed COBOL project as '.
           05 LINE + 1.
           05 COL 1, VALUE
           'well. (Especially seeing that Wry was supposed to '.
           05 LINE + 1.
           05 COL 1, VALUE
           'get a Gold edition for it back in the day that '.
           05 LINE + 1.
           05 COL 1, VALUE
           'would have been a code rewrite of the stories that'.
           05 LINE + 1.
           05 COL 1, VALUE
           'didnt crash due to stack overflow.)'.
           05 LINE + 2.
           05 COL 1, VALUE
           '  None of the typos have been fixed from the original game'.
           05 LINE + 1.
           05 COL 1, VALUE
           'and unfortunately all of the interesting invalid '.
           05 LINE + 1.
           05 COL 1, VALUE
           'selection responses have been left out. Beating any of the'.
           05 LINE + 1.
           05 COL 1, VALUE
           'first three episodes will unlock the fourth one.'.
           05 LINE + 2.
           05 COL 1, VALUE
           '  The data files used for this game were generated using a'.
           05 LINE + 1.
           05 COL 1, VALUE
           'Java application I wrote called wry-parser that parses the'.
           05 LINE + 1.
           05 COL 1, VALUE
           'original Qbasic source file into usable objects that can'.
           05 LINE + 1.
           05 COL 1, VALUE
           'then outputted to different types of formats. So this is a'.
           05 LINE + 1.
           05 COL 1, VALUE
           'Qbasic game that was parsed by a Java application and'.
           05 LINE + 1.
           05 COL 1, VALUE
           'output to data files read by a COBOL application.'.
           05 LINE + 1.
           05 COL 1, VALUE
           'Completely unnecessary but it was still fun to make. :)'.
           05 LINE + 3.
           05 COL 5, VALUE 'Press enter to return to the main menu.'.


           05 RESPONSE-INPUT PIC X(1) TO WS-ABOUT-INPUT.
