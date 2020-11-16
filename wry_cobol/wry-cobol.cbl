      *>*****************************************************************
      *> Author: Erik Eriksen
      *> Date: 12/16/2018 to 02/20/2019
      *> Purpose: Version of Wry written in COBOL
      *> Tectonics: cobc
      *>*****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. WRY-COBOL.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
           FILE-CONTROL.

               SELECT FD-STORY-START-FILE
                   ASSIGN TO './data/story-start.dat'
                   ORGANIZATION IS LINE SEQUENTIAL.

               SELECT FD-STORY-FILE
                   ASSIGN TO './data/story.dat'
                   ORGANIZATION IS LINE SEQUENTIAL.

               SELECT FD-STORY-TEXT-FILE
                   ASSIGN TO './data/story-text.dat'
                   ORGANIZATION IS LINE SEQUENTIAL.

               SELECT FD-STORY-CHOICE-FILE
                   ASSIGN TO './data/story-choice.dat'
                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
      *>****************************************************************
      *> File descriptors for input data files.
      *>****************************************************************
       FD  FD-STORY-START-FILE.
       01  FD-STORY-START-FILE-RECORD.
           05 STORY-START-EPISODE-ID           PIC 9(1).
           05 STORY-START-STORY-ID             PIC 9(3).

       FD  FD-STORY-FILE.
       01  FD-STORY-RECORD.
           05 EPISODE-ID                       PIC 9(1).
           05 STORY-ID                         PIC 9(3).

       FD  FD-STORY-TEXT-FILE.
       01  FD-STORY-TEXT-RECORD.
           05 STORY-TEXT-EPISODE-ID            PIC 9(1).
           05 STORY-TEXT-ID                    PIC 9(3).
           05 STORY-TEXT                       PIC X(1000).

       FD  FD-STORY-CHOICE-FILE.
       01  FD-STORY-CHOICE-RECORD.
           05 STORY-CHOICE-EPISODE-ID          PIC 9(1).
           05 STORY-CHOICE-STORY-ID            PIC 9(3).
           05 STORY-CHOICE-DEST-STORY-ID       PIC 9(3).
           05 STORY-CHOICE-ID                  PIC 9(1).
           05 STORY-CHOICE-TEXT                PIC X(255).

       WORKING-STORAGE SECTION.

      *>****************************************************************
      *> Level 77 variables.
      *>****************************************************************
       77  WS-DEBUG-PREFIX                     PIC X(5) VALUE 'echo'.
       77  WS-DEBUG-MSG                        PIC X(255).
       77  WS-DEBUG-CONCAT                     PIC X(300).

       77  WS-TITLE-INPUT                      PIC X.
       77  WS-MENU-INPUT                       PIC 9 VALUE 0.
       77  WS-EP-MENU-INPUT                    PIC 9 VALUE 0.
       77  WS-ABOUT-INPUT                      PIC X.
       77  WS-STORY-INPUT                      PIC 9(1).

       77  WS-CURRENT-EPISODE                  PIC 9(1).
       77  WS-CURRENT-RECORD                   PIC 9(3).

       77  WS-VALID-CHOICE                     PIC A(1) VALUE 'N'.

       77  WS-FINAL-EPISODE-UNLOCKED           PIC A(1) VALUE 'N'.

       77  WS-TAL-CTR                          PIC 9(2) VALUE 0.

      *>****************************************************************
      *> Conditional switches
      *>****************************************************************
       01  WS-GAMEOVER-SW                      PIC A(1) VALUE 'N'.
           88 WS-GAMEOVER                      VALUE 'Y'.

       01  WS-EOF-SW                           PIC X(1) VALUE 'N'.
           88 EOF-SW                           VALUE 'Y'.
           88 NOT-EOF-SW                       VALUE 'N'.

       01  WS-STORY-RECORD-FOUND               PIC A(1) VALUE 'N'.
           88 RECORD-FOUND                     VALUE 'Y'.
           88 RECORD-NOT-FOUND                 VALUE 'N'.

      *>****************************************************************
      *> Working storage variables for contents of data files
      *>****************************************************************
       01  WS-STORY-START-RECORD.
           05 WS-STORY-START-EPISODE-ID        PIC 9(1).
           05 WS-STORY-START-STORY-ID          PIC 9(3).

       01  WS-STORY-RECORD.
           88 EOF-STORY                        VALUE HIGH-VALUES.
           05 WS-EPISODE-ID                    PIC 9(1).
           05 WS-STORY-ID                      PIC 9(3).
           05 WS-CHOICES                       OCCURS 4 TIMES.
               10 WS-CHOICE-TEXT               PIC X(255).
               10 WS-CHOICE-DESTINATION        PIC 9(3).

       01  WS-STORY-TEXT-RECORD.
           05 WS-STORY-TEXT-EPISODE-ID         PIC 9(1).
           05 WS-STORY-TEXT-ID                 PIC 9(3).
           05 WS-STORY-TEXT                    PIC X(1000).

       01  WS-STORY-CHOICE-RECORD.
           05 WS-STORY-CHOICE-EPISODE-ID       PIC 9(1).
           05 WS-STORY-CHOICE-STORY-ID         PIC 9(3).
           05 WS-STORY-CHOICE-DEST-STORY-ID    PIC 9(3).
           05 WS-STORY-CHOICE-ID               PIC 9(1).
           05 WS-STORY-CHOICE-TEXT             PIC X(255).

      *>****************************************************************
      *> I/O screens used to dislay the various screens of the game
      *> and accept the input from each.
      *>****************************************************************
       SCREEN SECTION.

       COPY 'screens/blank.cbl'.
       COPY 'screens/title.cbl'.
       COPY 'screens/ep-menu.cbl'.
       COPY 'screens/ep-menu-locked.cbl'.
       COPY 'screens/menu.cbl'.
       COPY 'screens/about.cbl'.
       COPY 'screens/story.cbl'.


       PROCEDURE DIVISION.

      *>****************************************************************
      *> Main procedure is used to call the main menu. Once main menu
      *> returns here, the stop run is reached and the program exits.
      *>****************************************************************
       000-MAIN-PROCEDURE.

           ACCEPT TITLE-SCREEN

           PERFORM UNTIL WS-MENU-INPUT > 2
               PERFORM 100-MAIN-MENU
           END-PERFORM

           STOP RUN.

      *>****************************************************************
      *> Paragraph used to display debug messages to the terminal while
      *> the game is running.
      *>****************************************************************
       050-DEBUG-MESSAGE.
           STRING WS-DEBUG-PREFIX DELIMITED BY SPACE
               ' '   DELIMITED BY SIZE
               WS-DEBUG-MSG DELIMITED BY SIZE
               INTO WS-DEBUG-CONCAT
           END-STRING

           CALL 'SYSTEM' using WS-DEBUG-CONCAT.


      *>****************************************************************
      *> Displays main menu screens and handles inputs.
      *>
      *> New game will reset the story variables and display either the
      *> locked or unlocked episode select screen depending on if the
      *> player has completed an episode already.
      *> After the episode is selected, it will run the story until a
      *> game over is reached and reset as needed.
      *>
      *> About will call the paragraph to display the about information.
      *>
      *> When a 3 is entered to exit the game, the game will fall back
      *> to the main procedure paragraph and reach the stop run.
      *>****************************************************************
       100-MAIN-MENU.

           DISPLAY BLANK-SCREEN

           PERFORM UNTIL WS-MENU-INPUT = 3
           OR WS-MENU-INPUT = 2 OR WS-MENU-INPUT = 1
               ACCEPT MENU-SCREEN
           END-PERFORM

           IF WS-MENU-INPUT = 1
               PERFORM 105-RESET-STORY

               DISPLAY BLANK-SCREEN

               PERFORM UNTIL WS-EP-MENU-INPUT <= 4
               AND WS-EP-MENU-INPUT > 0
                   IF WS-FINAL-EPISODE-UNLOCKED = 'N' THEN
                       ACCEPT EPISODE-MENU-LOCKED-SCREEN
                       IF WS-EP-MENU-INPUT = 4 THEN
                           SET WS-EP-MENU-INPUT TO 9
                       END-IF
                   ELSE
                       ACCEPT EPISODE-MENU-SCREEN
                   END-IF

               END-PERFORM

               MOVE WS-EP-MENU-INPUT TO WS-CURRENT-EPISODE

               PERFORM 300-READ-STORY-START
               PERFORM 325-RUN-STORY

               PERFORM 105-RESET-STORY
               PERFORM 110-RESET-MENU-INPUT

           ELSE IF WS-MENU-INPUT = 2
               PERFORM 200-ABOUT
               PERFORM 110-RESET-MENU-INPUT
           END-IF.

      *>****************************************************************
      *> Resets the variables related to the episode and story as well
      *> as the menu input.
      *>****************************************************************
       105-RESET-STORY.
           MOVE 0 TO WS-EP-MENU-INPUT
           MOVE 0 TO WS-CURRENT-EPISODE
           MOVE 000 TO WS-CURRENT-RECORD
           MOVE 'N' TO WS-GAMEOVER-SW.

      *>****************************************************************
      *> Resets the menu input variable back to zero.
      *>****************************************************************
       110-RESET-MENU-INPUT.
           MOVE 0 TO WS-MENU-INPUT.

      *>****************************************************************
      *> Displays screen with the about game information.
      *>****************************************************************
       200-ABOUT.
           DISPLAY BLANK-SCREEN
           ACCEPT ABOUT-SCREEN.

      *>****************************************************************
      *> Initializes the story for the episode selected based on the
      *> the contents of the story start file.
      *>****************************************************************
       300-READ-STORY-START.

           OPEN INPUT FD-STORY-START-FILE
               PERFORM UNTIL EOF-SW
                   READ FD-STORY-START-FILE INTO WS-STORY-START-RECORD
                       AT END MOVE 'Y' TO WS-EOF-SW
                   NOT AT END
                       IF WS-STORY-START-EPISODE-ID = WS-CURRENT-EPISODE

                           MOVE 'setting start info' TO WS-DEBUG-MSG
                           PERFORM 050-DEBUG-MESSAGE

      *>   Probably more assignment here than necessary...
                           MOVE WS-STORY-START-STORY-ID
                               TO WS-CURRENT-RECORD

                           MOVE WS-STORY-START-STORY-ID
                               TO WS-STORY-ID

                           MOVE WS-STORY-START-STORY-ID
                               TO WS-STORY-TEXT-ID

                           MOVE WS-CURRENT-EPISODE TO
                               WS-STORY-TEXT-EPISODE-ID

                           MOVE WS-STORY-START-STORY-ID
                               TO WS-STORY-CHOICE-STORY-ID

                          MOVE WS-STORY-START-EPISODE-ID
                               TO WS-STORY-CHOICE-EPISODE-ID

                           ACCEPT BLANK-SCREEN

                       END-IF
                   END-READ
               END-PERFORM
           CLOSE FD-STORY-START-FILE
           MOVE 'N' TO WS-EOF-SW.

      *>****************************************************************
      *> Paragraph used to keep reading next pages in the story until
      *> the game over flag is set.
      *>****************************************************************
       325-RUN-STORY.
           PERFORM UNTIL WS-GAMEOVER-SW = 'Y'

               MOVE 'Running story' TO WS-DEBUG-MSG
               PERFORM 050-DEBUG-MESSAGE

               PERFORM 350-READ-STORY
           END-PERFORM.

      *>****************************************************************
      *> Builds each page of the story by calling paragraphs to build
      *> the story page text, the story choices and to handle the
      *> page I/O. The input from the I/O paragraph dictates the next
      *> story page that is loaded and built.
      *>****************************************************************
       350-READ-STORY.

           MOVE 'Reading story page' TO WS-DEBUG-MSG
           PERFORM 050-DEBUG-MESSAGE

           OPEN INPUT FD-STORY-FILE
               PERFORM UNTIL EOF-SW
               OR WS-GAMEOVER
                   READ FD-STORY-FILE INTO WS-STORY-RECORD
                       AT END MOVE 'Y' TO WS-EOF-SW
                       NOT AT END
                           IF WS-CURRENT-RECORD = WS-STORY-ID

                               MOVE 'Found story record' TO WS-DEBUG-MSG
                               PERFORM 050-DEBUG-MESSAGE

                               PERFORM 400-READ-STORY-TEXT
                               PERFORM 450-READ-STORY-CHOICES
                               PERFORM 500-HANDLE-STORY-IO
                           END-IF
                   END-READ
               END-PERFORM
           CLOSE FD-STORY-FILE
           MOVE 'N' TO WS-EOF-SW.

      *>****************************************************************
      *> Reads story page text for current page into the story text
      *> record.
      *>****************************************************************
       400-READ-STORY-TEXT.

           MOVE 'Reading story text for page.' TO WS-DEBUG-MSG
           PERFORM 050-DEBUG-MESSAGE

           OPEN INPUT FD-STORY-TEXT-FILE
               PERFORM UNTIL EOF-SW OR RECORD-FOUND
                   READ FD-STORY-TEXT-FILE INTO WS-STORY-TEXT-RECORD
                       AT END MOVE 'Y' TO WS-EOF-SW
                       NOT AT END
                           IF WS-STORY-TEXT-ID = WS-STORY-ID
                           AND WS-STORY-TEXT-EPISODE-ID = WS-EPISODE-ID

                               MOVE 'Found story text.' TO WS-DEBUG-MSG
                               PERFORM 050-DEBUG-MESSAGE

                               MOVE 'Y' TO WS-STORY-RECORD-FOUND
                           END-IF
                   END-READ
               END-PERFORM
           CLOSE FD-STORY-TEXT-FILE
           MOVE 'N' TO WS-EOF-SW
           MOVE 'N' TO WS-STORY-RECORD-FOUND.

      *>****************************************************************
      *> Reads and sets up the story page's choice's number and text
      *>
      *> Default values are set to 998 to flag empty choices as invalid
      *>****************************************************************
       450-READ-STORY-CHOICES.

           MOVE 'Reading story choices for page.' TO WS-DEBUG-MSG
           PERFORM 050-DEBUG-MESSAGE

           MOVE 998 TO WS-CHOICE-DESTINATION(1)
           MOVE 998 TO WS-CHOICE-DESTINATION(2)
           MOVE 998 TO WS-CHOICE-DESTINATION(3)
           MOVE 998 TO WS-CHOICE-DESTINATION(4)

           OPEN INPUT FD-STORY-CHOICE-FILE
               PERFORM UNTIL EOF-SW OR RECORD-FOUND
                   READ FD-STORY-CHOICE-FILE INTO WS-STORY-CHOICE-RECORD
                       AT END MOVE 'Y' TO WS-EOF-SW
                       NOT AT END
                           IF WS-STORY-CHOICE-STORY-ID = WS-STORY-ID
                           AND WS-STORY-CHOICE-EPISODE-ID
                               = WS-EPISODE-ID
                               MOVE 'Found choice' TO WS-DEBUG-MSG
                               PERFORM 050-DEBUG-MESSAGE

                               MOVE WS-STORY-CHOICE-TEXT TO
                                   WS-CHOICE-TEXT(WS-STORY-CHOICE-ID)

                               MOVE WS-STORY-CHOICE-DEST-STORY-ID TO
                                   WS-CHOICE-DESTINATION
                                       (WS-STORY-CHOICE-ID)

                           END-IF
                   END-READ
               END-PERFORM
           CLOSE FD-STORY-CHOICE-FILE
           MOVE 'N' TO WS-EOF-SW
           MOVE 'N' TO WS-STORY-RECORD-FOUND.

      *>****************************************************************
      *> Handles displaying output of the current stories page and
      *> choices built in previous paragraphs.
      *>
      *> Handles user input on the story page and checks to see if
      *> if they have won an episode. If so, the unlock flag is set
      *> for episode 4.
      *>****************************************************************
       500-HANDLE-STORY-IO.

           MOVE 'Displaying story page' TO WS-DEBUG-MSG
           PERFORM 050-DEBUG-MESSAGE

           MOVE 'N' TO WS-VALID-CHOICE

           PERFORM UNTIL WS-VALID-CHOICE = 'Y'

               DISPLAY BLANK-SCREEN
               ACCEPT STORY-SCREEN

               IF WS-STORY-INPUT NOT GREATER THAN 4
                   AND WS-STORY-INPUT GREATER THAN 0 THEN

                   MOVE WS-CHOICE-DESTINATION(WS-STORY-INPUT)
                       TO WS-CURRENT-RECORD

                   IF WS-CURRENT-RECORD NOT EQUAL 998
                       MOVE 'Y' TO WS-VALID-CHOICE
                   END-IF

               END-IF

           END-PERFORM

           IF WS-CURRENT-RECORD = 999
               MOVE 'Y' TO WS-GAMEOVER-SW

               SET WS-TAL-CTR TO 0

               INSPECT WS-STORY-TEXT
                   TALLYING WS-TAL-CTR
                   FOR ALL 'GAME OVER'

               IF WS-TAL-CTR <= 0 THEN
                   MOVE 'Episode won! Unlock!' TO WS-DEBUG-MSG
                   PERFORM 050-DEBUG-MESSAGE
                   MOVE 'Y' TO WS-FINAL-EPISODE-UNLOCKED
               END-IF
           END-IF.

       END PROGRAM WRY-COBOL.
