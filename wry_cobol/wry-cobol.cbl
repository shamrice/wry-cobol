      *>*****************************************************************
      *> Author: Erik Eriksen
      *> Date: 12/16/2018
      *> Purpose: Version of Wry written in COBOL
      *> Tectonics: cobc
      *>*****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. WRY-COBOL.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT FD-STORY-FILE
                   ASSIGN TO '../data/story.dat'
                   ORGANIZATION IS LINE SEQUENTIAL.

               SELECT FD-STORY-TEXT-FILE
                   ASSIGN TO "../data/story-text.dat"
                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  FD-STORY-FILE.
       01  FD-STORY-RECORD.
           88 EOF-STORY                        VALUE HIGH-VALUES.
           05 STORY-ID                         PIC 9(4).
           05 CORRECT-STORY-ID                 PIC 9(4).
           05 CORRECT-CHOICE-ID                PIC 9(1).
           05 CHOICES                          OCCURS 4 TIMES.
               10 CHOICE-TEXT                  PIC X(50).

       FD  FD-STORY-TEXT-FILE.
       01  FD-STORY-TEXT-RECORD.
           88 EOF-STORY-TEXT                   VALUE HIGH-VALUES.
           05 STORY-TEXT-ID                    PIC 9(4).
           05 STORY-TEXT                       PIC X(255).

       WORKING-STORAGE SECTION.

       77  WS-VERSION-NUM                      PIC X(5) VALUE "0.001".
       77  WS-CURRENT-RECORD                   PIC 9(4) VALUE 0000.

       77  WS-TITLE-INPUT                      PIC X.
       77  WS-STORY-INPUT                      PIC 9(1).

       01  WS-EOF-SW                           PIC X(1) VALUE 'N'.
           88 EOF-SW                           VALUE 'Y'.
           88 NOT-EOF-SW                       VALUE 'N'.

       01  WS-STORY-RECORD.
           05 WS-STORY-ID                      PIC 9(4).
           05 WS-CORRECT-STORY-ID              PIC 9(4).
           05 WS-CORRECT-CHOICE-ID             PIC 9(1).
           05 WS-CHOICES                       OCCURS 4 TIMES.
               10 WS-CHOICES-TEXT              PIC X(9).

       01  WS-STORY-TEXT-RECORD.
           05 WS-STORY-TEXT-ID                 PIC 9(4).
           05 WS-STORY-TEXT                    PIC X(255).

       01  WS-STORY-TEXT-RECORD-FOUND          PIC A(1) VALUE 'N'.
           88 RECORD-FOUND                     VALUE 'Y'.
           88 RECORD-NOT-FOUND                 VALUE 'N'.

       SCREEN SECTION.

       COPY 'screens/blank.cbl'.

       COPY 'screens/title.cbl'.

       COPY 'screens/story.cbl'.


       PROCEDURE DIVISION.

       000-MAIN-PROCEDURE.
           DISPLAY TITLE-SCREEN
           ACCEPT TITLE-SCREEN

           PERFORM 100-READ-STORY


           STOP RUN.

       100-READ-STORY.

           OPEN INPUT FD-STORY-FILE
               PERFORM UNTIL EOF-STORY
                   READ FD-STORY-FILE INTO WS-STORY-RECORD
                       AT END SET EOF-STORY TO TRUE
                       NOT AT END
      *                     DISPLAY WS-STORY-ID
      *                     DISPLAY WS-CORRECT-STORY-ID
      *                     DISPLAY WS-CORRECT-CHOICE-ID
      *                     DISPLAY WS-CHOICES(1)

                           PERFORM 200-READ-STORY-TEXT

                           DISPLAY BLANK-SCREEN
                           DISPLAY STORY-SCREEN
                           ACCEPT STORY-SCREEN


                   END-READ
               END-PERFORM
           CLOSE FD-STORY-FILE.


       200-READ-STORY-TEXT.
           OPEN INPUT FD-STORY-TEXT-FILE
      *         DISPLAY 'Reading story data'
               PERFORM UNTIL EOF-SW OR RECORD-FOUND
      *             DISPLAY 'Not at eof so reading story...'
                   READ FD-STORY-TEXT-FILE INTO WS-STORY-TEXT-RECORD
                       AT END MOVE 'Y' TO WS-EOF-SW
                       NOT AT END
                           IF WS-STORY-TEXT-ID = WS-STORY-ID
      *                         DISPLAY WS-STORY-TEXT
                               MOVE 'Y' TO WS-STORY-TEXT-RECORD-FOUND
                           END-IF
                   END-READ
               END-PERFORM
      *         DISPLAY 'Done story data perform.. clsoing file'
           CLOSE FD-STORY-TEXT-FILE
           MOVE 'N' TO WS-EOF-SW
           MOVE 'N' TO WS-STORY-TEXT-RECORD-FOUND.

       END PROGRAM WRY-COBOL.
