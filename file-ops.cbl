IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-OPS.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DISK-FILE ASSIGN TO WS-FILE-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  DISK-FILE.
       01  FILE-RECORD         PIC X(1024).
       
       WORKING-STORAGE SECTION.
       01 WS-FILE-NAME         PIC X(512).
       01 WS-FILE-STATUS       PIC XX.
       01 WS-BUFFER-POS        PIC 9(8) COMP-5.
       01 WS-LINE-LEN          PIC 9(4) COMP-5.
       01 WS-EOF-FLAG          PIC 9 VALUE 0.
       01 WS-OVERFLOW-FLAG     PIC 9 VALUE 0.
       
       LINKAGE SECTION.
       01 LS-FILE-PATH         PIC X(512).
       01 LS-FILE-BUFFER       PIC X(65536).
       01 LS-FILE-SIZE         PIC 9(8) COMP-5.
       01 LS-RETURN-CODE       PIC 9.
          88 FILE-READ-OK      VALUE 0.
          88 FILE-READ-ERROR   VALUE 1.
          88 FILE-TOO-LARGE    VALUE 2.
       
       PROCEDURE DIVISION USING LS-FILE-PATH LS-FILE-BUFFER 
                                LS-FILE-SIZE LS-RETURN-CODE.
       
       MAIN-LOGIC.
           MOVE SPACES TO LS-FILE-BUFFER
           MOVE 0 TO LS-FILE-SIZE
           MOVE 0 TO LS-RETURN-CODE
           MOVE 1 TO WS-BUFFER-POS
           MOVE 0 TO WS-OVERFLOW-FLAG
           
           MOVE SPACES TO WS-FILE-NAME
*> Find actual length of file path including embedded spaces
*> Scan backwards from end to find last non-space, non-null character
*> This allows filenames with spaces (e.g. "test file.html")
           MOVE 0 TO WS-LINE-LEN
           PERFORM VARYING WS-LINE-LEN FROM 512 BY -1
               UNTIL WS-LINE-LEN < 1
               IF LS-FILE-PATH(WS-LINE-LEN:1) NOT = SPACE AND
                  LS-FILE-PATH(WS-LINE-LEN:1) NOT = LOW-VALUE
                   EXIT PERFORM
               END-IF
           END-PERFORM
*> Copy the trimmed path to file name variable
           IF WS-LINE-LEN > 0
               MOVE LS-FILE-PATH(1:WS-LINE-LEN) TO WS-FILE-NAME
           ELSE
               MOVE LS-FILE-PATH TO WS-FILE-NAME
           END-IF
           
      *>   DISPLAY "FILE-OPS: Opening file: '" WS-FILE-NAME(1:50) "'"
           
           OPEN INPUT DISK-FILE
           
           IF WS-FILE-STATUS NOT = "00"
      *>       DISPLAY "FILE-OPS: Open failed, status: " WS-FILE-STATUS
               MOVE 1 TO LS-RETURN-CODE
               GOBACK
           END-IF
           
      *>   DISPLAY "FILE-OPS: File opened successfully"
           
           MOVE 0 TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF-FLAG = 1
               READ DISK-FILE
                   AT END
                       MOVE 1 TO WS-EOF-FLAG
                   NOT AT END
*> Calculate actual line length by trimming trailing whitespace
*> Start from end and work backwards to find last non-space character
                       MOVE 0 TO WS-LINE-LEN
                       PERFORM VARYING WS-LINE-LEN FROM 1024 BY -1
                           UNTIL WS-LINE-LEN < 1
                           IF FILE-RECORD(WS-LINE-LEN:1) NOT = SPACE AND
                              FILE-RECORD(WS-LINE-LEN:1) NOT = X"0D" AND
                              FILE-RECORD(WS-LINE-LEN:1) NOT = LOW-VALUE
                               EXIT PERFORM
                           END-IF
                       END-PERFORM
*> Check if line fits in output buffer (prevent overflow)
                       IF WS-BUFFER-POS + WS-LINE-LEN + 1 <= 65536
*> Copy line content to output buffer if not empty
                           IF WS-LINE-LEN > 0
                               MOVE FILE-RECORD(1:WS-LINE-LEN) TO
                                   LS-FILE-BUFFER(WS-BUFFER-POS:WS-LINE-LEN)
                               ADD WS-LINE-LEN TO WS-BUFFER-POS
                           END-IF
*> Add line terminator (LF character) after each line
                           MOVE X"0A" TO
                               LS-FILE-BUFFER(WS-BUFFER-POS:1)
                           ADD 1 TO WS-BUFFER-POS
                       ELSE
*> Stop reading if buffer would overflow - mark as error
      *>                   DISPLAY "FILE-OPS: Buffer full"
                           MOVE 1 TO WS-EOF-FLAG
                           MOVE 1 TO WS-OVERFLOW-FLAG
                       END-IF
               END-READ
           END-PERFORM
           
*> Calculate final file size (subtract 1 for final LF)
           IF WS-BUFFER-POS > 1
               COMPUTE LS-FILE-SIZE = WS-BUFFER-POS - 1
           ELSE
               MOVE 0 TO LS-FILE-SIZE
           END-IF

      *>   DISPLAY "FILE-OPS: Total bytes read=" LS-FILE-SIZE

*> Close file to free system resources
           CLOSE DISK-FILE

*> Return error if file was too large for buffer
           IF WS-OVERFLOW-FLAG = 1
               MOVE 2 TO LS-RETURN-CODE
           END-IF

           GOBACK.
