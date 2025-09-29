IDENTIFICATION DIVISION.
       PROGRAM-ID. PATH-UTILS.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-INDEX             PIC 9(4) COMP.
       01 WS-LENGTH            PIC 9(4) COMP.
       01 WS-CHAR              PIC X.
       01 WS-PREV-CHAR         PIC X VALUE SPACE.
       
       LINKAGE SECTION.
       01 LS-INPUT-PATH        PIC X(512).
       01 LS-OUTPUT-PATH       PIC X(512).
       01 LS-RETURN-CODE       PIC 9.
          88 PATH-VALID        VALUE 0.
          88 PATH-INVALID      VALUE 1.
       
       PROCEDURE DIVISION USING LS-INPUT-PATH LS-OUTPUT-PATH 
                                LS-RETURN-CODE.
       
       MAIN-LOGIC.
           MOVE SPACES TO LS-OUTPUT-PATH
           MOVE 0 TO LS-RETURN-CODE
           
           INSPECT LS-INPUT-PATH TALLYING WS-LENGTH 
               FOR CHARACTERS BEFORE INITIAL SPACE
           
      *>   DISPLAY "PATH-UTILS: Input length=" WS-LENGTH
      *>   DISPLAY "PATH-UTILS: Input path='" LS-INPUT-PATH(1:50) "'"
           
           IF WS-LENGTH = 0
      *>       DISPLAY "PATH-UTILS: Empty path, rejecting"
               MOVE 1 TO LS-RETURN-CODE
               GOBACK
           END-IF
           
           IF LS-INPUT-PATH = "/" OR LS-INPUT-PATH(1:2) = "/ "
      *>       DISPLAY "PATH-UTILS: Root path, using index.html"
               MOVE "index.html" TO LS-OUTPUT-PATH
               GOBACK
           END-IF
           
           IF LS-INPUT-PATH(1:1) = "/"
               MOVE LS-INPUT-PATH(2:) TO LS-OUTPUT-PATH
           ELSE
               MOVE LS-INPUT-PATH TO LS-OUTPUT-PATH
           END-IF
           
      *>   DISPLAY "PATH-UTILS: Output path='" LS-OUTPUT-PATH(1:50) "'"
           
           PERFORM VARYING WS-INDEX FROM 1 BY 1 
               UNTIL WS-INDEX > 510
               MOVE LS-OUTPUT-PATH(WS-INDEX:1) TO WS-CHAR
               IF WS-CHAR = SPACE OR WS-CHAR = LOW-VALUE
                   EXIT PERFORM
               END-IF
               IF WS-CHAR = "." AND WS-PREV-CHAR = "."
                   MOVE 1 TO LS-RETURN-CODE
                   GOBACK
               END-IF
               MOVE WS-CHAR TO WS-PREV-CHAR
           END-PERFORM
           
           IF LS-OUTPUT-PATH(1:3) = "../" OR
              LS-OUTPUT-PATH = ".."
      *>       DISPLAY "PATH-UTILS: Directory traversal detected"
               MOVE 1 TO LS-RETURN-CODE
           END-IF
           
      *>   DISPLAY "PATH-UTILS: Final return code=" LS-RETURN-CODE
           
           GOBACK.
