*> Path validation and sanitization module for security
IDENTIFICATION DIVISION.
       PROGRAM-ID. PATH-UTILS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
*> Index for character-by-character path scanning
       01 WS-INDEX             PIC 9(4) COMP.
*> Length of input path string
       01 WS-LENGTH            PIC 9(4) COMP.
*> Current character being examined
       01 WS-CHAR              PIC X.
*> Previous character (for detecting ".." sequences)
       01 WS-PREV-CHAR         PIC X VALUE SPACE.
       
*> Parameters passed from calling program
       LINKAGE SECTION.
*> Original path from HTTP request
       01 LS-INPUT-PATH        PIC X(512).
*> Sanitized path safe for file system access
       01 LS-OUTPUT-PATH       PIC X(512).
*> Return code indicating validation result
       01 LS-RETURN-CODE       PIC 9.
*> Condition names for readable code (88-level items)
          88 PATH-VALID        VALUE 0.
          88 PATH-INVALID      VALUE 1.
       
       PROCEDURE DIVISION USING LS-INPUT-PATH LS-OUTPUT-PATH 
                                LS-RETURN-CODE.
       
*> Main path validation and sanitization logic
       MAIN-LOGIC.
*> Initialize output path and assume path is valid
           MOVE SPACES TO LS-OUTPUT-PATH
           MOVE 0 TO LS-RETURN-CODE

*> Calculate actual length of input path (excluding trailing spaces)
           INSPECT LS-INPUT-PATH TALLYING WS-LENGTH
               FOR CHARACTERS BEFORE INITIAL SPACE
           
      *>   DISPLAY "PATH-UTILS: Input length=" WS-LENGTH
      *>   DISPLAY "PATH-UTILS: Input path='" LS-INPUT-PATH(1:50) "'"
           
*> Reject empty paths as invalid
           IF WS-LENGTH = 0
      *>       DISPLAY "PATH-UTILS: Empty path, rejecting"
               MOVE 1 TO LS-RETURN-CODE
               GOBACK
           END-IF

*> Handle root path (/) by serving default index.html
           IF LS-INPUT-PATH = "/" OR LS-INPUT-PATH(1:2) = "/ "
      *>       DISPLAY "PATH-UTILS: Root path, using index.html"
               MOVE "index.html" TO LS-OUTPUT-PATH
               GOBACK
           END-IF
           
*> Remove leading slash to create relative path
*> Web paths start with / but file system needs relative paths
           IF LS-INPUT-PATH(1:1) = "/"
               MOVE LS-INPUT-PATH(2:) TO LS-OUTPUT-PATH
           ELSE
               MOVE LS-INPUT-PATH TO LS-OUTPUT-PATH
           END-IF
           
      *>   DISPLAY "PATH-UTILS: Output path='" LS-OUTPUT-PATH(1:50) "'"
           
*> Reject absolute paths as security risk
*> Prevents access outside current directory
           IF LS-OUTPUT-PATH(1:1) = "/"
      *>       DISPLAY "PATH-UTILS: Absolute path detected"
               MOVE 1 TO LS-RETURN-CODE
               GOBACK
           END-IF
           
*> Scan path character by character for security threats
*> Look for ".." sequences that could access parent directories
           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > 510
               MOVE LS-OUTPUT-PATH(WS-INDEX:1) TO WS-CHAR
*> Stop at end of string
               IF WS-CHAR = SPACE OR WS-CHAR = LOW-VALUE
                   EXIT PERFORM
               END-IF
*> Detect ".." pattern (directory traversal attack)
               IF WS-CHAR = "." AND WS-PREV-CHAR = "."
                   MOVE 1 TO LS-RETURN-CODE
                   GOBACK
               END-IF
               MOVE WS-CHAR TO WS-PREV-CHAR
           END-PERFORM
           
*> Additional check for paths starting with "../" or exactly ".."
*> These are classic directory traversal patterns
           IF LS-OUTPUT-PATH(1:3) = "../" OR
              LS-OUTPUT-PATH = ".."
      *>       DISPLAY "PATH-UTILS: Directory traversal detected"
               MOVE 1 TO LS-RETURN-CODE
           END-IF
           
      *>   DISPLAY "PATH-UTILS: Final return code=" LS-RETURN-CODE
           
           GOBACK.
