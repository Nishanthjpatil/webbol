IDENTIFICATION DIVISION.
       PROGRAM-ID. HTTP-HANDLER.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "http-structs.cpy".
       COPY "file-structs.cpy".
       
       01 WS-INDEX             PIC 9(4) COMP.
       01 WS-SPACE-POS         PIC 9(4) COMP.
       01 WS-PATH-LEN          PIC 9(4) COMP.
       01 WS-RETURN-CODE       PIC 9.
       01 WS-SIZE-STR          PIC X(10).
       01 WS-CRLF              PIC XX VALUE X"0D0A".
       
       LINKAGE SECTION.
       01 LS-REQUEST-BUF       PIC X(8192).
       01 LS-RESPONSE-BUF      PIC X(65536).
       01 LS-RESPONSE-LEN      PIC 9(8) COMP-5.
       
       PROCEDURE DIVISION USING LS-REQUEST-BUF LS-RESPONSE-BUF 
                                LS-RESPONSE-LEN.
       
       MAIN-LOGIC.
           MOVE SPACES TO REQUEST-METHOD
           MOVE SPACES TO REQUEST-PATH
           MOVE 0 TO LS-RESPONSE-LEN
           
      *>   DISPLAY "Raw request: '" LS-REQUEST-BUF(1:80) "'"
           
           MOVE 0 TO WS-SPACE-POS
           INSPECT LS-REQUEST-BUF TALLYING WS-SPACE-POS 
               FOR CHARACTERS BEFORE INITIAL SPACE
           
      *>   DISPLAY "First space at position: " WS-SPACE-POS
      *>   DISPLAY "Character at pos 4: '" LS-REQUEST-BUF(4:1) "' = "
      *>       FUNCTION ORD(LS-REQUEST-BUF(4:1))
      *>   DISPLAY "Character at pos 5: '" LS-REQUEST-BUF(5:1) "' = "
      *>       FUNCTION ORD(LS-REQUEST-BUF(5:1))
           
           IF WS-SPACE-POS > 0 AND WS-SPACE-POS <= 10
               MOVE LS-REQUEST-BUF(1:WS-SPACE-POS) TO REQUEST-METHOD
      *>       DISPLAY "Method: '" REQUEST-METHOD "'"
           END-IF
           
           COMPUTE WS-INDEX = WS-SPACE-POS + 2
      *>   DISPLAY "Starting path search at position: " WS-INDEX
           MOVE 0 TO WS-PATH-LEN
           PERFORM VARYING WS-SPACE-POS FROM WS-INDEX BY 1
               UNTIL WS-SPACE-POS > 8192
               IF LS-REQUEST-BUF(WS-SPACE-POS:1) = SPACE OR
                  LS-REQUEST-BUF(WS-SPACE-POS:1) = X"0D" OR
                  LS-REQUEST-BUF(WS-SPACE-POS:1) = X"0A"
                   COMPUTE WS-PATH-LEN = WS-SPACE-POS - WS-INDEX
      *>           DISPLAY "Found delimiter at position: " WS-SPACE-POS
      *>           DISPLAY "Delimiter is: " 
      *>               FUNCTION ORD(LS-REQUEST-BUF(WS-SPACE-POS:1))
                   EXIT PERFORM
               END-IF
           END-PERFORM
           
      *>   DISPLAY "Path starts at: " WS-INDEX
      *>   DISPLAY "Path length: " WS-PATH-LEN
           
           IF WS-PATH-LEN > 0 AND WS-PATH-LEN <= 512
               MOVE LS-REQUEST-BUF(WS-INDEX:WS-PATH-LEN) 
                   TO REQUEST-PATH
      *>       DISPLAY "Extracted path: '" REQUEST-PATH(1:50) "'"
           END-IF
           
           CALL "PATH-UTILS" USING REQUEST-PATH SANITIZED-PATH 
                                   WS-RETURN-CODE
           
      *>   DISPLAY "Requested path: '" REQUEST-PATH "'"
      *>   DISPLAY "Sanitized path: '" SANITIZED-PATH "'"
      *>   DISPLAY "Path validation result: " WS-RETURN-CODE
           
           IF WS-RETURN-CODE NOT = 0
               PERFORM BUILD-403-RESPONSE
               GOBACK
           END-IF
           
           CALL "FILE-OPS" USING SANITIZED-PATH FILE-BUFFER 
                                 FILE-SIZE WS-RETURN-CODE
           
      *>   DISPLAY "File read result: " WS-RETURN-CODE
      *>   DISPLAY "File size: " FILE-SIZE
           
           IF WS-RETURN-CODE NOT = 0
      *>       DISPLAY "File not found: '" SANITIZED-PATH "'"
               PERFORM BUILD-404-RESPONSE
               GOBACK
           END-IF
           
           CALL "MIME-TYPES" USING SANITIZED-PATH MIME-TYPE
           
           PERFORM BUILD-200-RESPONSE
           
           GOBACK.
       
       BUILD-200-RESPONSE.
           MOVE FILE-SIZE TO WS-SIZE-STR
           MOVE LOW-VALUE TO LS-RESPONSE-BUF
           
           STRING "HTTP/1.1 200 OK" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "Content-Type: " DELIMITED BY SIZE
                  MIME-TYPE DELIMITED BY SPACE
                  WS-CRLF DELIMITED BY SIZE
                  "Content-Length: " DELIMITED BY SIZE
                  WS-SIZE-STR DELIMITED BY SPACE
                  WS-CRLF DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  INTO LS-RESPONSE-BUF
           END-STRING
           
           MOVE 0 TO LS-RESPONSE-LEN
           INSPECT LS-RESPONSE-BUF TALLYING LS-RESPONSE-LEN
               FOR CHARACTERS BEFORE INITIAL LOW-VALUE
           
      *>   DISPLAY "Header length: " LS-RESPONSE-LEN
           
           IF LS-RESPONSE-LEN > 0 AND FILE-SIZE > 0
               MOVE FILE-BUFFER(1:FILE-SIZE) TO 
                   LS-RESPONSE-BUF(LS-RESPONSE-LEN + 1:FILE-SIZE)
               ADD FILE-SIZE TO LS-RESPONSE-LEN
           END-IF
           
      *>   DISPLAY "Total response length: " LS-RESPONSE-LEN
      *>   DISPLAY "File size: " FILE-SIZE
           .
       
       BUILD-404-RESPONSE.
           STRING "HTTP/1.1 404 Not Found" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "Content-Type: text/html" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "Content-Length: 47" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "<html><body><h1>404 Not Found</h1></body></html>"
                      DELIMITED BY SIZE
                  INTO LS-RESPONSE-BUF
           END-STRING
           
           INSPECT LS-RESPONSE-BUF TALLYING LS-RESPONSE-LEN
               FOR CHARACTERS BEFORE INITIAL LOW-VALUE
           .
       
       BUILD-403-RESPONSE.
           STRING "HTTP/1.1 403 Forbidden" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "Content-Type: text/html" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "Content-Length: 47" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "<html><body><h1>403 Forbidden</h1></body></html>"
                      DELIMITED BY SIZE
                  INTO LS-RESPONSE-BUF
           END-STRING
           
           INSPECT LS-RESPONSE-BUF TALLYING LS-RESPONSE-LEN
               FOR CHARACTERS BEFORE INITIAL LOW-VALUE
           .
