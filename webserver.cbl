IDENTIFICATION DIVISION.
       PROGRAM-ID. WEBSERVER.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "config.cpy".
       COPY "socket-defs.cpy".
       COPY "http-structs.cpy".
       
       01 WS-REQUEST-COUNT     PIC 9(8) VALUE 0.
       01 WS-PORT-STR          PIC X(5).
       01 WS-PORT-NETWORK      PIC 9(4) COMP-5.
       01 WS-HEADER-END        PIC 9(4) COMP-5.
       
       PROCEDURE DIVISION.
       
       MAIN-LOGIC.
           DISPLAY "COBOL Web Server Starting..."
           DISPLAY "Press Ctrl+C to stop"
           DISPLAY " "
           
           PERFORM INIT-SOCKET
           
           IF SOCKET-HANDLE < 0
               DISPLAY "Failed to initialize socket"
               STOP RUN
           END-IF
           
           PERFORM ACCEPT-LOOP UNTIL 1 = 0
           
           STOP RUN.
       
       INIT-SOCKET.
           CALL "socket" USING BY VALUE 2 BY VALUE 1 BY VALUE 0
               RETURNING SOCKET-HANDLE
           END-CALL
           
           IF SOCKET-HANDLE < 0
               DISPLAY "Socket creation failed"
               GOBACK
           END-IF
           
           CALL "setsockopt" USING 
               BY VALUE SOCKET-HANDLE
               BY VALUE 1
               BY VALUE 2
               BY REFERENCE SOCKET-OPT
               BY VALUE 4
               RETURNING SOCKET-RESULT
           END-CALL
           
           MOVE FUNCTION BYTE-LENGTH(SERVER-ADDRESS) TO ADDR-LEN
           
           COMPUTE WS-PORT-NETWORK = 
               FUNCTION MOD(SERVER-PORT, 256) * 256 +
               SERVER-PORT / 256
           MOVE WS-PORT-NETWORK TO SA-PORT
           
           CALL "bind" USING 
               BY VALUE SOCKET-HANDLE
               BY REFERENCE SERVER-ADDRESS
               BY VALUE ADDR-LEN
               RETURNING SOCKET-RESULT
           END-CALL
           
           IF SOCKET-RESULT < 0
               DISPLAY "Bind failed - check if port is in use"
               GOBACK
           END-IF
           
           CALL "listen" USING 
               BY VALUE SOCKET-HANDLE
               BY VALUE BACKLOG
               RETURNING SOCKET-RESULT
           END-CALL
           
           IF SOCKET-RESULT < 0
               DISPLAY "Listen failed"
               GOBACK
           END-IF
           
           DISPLAY "Server listening on port " SERVER-PORT
           .
       
       ACCEPT-LOOP.
           MOVE FUNCTION BYTE-LENGTH(SERVER-ADDRESS) TO ADDR-LEN
           
           CALL "accept" USING 
               BY VALUE SOCKET-HANDLE
               BY REFERENCE SERVER-ADDRESS
               BY REFERENCE ADDR-LEN
               RETURNING CLIENT-SOCKET
           END-CALL
           
           IF CLIENT-SOCKET < 0
               DISPLAY "Accept failed"
               GOBACK
           END-IF
           
           ADD 1 TO WS-REQUEST-COUNT
           
           PERFORM HANDLE-REQUEST
           
           CALL "close" USING BY VALUE CLIENT-SOCKET
           END-CALL
           .
       
       HANDLE-REQUEST.
           MOVE SPACES TO REQUEST-BUFFER
           MOVE SPACES TO RESPONSE-BUFFER
           
           CALL "recv" USING 
               BY VALUE CLIENT-SOCKET
               BY REFERENCE REQUEST-BUFFER
               BY VALUE 8192
               BY VALUE 0
               RETURNING BYTES-READ
           END-CALL
           
           IF BYTES-READ <= 0
               GOBACK
           END-IF
           
           ADD 1 TO WS-REQUEST-COUNT
           
           MOVE 0 TO WS-HEADER-END
           PERFORM VARYING WS-HEADER-END FROM 4 BY 1
               UNTIL WS-HEADER-END > BYTES-READ OR WS-HEADER-END > 8188
               IF REQUEST-BUFFER(WS-HEADER-END - 3:4) = X"0D0A0D0A"
                   SUBTRACT 3 FROM WS-HEADER-END
                   EXIT PERFORM
               END-IF
           END-PERFORM
           
           IF WS-HEADER-END > 0 AND WS-HEADER-END <= BYTES-READ
               DISPLAY "Request #" WS-REQUEST-COUNT ":"
               DISPLAY REQUEST-BUFFER(1:WS-HEADER-END)
           ELSE
               DISPLAY "Request #" WS-REQUEST-COUNT ": " 
                   REQUEST-BUFFER(1:200)
           END-IF
           
           CALL "HTTP-HANDLER" USING 
               REQUEST-BUFFER 
               RESPONSE-BUFFER
               RESPONSE-LEN
           
           IF RESPONSE-LEN > 0
               CALL "send" USING 
                   BY VALUE CLIENT-SOCKET
                   BY REFERENCE RESPONSE-BUFFER
                   BY VALUE RESPONSE-LEN
                   BY VALUE 0
                   RETURNING BYTES-SENT
               END-CALL
           END-IF
           .
