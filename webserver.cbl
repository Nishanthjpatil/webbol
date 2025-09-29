*> Program identification - required in every COBOL program
IDENTIFICATION DIVISION.
       PROGRAM-ID. WEBSERVER.

*> Data definitions section - defines all variables and structures
       DATA DIVISION.
       WORKING-STORAGE SECTION.
*> Include shared configuration values (port, buffer sizes)
       COPY "config.cpy".
*> Include socket system call definitions and data structures
       COPY "socket-defs.cpy".
*> Include HTTP request/response data structures
       COPY "http-structs.cpy".

*> Counter for tracking total requests served (8-digit number)
       01 WS-REQUEST-COUNT     PIC 9(8) VALUE 0.
*> String representation of port number (5 characters max)
       01 WS-PORT-STR          PIC X(5).
*> Port number in network byte order (binary format for system calls)
       01 WS-PORT-NETWORK      PIC 9(4) COMP-5.
*> Position where HTTP headers end in request buffer
       01 WS-HEADER-END        PIC 9(4) COMP-5.
       
*> Executable code section - contains the program's logic
       PROCEDURE DIVISION.

*> Main program entry point
       MAIN-LOGIC.
           DISPLAY "COBOL Web Server Starting..."
           DISPLAY "Press Ctrl+C to stop"
           DISPLAY " "

*> Initialize socket for accepting connections
           PERFORM INIT-SOCKET

*> Check if socket creation failed (negative handle indicates error)
           IF SOCKET-HANDLE < 0
               DISPLAY "Failed to initialize socket"
               STOP RUN
           END-IF

*> Infinite loop to accept and handle client connections
*> UNTIL 1 = 0 creates a loop that never ends naturally
           PERFORM ACCEPT-LOOP UNTIL 1 = 0

           STOP RUN.
       
*> Create and configure a TCP socket for the web server
       INIT-SOCKET.
*> Create socket: AF_INET(2), SOCK_STREAM(1), IPPROTO_TCP(0)
           CALL "socket" USING BY VALUE 2 BY VALUE 1 BY VALUE 0
               RETURNING SOCKET-HANDLE
           END-CALL

           IF SOCKET-HANDLE < 0
               DISPLAY "Socket creation failed"
               GOBACK
           END-IF

*> Set SO_REUSEADDR option to allow immediate port reuse after restart
*> Parameters: socket, SOL_SOCKET(1), SO_REUSEADDR(2), option_value, size
           CALL "setsockopt" USING
               BY VALUE SOCKET-HANDLE
               BY VALUE 1
               BY VALUE 2
               BY REFERENCE SOCKET-OPT
               BY VALUE 4
               RETURNING SOCKET-RESULT
           END-CALL

*> Get size of socket address structure for system calls
           MOVE FUNCTION BYTE-LENGTH(SERVER-ADDRESS) TO ADDR-LEN

*> Convert port to network byte order (big-endian)
*> Split port into low/high bytes and swap them
           COMPUTE WS-PORT-NETWORK =
               FUNCTION MOD(SERVER-PORT, 256) * 256 +
               SERVER-PORT / 256
           MOVE WS-PORT-NETWORK TO SA-PORT

*> Bind socket to address and port (makes it available for connections)
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

*> Start listening for connections with specified backlog queue size
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
       
*> Main server loop - accept and handle client connections
       ACCEPT-LOOP.
*> Reset address length for each accept call
           MOVE FUNCTION BYTE-LENGTH(SERVER-ADDRESS) TO ADDR-LEN

*> Wait for and accept incoming client connection
*> This blocks until a client connects
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

*> Increment request counter for logging
           ADD 1 TO WS-REQUEST-COUNT

*> Process the HTTP request and send response
           PERFORM HANDLE-REQUEST

*> Close client connection (one request per connection)
           CALL "close" USING BY VALUE CLIENT-SOCKET
           END-CALL
           .
       
*> Read HTTP request from client and generate response
       HANDLE-REQUEST.
*> Clear buffers before processing new request
           MOVE SPACES TO REQUEST-BUFFER
           MOVE SPACES TO RESPONSE-BUFFER

*> Read HTTP request data from client socket
*> Parameters: socket, buffer, max_bytes, flags
           CALL "recv" USING
               BY VALUE CLIENT-SOCKET
               BY REFERENCE REQUEST-BUFFER
               BY VALUE 8192
               BY VALUE 0
               RETURNING BYTES-READ
           END-CALL

*> Exit if no data received or connection closed
           IF BYTES-READ <= 0
               GOBACK
           END-IF

*> Increment request counter for this specific request
           ADD 1 TO WS-REQUEST-COUNT

*> Find end of HTTP headers (marked by CRLF CRLF sequence)
*> X"0D0A0D0A" represents carriage return + line feed twice
           MOVE 0 TO WS-HEADER-END
           PERFORM VARYING WS-HEADER-END FROM 4 BY 1
               UNTIL WS-HEADER-END > BYTES-READ OR WS-HEADER-END > 8188
               IF REQUEST-BUFFER(WS-HEADER-END - 3:4) = X"0D0A0D0A"
                   SUBTRACT 3 FROM WS-HEADER-END
                   EXIT PERFORM
               END-IF
           END-PERFORM

*> Log the HTTP request headers to console
           IF WS-HEADER-END > 0 AND WS-HEADER-END <= BYTES-READ
               DISPLAY "Request #" WS-REQUEST-COUNT ":"
               DISPLAY REQUEST-BUFFER(1:WS-HEADER-END)
           ELSE
               DISPLAY "Request #" WS-REQUEST-COUNT ": "
                   REQUEST-BUFFER(1:200)
           END-IF

*> Call HTTP handler to parse request and build response
           CALL "HTTP-HANDLER" USING
               REQUEST-BUFFER
               RESPONSE-BUFFER
               RESPONSE-LEN

*> Send HTTP response back to client if response was generated
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
