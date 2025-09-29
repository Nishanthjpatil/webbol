*> Socket system call definitions and data structures
*> Server socket file descriptor (signed binary for system calls)
       01 SOCKET-HANDLE        PIC S9(9) COMP-5.
*> Client connection socket file descriptor
       01 CLIENT-SOCKET        PIC S9(9) COMP-5.
*> Return value from socket system calls (negative = error)
       01 SOCKET-RESULT        PIC S9(9) COMP-5.
*> Number of bytes received from client
       01 BYTES-READ           PIC S9(9) COMP-5.
*> Number of bytes sent to client
       01 BYTES-SENT           PIC S9(9) COMP-5.

*> Internet socket address structure (matches C struct sockaddr_in)
       01 SERVER-ADDRESS.
*> Address family: AF_INET (2) for IPv4
          05 SA-FAMILY         PIC 9(4) COMP-5 VALUE 2.
*> Port number in network byte order
          05 SA-PORT           PIC 9(4) COMP-5.
*> IP address (0 = INADDR_ANY, bind to all interfaces)
          05 SA-ADDR           PIC 9(8) COMP-5 VALUE 0.
*> Padding to match C struct size (8 bytes)
          05 FILLER            PIC X(8) VALUE SPACES.

*> Size of socket address structure (16 bytes for IPv4)
       01 ADDR-LEN             PIC 9(9) COMP-5 VALUE 16.
*> Maximum pending connections for listen() system call
       01 BACKLOG              PIC 9(9) COMP-5 VALUE 10.
*> Socket option value for SO_REUSEADDR (1 = enable)
       01 SOCKET-OPT           PIC 9(9) COMP-5 VALUE 1.
