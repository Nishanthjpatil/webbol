*> Socket definitions copybook
       01 SOCKET-HANDLE        PIC S9(9) COMP-5.
       01 CLIENT-SOCKET        PIC S9(9) COMP-5.
       01 SOCKET-RESULT        PIC S9(9) COMP-5.
       01 BYTES-READ           PIC S9(9) COMP-5.
       01 BYTES-SENT           PIC S9(9) COMP-5.
       
       01 SERVER-ADDRESS.
          05 SA-FAMILY         PIC 9(4) COMP-5 VALUE 2.
          05 SA-PORT           PIC 9(4) COMP-5.
          05 SA-ADDR           PIC 9(8) COMP-5 VALUE 0.
          05 FILLER            PIC X(8) VALUE SPACES.
          
       01 ADDR-LEN             PIC 9(9) COMP-5 VALUE 16.
       01 BACKLOG              PIC 9(9) COMP-5 VALUE 10.
       01 SOCKET-OPT           PIC 9(9) COMP-5 VALUE 1.
