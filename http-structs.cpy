*> HTTP structures copybook
       01 HTTP-REQUEST.
          05 REQUEST-METHOD    PIC X(10).
          05 REQUEST-PATH      PIC X(512).
          05 REQUEST-BUFFER    PIC X(8192).
          
       01 HTTP-RESPONSE.
          05 RESPONSE-BUFFER   PIC X(65536).
          05 RESPONSE-LEN      PIC 9(8) COMP-5.
          
       01 STATUS-LINE          PIC X(50).
       01 CONTENT-TYPE-HDR     PIC X(100).
       01 CONTENT-LENGTH-HDR   PIC X(50).
