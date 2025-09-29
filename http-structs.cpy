*> HTTP request and response data structures
*> Structure for parsing HTTP requests
       01 HTTP-REQUEST.
*> HTTP method (GET, POST, PUT, etc.) - up to 10 characters
          05 REQUEST-METHOD    PIC X(10).
*> Requested URL path - up to 512 characters
          05 REQUEST-PATH      PIC X(512).
*> Raw HTTP request data from client - 8KB maximum
          05 REQUEST-BUFFER    PIC X(8192).
          
*> Structure for building HTTP responses
       01 HTTP-RESPONSE.
*> Complete HTTP response (headers + content) - 64KB maximum
          05 RESPONSE-BUFFER   PIC X(65536).
*> Actual length of response data (binary for efficiency)
          05 RESPONSE-LEN      PIC 9(8) COMP-5.
          
*> Utility fields for HTTP header construction
*> HTTP status line (e.g., "HTTP/1.1 200 OK")
       01 STATUS-LINE          PIC X(50).
*> Content-Type header value (e.g., "text/html")
       01 CONTENT-TYPE-HDR     PIC X(100).
*> Content-Length header value (e.g., "1024")
       01 CONTENT-LENGTH-HDR   PIC X(50).
