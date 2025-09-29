*> Server configuration constants (copybook for reuse across modules)
*> TCP port number for HTTP server (standard development port)
       01 SERVER-PORT          PIC 9(5) VALUE 8080.
*> Maximum pending connections in listen queue
       01 MAX-CONNECTIONS      PIC 9(3) VALUE 10.
*> Maximum size for file content and HTTP responses (64KB)
       01 BUFFER-SIZE          PIC 9(8) VALUE 65536.
*> Maximum length for file paths (prevents buffer overflow)
       01 MAX-PATH-LEN         PIC 9(4) VALUE 512.
