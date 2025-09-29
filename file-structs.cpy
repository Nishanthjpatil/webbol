*> File handling data structures and flags
*> Buffer to store entire file content (64KB maximum)
       01 FILE-BUFFER          PIC X(65536).
*> Actual size of file in bytes (binary for system compatibility)
       01 FILE-SIZE            PIC 9(8) COMP-5.
*> Original file path from HTTP request
       01 FILE-PATH            PIC X(512).
*> Security-validated path safe for file system access
       01 SANITIZED-PATH       PIC X(512).
*> MIME content type determined from file extension
       01 MIME-TYPE            PIC X(64).
*> File extension extracted for MIME type lookup
       01 FILE-EXTENSION       PIC X(10).
*> Flag indicating whether requested file exists
       01 FILE-EXISTS-FLAG     PIC 9 VALUE 0.
*> Condition names for readable file existence checking
          88 FILE-EXISTS       VALUE 1.
          88 FILE-NOT-FOUND    VALUE 0.
