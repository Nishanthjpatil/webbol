*> File structures copybook
       01 FILE-BUFFER          PIC X(65536).
       01 FILE-SIZE            PIC 9(8) COMP-5.
       01 FILE-PATH            PIC X(512).
       01 SANITIZED-PATH       PIC X(512).
       01 MIME-TYPE            PIC X(64).
       01 FILE-EXTENSION       PIC X(10).
       01 FILE-EXISTS-FLAG     PIC 9 VALUE 0.
          88 FILE-EXISTS       VALUE 1.
          88 FILE-NOT-FOUND    VALUE 0.
