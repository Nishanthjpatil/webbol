IDENTIFICATION DIVISION.
       PROGRAM-ID. MIME-TYPES.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-INDEX             PIC 9(4) COMP.
       01 WS-LENGTH            PIC 9(4) COMP.
       01 WS-EXT-START         PIC 9(4) COMP.
       
       LINKAGE SECTION.
       01 LS-FILE-PATH         PIC X(512).
       01 LS-MIME-TYPE         PIC X(64).
       
       PROCEDURE DIVISION USING LS-FILE-PATH LS-MIME-TYPE.
       
       MAIN-LOGIC.
           MOVE "application/octet-stream" TO LS-MIME-TYPE
           
           MOVE 0 TO WS-EXT-START
           INSPECT LS-FILE-PATH TALLYING WS-LENGTH 
               FOR CHARACTERS BEFORE INITIAL SPACE
           
           PERFORM VARYING WS-INDEX FROM WS-LENGTH BY -1 
               UNTIL WS-INDEX < 1
               IF LS-FILE-PATH(WS-INDEX:1) = "."
                   COMPUTE WS-EXT-START = WS-INDEX + 1
                   EXIT PERFORM
               END-IF
           END-PERFORM
           
           IF WS-EXT-START = 0
               GOBACK
           END-IF
           
           EVALUATE LS-FILE-PATH(WS-EXT-START:4)
               WHEN "html"
                   MOVE "text/html" TO LS-MIME-TYPE
               WHEN "htm "
                   MOVE "text/html" TO LS-MIME-TYPE
               WHEN "css "
                   MOVE "text/css" TO LS-MIME-TYPE
               WHEN "js  "
                   MOVE "application/javascript" TO LS-MIME-TYPE
               WHEN "json"
                   MOVE "application/json" TO LS-MIME-TYPE
               WHEN "xml "
                   MOVE "application/xml" TO LS-MIME-TYPE
               WHEN "txt "
                   MOVE "text/plain" TO LS-MIME-TYPE
               WHEN "png "
                   MOVE "image/png" TO LS-MIME-TYPE
               WHEN "jpg "
                   MOVE "image/jpeg" TO LS-MIME-TYPE
               WHEN "jpeg"
                   MOVE "image/jpeg" TO LS-MIME-TYPE
               WHEN "gif "
                   MOVE "image/gif" TO LS-MIME-TYPE
               WHEN "svg "
                   MOVE "image/svg+xml" TO LS-MIME-TYPE
               WHEN "ico "
                   MOVE "image/x-icon" TO LS-MIME-TYPE
               WHEN "pdf "
                   MOVE "application/pdf" TO LS-MIME-TYPE
           END-EVALUATE
           
           GOBACK.
