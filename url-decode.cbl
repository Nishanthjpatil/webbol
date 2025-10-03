*> URL decoding module - converts %XX encoded characters to actual characters
IDENTIFICATION DIVISION.
       PROGRAM-ID. URL-DECODE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
*> Index for scanning input string
       01 WS-IN-INDEX          PIC 9(4) COMP.
*> Index for building output string
       01 WS-OUT-INDEX         PIC 9(4) COMP.
*> Length of input string
       01 WS-LENGTH            PIC 9(4) COMP.
*> Current character being examined
       01 WS-CHAR              PIC X.
*> Two-character hex code from URL encoding
       01 WS-HEX-CODE          PIC XX.
*> Numeric value of hex code
       01 WS-HEX-VALUE         PIC 9(4) COMP.
*> Character representation of decoded value
       01 WS-DECODED-CHAR      PIC X.

*> Parameters passed from calling program
       LINKAGE SECTION.
*> URL-encoded input path (may contain %20, etc.)
       01 LS-INPUT-PATH        PIC X(512).
*> Decoded output path (with actual characters)
       01 LS-OUTPUT-PATH       PIC X(512).

       PROCEDURE DIVISION USING LS-INPUT-PATH LS-OUTPUT-PATH.

*> Main URL decoding logic
       MAIN-LOGIC.
*> Initialize output path to spaces and reset position indices
           MOVE SPACES TO LS-OUTPUT-PATH
           MOVE 1 TO WS-IN-INDEX
           MOVE 1 TO WS-OUT-INDEX

*> Calculate actual length of input path (excluding trailing spaces)
           INSPECT LS-INPUT-PATH TALLYING WS-LENGTH
               FOR CHARACTERS BEFORE INITIAL SPACE

*> Handle empty input path by returning immediately
           IF WS-LENGTH = 0
               GOBACK
           END-IF

*> Process each character in input path one at a time
*> URL encoding uses %XX format where XX is hexadecimal ASCII code
           PERFORM UNTIL WS-IN-INDEX > WS-LENGTH
               MOVE LS-INPUT-PATH(WS-IN-INDEX:1) TO WS-CHAR

*> Check if current character is % (start of URL-encoded sequence)
               IF WS-CHAR = "%"
*> Ensure we have at least 2 more characters for hex code (e.g. %20)
                   IF WS-IN-INDEX + 2 <= WS-LENGTH
*> Extract the two-character hex code following the %
                       MOVE LS-INPUT-PATH(WS-IN-INDEX + 1:2)
                           TO WS-HEX-CODE
*> Convert hex code to actual character (e.g. "20" becomes space)
                       PERFORM DECODE-HEX-CHAR
*> Write decoded character to output at current position
                       MOVE WS-DECODED-CHAR TO
                           LS-OUTPUT-PATH(WS-OUT-INDEX:1)
                       ADD 1 TO WS-OUT-INDEX
*> Skip past the % and two hex digits (3 characters total)
                       ADD 3 TO WS-IN-INDEX
                   ELSE
*> Invalid encoding (% without two hex digits), copy % as-is
                       MOVE WS-CHAR TO LS-OUTPUT-PATH(WS-OUT-INDEX:1)
                       ADD 1 TO WS-OUT-INDEX
                       ADD 1 TO WS-IN-INDEX
                   END-IF
               ELSE
*> Regular unencoded character, copy directly to output
                   MOVE WS-CHAR TO LS-OUTPUT-PATH(WS-OUT-INDEX:1)
                   ADD 1 TO WS-OUT-INDEX
                   ADD 1 TO WS-IN-INDEX
               END-IF
           END-PERFORM

           GOBACK.

*> Convert two-character hex code to actual character
*> Handles common URL-encoded special characters used in web requests
       DECODE-HEX-CHAR.
*> Initialize to space as default for unrecognized hex codes
           MOVE SPACE TO WS-DECODED-CHAR

*> Decode common URL encodings explicitly
*> Most common: %20 = space character (used in filenames and URLs)
           IF WS-HEX-CODE = "20"
               MOVE SPACE TO WS-DECODED-CHAR
*> %21 = ! (exclamation mark)
           ELSE IF WS-HEX-CODE = "21"
               MOVE "!" TO WS-DECODED-CHAR
*> %22 = " (double quote)
           ELSE IF WS-HEX-CODE = "22"
               MOVE '"' TO WS-DECODED-CHAR
*> %23 = # (hash/pound sign)
           ELSE IF WS-HEX-CODE = "23"
               MOVE "#" TO WS-DECODED-CHAR
*> %24 = $ (dollar sign)
           ELSE IF WS-HEX-CODE = "24"
               MOVE "$" TO WS-DECODED-CHAR
*> %25 = % (percent sign)
           ELSE IF WS-HEX-CODE = "25"
               MOVE "%" TO WS-DECODED-CHAR
*> %26 = & (ampersand)
           ELSE IF WS-HEX-CODE = "26"
               MOVE "&" TO WS-DECODED-CHAR
*> %27 = ' (single quote)
           ELSE IF WS-HEX-CODE = "27"
               MOVE "'" TO WS-DECODED-CHAR
*> %28 = ( (left parenthesis)
           ELSE IF WS-HEX-CODE = "28"
               MOVE "(" TO WS-DECODED-CHAR
*> %29 = ) (right parenthesis)
           ELSE IF WS-HEX-CODE = "29"
               MOVE ")" TO WS-DECODED-CHAR
*> %2B = + (plus sign)
           ELSE IF WS-HEX-CODE = "2B" OR WS-HEX-CODE = "2b"
               MOVE "+" TO WS-DECODED-CHAR
*> %2C = , (comma)
           ELSE IF WS-HEX-CODE = "2C" OR WS-HEX-CODE = "2c"
               MOVE "," TO WS-DECODED-CHAR
*> %2D = - (hyphen/minus)
           ELSE IF WS-HEX-CODE = "2D" OR WS-HEX-CODE = "2d"
               MOVE "-" TO WS-DECODED-CHAR
*> %2E = . (period/dot)
           ELSE IF WS-HEX-CODE = "2E" OR WS-HEX-CODE = "2e"
               MOVE "." TO WS-DECODED-CHAR
*> %2F = / (forward slash)
           ELSE IF WS-HEX-CODE = "2F" OR WS-HEX-CODE = "2f"
               MOVE "/" TO WS-DECODED-CHAR
*> %3A = : (colon)
           ELSE IF WS-HEX-CODE = "3A" OR WS-HEX-CODE = "3a"
               MOVE ":" TO WS-DECODED-CHAR
*> %3B = ; (semicolon)
           ELSE IF WS-HEX-CODE = "3B" OR WS-HEX-CODE = "3b"
               MOVE ";" TO WS-DECODED-CHAR
*> %3D = = (equals sign)
           ELSE IF WS-HEX-CODE = "3D" OR WS-HEX-CODE = "3d"
               MOVE "=" TO WS-DECODED-CHAR
*> %3F = ? (question mark)
           ELSE IF WS-HEX-CODE = "3F" OR WS-HEX-CODE = "3f"
               MOVE "?" TO WS-DECODED-CHAR
*> %40 = @ (at sign)
           ELSE IF WS-HEX-CODE = "40"
               MOVE "@" TO WS-DECODED-CHAR
*> %5B = [ (left square bracket)
           ELSE IF WS-HEX-CODE = "5B" OR WS-HEX-CODE = "5b"
               MOVE "[" TO WS-DECODED-CHAR
*> %5D = ] (right square bracket)
           ELSE IF WS-HEX-CODE = "5D" OR WS-HEX-CODE = "5d"
               MOVE "]" TO WS-DECODED-CHAR
           ELSE
*> Unrecognized hex code - default to space for safety
               MOVE SPACE TO WS-DECODED-CHAR
           END-IF
           .
