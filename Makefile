# Makefile for COBOL Web Server
# Uses GnuCOBOL compiler

CC = cobc
CFLAGS = -free -x
CFLAGS_OBJ = -free -c

# Copybook directory
COPYPATH = -I.

# Target executable
TARGET = webserver

# Module object files
MODULES = path-utils.o mime-types.o file-ops.o http-handler.o url-decode.o

# Default target
all: $(TARGET)

# Compile modules to object files
path-utils.o: path-utils.cbl
	$(CC) $(CFLAGS_OBJ) $(COPYPATH) path-utils.cbl

mime-types.o: mime-types.cbl
	$(CC) $(CFLAGS_OBJ) $(COPYPATH) mime-types.cbl

file-ops.o: file-ops.cbl
	$(CC) $(CFLAGS_OBJ) $(COPYPATH) file-ops.cbl

http-handler.o: http-handler.cbl http-structs.cpy file-structs.cpy
	$(CC) $(CFLAGS_OBJ) $(COPYPATH) http-handler.cbl

url-decode.o: url-decode.cbl
	$(CC) $(CFLAGS_OBJ) $(COPYPATH) url-decode.cbl

# Compile main program and link with modules
$(TARGET): webserver.cbl $(MODULES) config.cpy socket-defs.cpy http-structs.cpy
	$(CC) $(CFLAGS) $(COPYPATH) webserver.cbl $(MODULES) -o $(TARGET)

# Clean build artifacts
clean:
	rm -f $(TARGET) *.so *.dylib *.o *.c *.c.l.h *.c.h

# Run the server
run: $(TARGET)
	./$(TARGET)

.PHONY: all clean run
