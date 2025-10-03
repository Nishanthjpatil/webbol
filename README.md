# Webbol

A minimal static web server written in COBOL using GnuCOBOL.

## Features

- Serves static files from the current directory
- Automatic MIME type detection for common file types
- HTTP status codes: 200 (OK), 403 (Forbidden), 404 (Not Found), 413 (Payload Too Large)
- Path traversal attack prevention
- Clean request logging with full HTTP headers
- Defaults to `index.html` for root path requests

## Requirements

- GnuCOBOL (cobc) compiler
- POSIX-compatible operating system (Linux, macOS, BSD)
- make

### Installing GnuCOBOL

**macOS:**
```bash
brew install gnucobol
```

**Ubuntu/Debian:**
```bash
sudo apt-get install gnucobol
```

**Fedora/RHEL:**
```bash
sudo dnf install gnucobol
```

## Building

Clone or download the repository, then compile:

```bash
make
```

This will compile all modules and create the `webserver` executable.

To clean build artifacts:

```bash
make clean
```

## Usage

Start the server from the directory you want to serve:

```bash
./webserver
```

The server will start on port 8080 and serve files from the current directory.

### Example

```bash
# Create a test HTML file
echo "<html><body><h1>Hello from COBOL!</h1></body></html>" > index.html

# Start the server
./webserver

# In another terminal, test it
curl http://localhost:8080/
```

### Accessing the Server

Once running, you can access files via:

- `http://localhost:8080/` - serves `index.html` from the current directory
- `http://localhost:8080/filename.html` - serves the specified file
- `http://localhost:8080/path/to/file.txt` - serves files from subdirectories

Press `Ctrl+C` to stop the server.

## Configuration

To change the server port, edit `config.cpy` and modify the `SERVER-PORT` value:

```cobol
01 SERVER-PORT          PIC 9(5) VALUE 8080.
```

Then recompile with `make`.

## Project Structure

```
webbol/
├── Makefile              # Build configuration
├── README.md            # This file
├── config.cpy           # Server configuration
├── socket-defs.cpy      # Socket structure definitions
├── http-structs.cpy     # HTTP data structures
├── file-structs.cpy     # File handling structures
├── path-utils.cbl       # Path validation and sanitization
├── mime-types.cbl       # MIME type detection
├── file-ops.cbl         # File reading operations
├── http-handler.cbl     # HTTP request/response handling
└── webserver.cbl        # Main server program
```

## Supported MIME Types

- HTML: `text/html`
- CSS: `text/css`
- JavaScript: `application/javascript`
- JSON: `application/json`
- XML: `application/xml`
- Plain text: `text/plain`
- PNG: `image/png`
- JPEG: `image/jpeg`
- GIF: `image/gif`
- SVG: `image/svg+xml`
- ICO: `image/x-icon`
- PDF: `application/pdf`

Additional MIME types can be added by editing `mime-types.cbl`.

## Security Features

- Path traversal prevention: Blocks requests containing `..` sequences
- Directory access restriction: Only serves files from the current directory and subdirectories
- Safe file handling: Validates all paths before file system access

## Limitations

- Single-threaded: Handles one request at a time
- No SSL/TLS support
- Maximum file size: 64KB
- Line sequential file organization only (text files)
- No caching or compression
- No range requests or partial content support

## Troubleshooting

**Port already in use:**
```
Bind failed - check if port is in use
```
Another process is using port 8080. Either stop that process or change the port in `config.cpy`.

**Permission denied:**
Ensure the files you're trying to serve have read permissions and the current user can access them.

**File not found (404):**
Verify the file exists in the current directory where the server is running. File paths are case-sensitive.

## License

This project is released into the public domain. Use it however you'd like.

## Acknowledgments

Built with GnuCOBOL, demonstrating that COBOL can still be used for modern systems programming tasks.
