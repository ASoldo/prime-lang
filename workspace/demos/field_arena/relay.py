"""Connect an adapter's stdio child interface to its arena's private Unix socket."""
import os
import selectors
import socket
import sys


def main(path):
    with socket.socket(socket.AF_UNIX) as link, selectors.DefaultSelector() as ready:
        link.connect(path)
        ready.register(0, selectors.EVENT_READ)
        ready.register(link, selectors.EVENT_READ)
        while True:
            for key, _ in ready.select():
                if key.fileobj == 0:
                    data = os.read(0, 8192)
                    if not data:
                        return
                    link.sendall(data)
                else:
                    data = link.recv(8192)
                    if not data:
                        return
                    sys.stdout.buffer.write(data)
                    sys.stdout.buffer.flush()


if __name__ == "__main__":
    main(sys.argv[1])
