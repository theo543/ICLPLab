from pathlib import Path
from queue import Queue
from sys import argv
from threading import Thread, Barrier, Lock
import json

def producer(dirs: Queue[Path | None], files: Queue[Path | None], tracker: Queue[int]):
    files_found = 0
    while True:
        directory = dirs.get()
        if directory is None:
            dirs.put(None)
            tracker.put(files_found)
            return
        for entry in directory.iterdir():
            if entry.is_dir():
                tracker.put(1)
                dirs.put(entry)
            else:
                files_found += 1
                files.put(entry)
        tracker.put(-1)

def consumer(files: Queue[Path | None], shutdown: Barrier, print_lock: Lock):
    while True:
        file = files.get()
        if file is None:
            files.put(None)
            shutdown.wait()
            return
        try:
            data = json.loads(file.read_bytes())
        except (json.JSONDecodeError, UnicodeDecodeError, UnicodeError):
            data = f"Could not decode {file.absolute()}"
        with print_lock:
            print(data)

PRODUCERS = 10
CONSUMERS = 10

def main():
    assert len(argv) == 2
    root = Path(argv[1])
    assert root.is_dir()

    dirs: Queue[Path | None] = Queue()
    files: Queue[Path | None] = Queue()
    tracker: Queue[int] = Queue()
    consumer_shutdown = Barrier(CONSUMERS + 1)
    print_lock = Lock()

    for _ in range(PRODUCERS):
        t = Thread(target=producer, args=(dirs, files, tracker))
        t.start()

    for _ in range(CONSUMERS):
        t = Thread(target=consumer, args=(files, consumer_shutdown, print_lock))
        t.start()

    dirs.put(root)

    total_dirs = 1
    active_dirs = 1
    while active_dirs > 0:
        change = tracker.get()
        active_dirs += change
        if change == 1:
            total_dirs += 1

    dirs.put(None)
    files.put(None)

    total_files = 0
    for _ in range(PRODUCERS):
        total_files += tracker.get()

    consumer_shutdown.wait()

    print(f"Total files: {total_files}")
    print(f"Total directories: {total_dirs}")

if __name__ == "__main__":
    main()
