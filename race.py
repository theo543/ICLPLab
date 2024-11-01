import threading
import os

REPEATS = 5000000
THREADS = os.cpu_count() or 10
if (THREADS % 2) == 1:
    THREADS += 1

barrier = threading.Barrier(THREADS, action=lambda: print(f"{THREADS} threads running..."))

counter = 0

def add(increment):
    global counter # pylint: disable=global-statement
    barrier.wait()
    for _ in range(REPEATS):
        counter = counter + increment

def main():
    threads = []

    for i in range(1, THREADS // 2 + 1):
        threads.append(threading.Thread(target=add, args=(i,)))
        threads.append(threading.Thread(target=add, args=(-i,)))

    for thread in threads:
        thread.start()

    for thread in threads:
        thread.join()

    # race might be impossible because of GIL?
    assert counter == 0

    print("No race detected")

if __name__ == "__main__":
    main()
