from threading import Lock, Condition, Thread
from random import randint, random
from time import sleep
from dataclasses import dataclass

print_lock = Lock()

def safe_print(msg):
    with print_lock:
        print(msg)

@dataclass
class Buffer:
    buffer: list[int | None]
    filled_len: int
    no_longer_full: Condition
    no_longer_empty: Condition
    lock: Lock

def new_buffer(capacity: int) -> Buffer:
    buffer: list[int | None] = [None for _ in range(capacity)]
    lock = Lock()
    no_longer_full = Condition(lock)
    no_longer_empty = Condition(lock)
    return Buffer(buffer, 0, no_longer_full, no_longer_empty, lock)

def producer(buffer: Buffer, possible_types: list[int], num_to_generate: int):
    for i in range(num_to_generate + 1):
        sleep(random() / 2)
        with buffer.lock:
            while buffer.filled_len == len(buffer.buffer):
                safe_print("Buffer is full. Producer waiting...")
                buffer.no_longer_full.wait()
            slot = buffer.filled_len
            safe_print(f"Free slot {slot} in buffer found. Generating object...")
            sleep(random())
            generated = None
            if i != num_to_generate:
                generated = possible_types[randint(0, len(possible_types) - 1)]
                safe_print(f"Generated {generated}")
            else:
                safe_print("Last iteration. Sending None to signal consumer to stop.")
            buffer.buffer[slot] = generated
            buffer.filled_len += 1
            if slot == 0:
                buffer.no_longer_empty.notify_all()

def consumer(buffer: Buffer, allowed_types: list[int]):
    processed = 0
    producer_stopped = False
    while True:
        sleep(random())
        obj = None
        with buffer.lock:
            while buffer.filled_len == 0:
                if producer_stopped:
                    safe_print("Buffer is empty and producer is stopped. Exiting consumer.")
                    return
                safe_print("Buffer is empty. Consumer waiting...")
                buffer.no_longer_empty.wait()
            slot = buffer.filled_len - 1
            safe_print(f"Taking object from slot {slot}")
            obj = buffer.buffer[slot]
            buffer.buffer[slot] = None
            buffer.filled_len -= 1
            if buffer.filled_len == len(buffer.buffer) - 1:
                buffer.no_longer_full.notify_all()
        if obj is None:
            safe_print("Got None from producer. Will stop after processing remaining items.")
            assert not producer_stopped
            producer_stopped = True
            continue
        safe_print(f"Got {obj} from buffer. Processing...")
        assert obj in allowed_types
        sleep(random())
        processed += 1
        safe_print(f"Processed {processed} objects so far.")

CAPACITY = 5
POSSIBLE_TYPES = ["Apple", "Orange"]
NUM_TO_GENERATE = 100

def main():
    buffer = new_buffer(CAPACITY)
    p_thread = Thread(target=producer, args=(buffer, POSSIBLE_TYPES, NUM_TO_GENERATE))
    c_thread = Thread(target=consumer, args=(buffer, POSSIBLE_TYPES))
    p_thread.start()
    c_thread.start()
    p_thread.join()
    c_thread.join()

if __name__ == "__main__":
    main()
