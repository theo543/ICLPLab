from multiprocessing.managers import SharedMemoryManager
from multiprocessing.shared_memory import SharedMemory
from asyncio import new_event_loop, gather, AbstractEventLoop
from concurrent.futures import ProcessPoolExecutor
from time import perf_counter
import numpy as np

def _merge(mid: int, out: np.ndarray):
    tmp = out.copy()
    o = 0
    l = 0
    r = mid + 1
    while o <= len(out):
        if l > mid:
            out[o:] = tmp[r:]
            break
        if r == len(out):
            out[o:] = tmp[l:mid+1]
            break
        if tmp[l] > tmp[r]:
            out[o] = tmp[r]
            o += 1
            r += 1
        else:
            out[o] = tmp[l]
            o += 1
            l += 1

def _merge_wrapper(left: int, mid: int, right: int, sm_name: str, sm_size: int):
    sm = SharedMemory(name=sm_name, create=False, size=sm_size)
    out = np.ndarray((sm_size // 8,), dtype=np.int64, buffer=sm.buf)
    _merge(mid - left, out[left:right+1])

def _insertion_sort(array: np.ndarray):
    i = 1
    while i < len(array):
        j = i
        while j > 0 and array[j - 1] > array[j]:
            array[j - 1], array[j] = array[j], array[j - 1]
            j = j - 1
        i = i + 1

async def _merge_sort(left: int, right: int, sm: SharedMemory, array: np.ndarray, proc_pool: ProcessPoolExecutor, loop: AbstractEventLoop):
    assert left <= right
    if left == right:
        return
    if (right - left + 1) <= 32:
        _insertion_sort(array[left:right+1])
        return
    mid = (left + right) // 2
    await gather(
        _merge_sort(left, mid, sm, array, proc_pool, loop),
        _merge_sort(mid + 1, right, sm, array, proc_pool, loop)
    )
    await loop.run_in_executor(proc_pool, _merge_wrapper, left, mid, right, sm.name, sm.size)

async def merge_sort(in_array: np.ndarray, proc_pool: ProcessPoolExecutor, smm: SharedMemoryManager, loop: AbstractEventLoop):
    assert in_array.dtype == np.int64
    sm = smm.SharedMemory(8 * len(in_array))
    array = np.ndarray((len(in_array),), dtype=np.int64, buffer=sm.buf)
    np.copyto(array, in_array)
    await _merge_sort(0, len(array) - 1, sm, array, proc_pool, loop)
    return array.copy()

def main():
    loop = new_event_loop()
    array = np.random.randint(-2**63, 2**63, size=1_000_000, dtype=np.int64)
    with ProcessPoolExecutor() as proc_pool, SharedMemoryManager() as smm:
        start = perf_counter()
        sorted_array = loop.run_until_complete(merge_sort(array, proc_pool, smm, loop))
        duration = perf_counter() - start
    assert np.array_equal(sorted_array, np.sort(array))
    print(duration)

if __name__ == "__main__":
    main()
