# Problema 1.
# Fie un array de numere. Sa se scrie un algoritm paralel de sortare a acestor numere. \

# Solutie: sortare prin model de concurenta producer-consumer
# am nr de consumeri = nr de workeri care sa sorteze array-urile
# spor!
# Subiect Python: Sa se sorteze un array, folosind un algoritm concurent.

# Problema 2.
# Scrieti un program care sa citeasca fisiere json intr-o ierarhie de foldere, in paralel
# tot printr-un producer-consumer.
# Exista multipli produceri, care trebuie sa se sincronizeze sa parcurga ierarhia de fisiere,
# dar sa nu proceseze de mai multe ori acelasi fisier json.
# Un fisier este transmis de producer intr-o zona de memorie partajata, iar consumerii preiau json-ul respectiv
# si il deserializeaza, afisand informatiile din valorile acestui obiect.
# Spor.

# Deadline cerut de colegii vostri de la 461: 10.11.2024 ora 23:59.
# bogdan.macovei@unibuc.ro

from concurrent.futures import ThreadPoolExecutor
#import threading
import time
#import numpy as np
import random

def clock():
    return time.perf_counter()
#    return time.clock_gettime(time.CLOCK_MONOTONIC)

executor = ThreadPoolExecutor(max_workers=70)

def merge(left, right):
    result = []
    i = j = 0

    while i < len(left) and j < len(right):
        if left[i] <= right[j]:
            result.append(left[i])
            i += 1
        else:
            result.append(right[j])
            j += 1

    result.extend(left[i:])
    result.extend(right[j:])
    return result

def merge_sort(arr):
    if len(arr) <= 1:
        return arr

    mid = len(arr) // 2
    left = arr[:mid]
    right = arr[mid:]

    left_sorted = executor.submit(merge_sort, left)
    right_sorted = executor.submit(merge_sort, right)

    left = left_sorted.result()
    right = right_sorted.result()

    return merge(left, right)

def main():
    # k = 1000 => deadlock !!!
    arr = random.choices(range(1000), k=100)
    start_time = clock()
    sorted_arr = merge_sort(arr)
    end_time = clock()
    print(f"Time: {end_time - start_time}")
    assert sorted(arr) == sorted_arr
    print(sorted_arr)

if __name__ == "__main__":
    main()
