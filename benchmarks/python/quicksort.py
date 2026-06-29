def swap(a, i, j):
    a[i], a[j] = a[j], a[i]


def quicksort(a, lo, hi):
    if lo >= hi:
        return

    pivot = a[(lo + hi) // 2]
    i = lo
    j = hi

    while i <= j:
        while a[i] < pivot:
            i += 1
        while a[j] > pivot:
            j -= 1
        if i <= j:
            swap(a, i, j)
            i += 1
            j -= 1

    quicksort(a, lo, j)
    quicksort(a, i, hi)


n = 10000
data = [n - i for i in range(n)]

quicksort(data, 0, n - 1)
print(data[5000])
