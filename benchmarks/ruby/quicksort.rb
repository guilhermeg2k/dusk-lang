def swap(a, i, j)
  a[i], a[j] = a[j], a[i]
end

def quicksort(a, lo, hi)
  return if lo >= hi

  pivot = a[(lo + hi) / 2]
  i = lo
  j = hi

  while i <= j
    i += 1 while a[i] < pivot
    j -= 1 while a[j] > pivot
    if i <= j
      swap(a, i, j)
      i += 1
      j -= 1
    end
  end

  quicksort(a, lo, j)
  quicksort(a, i, hi)
end

n = 10_000
data = Array.new(n) { |i| n - i }

quicksort(data, 0, n - 1)
puts data[5000]
