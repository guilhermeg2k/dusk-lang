local function swap(a, i, j)
  a[i], a[j] = a[j], a[i]
end

local function quicksort(a, lo, hi)
  if lo >= hi then
    return
  end

  local pivot = a[(lo + hi) // 2]
  local i = lo
  local j = hi

  while i <= j do
    while a[i] < pivot do
      i = i + 1
    end
    while a[j] > pivot do
      j = j - 1
    end
    if i <= j then
      swap(a, i, j)
      i = i + 1
      j = j - 1
    end
  end

  quicksort(a, lo, j)
  quicksort(a, i, hi)
end

local n = 10000
local data = {}
for i = 1, n do
  data[i] = n - i + 1
end

quicksort(data, 1, n)
print(data[5001])
