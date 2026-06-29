local LIMIT = 100000

local sieve = {}
for i = 0, LIMIT do
  sieve[i] = true
end
sieve[0] = false
sieve[1] = false

local p = 2
while p * p <= LIMIT do
  if sieve[p] then
    local m = p * p
    while m <= LIMIT do
      sieve[m] = false
      m = m + p
    end
  end
  p = p + 1
end

local count = 0
for k = 2, LIMIT do
  if sieve[k] then
    count = count + 1
  end
end

print(count)
