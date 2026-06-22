LIMIT = 100_000

sieve = Array.new(LIMIT + 1, true)
sieve[0] = false
sieve[1] = false

p = 2
while p * p <= LIMIT
  if sieve[p]
    m = p * p
    while m <= LIMIT
      sieve[m] = false
      m += p
    end
  end
  p += 1
end

count = 0
(2..LIMIT).each do |k|
  count += 1 if sieve[k]
end

puts count
