LIMIT = 100000

sieve = [True] * (LIMIT + 1)
sieve[0] = False
sieve[1] = False

p = 2
while p * p <= LIMIT:
    if sieve[p]:
        for m in range(p * p, LIMIT + 1, p):
            sieve[m] = False
    p += 1

count = 0
for k in range(2, LIMIT + 1):
    if sieve[k]:
        count += 1

print(count)
