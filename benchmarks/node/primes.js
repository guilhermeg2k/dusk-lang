const LIMIT = 100000;
const sieve = new Array(LIMIT + 1).fill(true);
sieve[0] = false;
sieve[1] = false;

for (let p = 2; p * p <= LIMIT; p++) {
  if (sieve[p]) {
    for (let m = p * p; m <= LIMIT; m += p) {
      sieve[m] = false;
    }
  }
}

let count = 0;
for (let k = 2; k <= LIMIT; k++) {
  if (sieve[k]) count++;
}

console.log(count);
