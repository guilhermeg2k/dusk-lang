function swap(a, i, j) {
  const t = a[i];
  a[i] = a[j];
  a[j] = t;
}

function quicksort(a, lo, hi) {
  if (lo >= hi) return;

  const pivot = a[(lo + hi) >> 1];
  let i = lo;
  let j = hi;

  while (i <= j) {
    while (a[i] < pivot) i++;
    while (a[j] > pivot) j--;
    if (i <= j) {
      swap(a, i, j);
      i++;
      j--;
    }
  }

  quicksort(a, lo, j);
  quicksort(a, i, hi);
}

const n = 10000;
const data = new Array(n);
for (let i = 0; i < n; i++) {
  data[i] = n - i;
}

quicksort(data, 0, n - 1);
console.log(data[5000]);
