const W = 800;
const H = 800;
const MAX_ITER = 1000;

let sum = 0;
for (let py = 0; py < H; py++) {
  const cy = -1.0 + (py / H) * 2.0;
  for (let px = 0; px < W; px++) {
    const cx = -2.5 + (px / W) * 3.5;

    let zx = 0.0;
    let zy = 0.0;
    let zx2 = 0.0;
    let zy2 = 0.0;
    let iter = 0;

    while (zx2 + zy2 <= 4.0 && iter < MAX_ITER) {
      zy = 2.0 * zx * zy + cy;
      zx = zx2 - zy2 + cx;
      zx2 = zx * zx;
      zy2 = zy * zy;
      iter++;
    }

    sum += iter;
  }
}

console.log(sum);
