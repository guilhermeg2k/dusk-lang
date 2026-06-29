const N = 100000;
const particles = new Array(N);

for (let i = 0; i < N; i++) {
  particles[i] = { x: i, y: i * 2, vx: 0.5, vy: -0.5 };
}

for (let step = 0; step < 100; step++) {
  for (let i = 0; i < N; i++) {
    const p = particles[i];
    p.x += p.vx;
    p.y += p.vy;
  }
}

console.log(particles[49999].x);
