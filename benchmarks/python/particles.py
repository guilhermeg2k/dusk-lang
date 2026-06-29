N = 100000

particles = [
    {"x": float(i), "y": i * 2.0, "vx": 0.5, "vy": -0.5}
    for i in range(N)
]

for _ in range(100):
    for p in particles:
        p["x"] += p["vx"]
        p["y"] += p["vy"]

print(particles[49999]["x"])
