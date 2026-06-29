W = 800
H = 800
MAX_ITER = 1000

total = 0
for py in range(H):
    cy = -1.0 + (py / H) * 2.0
    for px in range(W):
        cx = -2.5 + (px / W) * 3.5

        zx = 0.0
        zy = 0.0
        zx2 = 0.0
        zy2 = 0.0
        it = 0

        while zx2 + zy2 <= 4.0 and it < MAX_ITER:
            zy = 2.0 * zx * zy + cy
            zx = zx2 - zy2 + cx
            zx2 = zx * zx
            zy2 = zy * zy
            it += 1

        total += it

print(total)
