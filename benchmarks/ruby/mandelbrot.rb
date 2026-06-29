W = 800
H = 800
MAX_ITER = 1000

sum = 0
H.times do |py|
  cy = -1.0 + (py.to_f / H) * 2.0
  W.times do |px|
    cx = -2.5 + (px.to_f / W) * 3.5

    zx = 0.0
    zy = 0.0
    zx2 = 0.0
    zy2 = 0.0
    iter = 0

    while zx2 + zy2 <= 4.0 && iter < MAX_ITER
      zy = 2.0 * zx * zy + cy
      zx = zx2 - zy2 + cx
      zx2 = zx * zx
      zy2 = zy * zy
      iter += 1
    end

    sum += iter
  end
end

puts sum
