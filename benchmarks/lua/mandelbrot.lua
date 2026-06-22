local W = 800
local H = 800
local MAX_ITER = 1000

local sum = 0
for py = 0, H - 1 do
  local cy = -1.0 + (py / H) * 2.0
  for px = 0, W - 1 do
    local cx = -2.5 + (px / W) * 3.5

    local zx = 0.0
    local zy = 0.0
    local zx2 = 0.0
    local zy2 = 0.0
    local iter = 0

    while zx2 + zy2 <= 4.0 and iter < MAX_ITER do
      zy = 2.0 * zx * zy + cy
      zx = zx2 - zy2 + cx
      zx2 = zx * zx
      zy2 = zy * zy
      iter = iter + 1
    end

    sum = sum + iter
  end
end

print(sum)
