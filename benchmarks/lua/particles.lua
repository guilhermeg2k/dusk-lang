local N = 100000
local particles = {}

for i = 1, N do
  local k = i - 1
  particles[i] = { x = k, y = k * 2.0, vx = 0.5, vy = -0.5 }
end

for _ = 1, 100 do
  for i = 1, N do
    local p = particles[i]
    p.x = p.x + p.vx
    p.y = p.y + p.vy
  end
end

print(particles[50000].x)
