N = 100_000

particles = Array.new(N) do |i|
  { x: i.to_f, y: i * 2.0, vx: 0.5, vy: -0.5 }
end

100.times do
  particles.each do |p|
    p[:x] += p[:vx]
    p[:y] += p[:vy]
  end
end

puts particles[49_999][:x]
