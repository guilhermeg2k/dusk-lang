name = "dusk"
total = 0
1_000_000.times do |i|
  line = "item #{i}: value=#{i * 3} tag=#{name}"
  fields = line.split(" ")
  joined = fields.join(",")
  total += line.length + fields.length + joined.length
end
puts total
