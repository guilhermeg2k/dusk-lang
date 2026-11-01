rows = []
500_000.times do |i|
  rows << "#{i},#{(i * 7) % 1000},item#{i}"
end
csv = rows.join("\n")
total = 0
count = 0
csv.split("\n").each do |row|
  fields = row.split(",")
  total += Integer(fields[0]) + Integer(fields[1])
  count += 1
end
puts total + count
