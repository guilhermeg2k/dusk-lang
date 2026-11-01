rows = []
for i in range(500000):
    rows.append(f"{i},{(i * 7) % 1000},item{i}")
csv = "\n".join(rows)
total = 0
count = 0
for row in csv.split("\n"):
    fields = row.split(",")
    total += int(fields[0]) + int(fields[1])
    count += 1
print(total + count)
