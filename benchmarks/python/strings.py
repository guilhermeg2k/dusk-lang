name = "dusk"
total = 0
for i in range(1000000):
    line = f"item {i}: value={i * 3} tag={name}"
    fields = line.split(" ")
    joined = ",".join(fields)
    total += len(line) + len(fields) + len(joined)
print(total)
