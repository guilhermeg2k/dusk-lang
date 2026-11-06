const rows = [];
for (let i = 0; i < 500000; i++) {
  rows.push(`${i},${(i * 7) % 1000},item${i}`);
}
const csv = rows.join("\n");
let total = 0;
let count = 0;
for (const row of csv.split("\n")) {
  const fields = row.split(",");
  total += parseInt(fields[0], 10) + parseInt(fields[1], 10);
  count += 1;
}
console.log(total + count);
