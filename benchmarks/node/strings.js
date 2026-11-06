const name = "dusk";
let total = 0;
for (let i = 0; i < 1000000; i++) {
  const line = `item ${i}: value=${i * 3} tag=${name}`;
  const fields = line.split(" ");
  const joined = fields.join(",");
  total += line.length + fields.length + joined.length;
}
console.log(total);
