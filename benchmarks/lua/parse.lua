local rows = {}
for i = 0, 499999 do
  rows[#rows + 1] = i .. "," .. ((i * 7) % 1000) .. ",item" .. i
end
local csv = table.concat(rows, "\n")
local total = 0
local count = 0
for row in string.gmatch(csv, "[^\n]+") do
  local a, b = string.match(row, "^([^,]*),([^,]*),")
  total = total + tonumber(a) + tonumber(b)
  count = count + 1
end
print(total + count)
