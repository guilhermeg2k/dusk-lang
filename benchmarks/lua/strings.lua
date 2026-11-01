local name = "dusk"
local total = 0
for i = 0, 999999 do
  local line = "item " .. i .. ": value=" .. (i * 3) .. " tag=" .. name
  local fields = {}
  for field in string.gmatch(line, "[^ ]+") do
    fields[#fields + 1] = field
  end
  local joined = table.concat(fields, ",")
  total = total + #line + #fields + #joined
end
print(total)
