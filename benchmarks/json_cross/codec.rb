require 'json'

TEXT = '{"id":42,"name":"Ada\nLovelace","is_active":true,"email":null,"tags":["compiler","runtime"],"counts":{"views":7,"likes":3},"profile":{"city":"Porto","score":1.5}}'
BULK = '[' + ([TEXT] * 256).join(',') + ']'

def check(is_valid)
  raise 'unexpected JSON result' unless is_valid
end

total = 0
case ARGV.fetch(0)
when 'json_decode'
  100_000.times do
    record = JSON.parse(TEXT)
    check(record['name'] == "Ada\nLovelace")
    check(record['email'].nil?)
    check(record['is_active'])
    check(record['profile']['score'] == 1.5)
    total += record['id'] + record['tags'].length + record['counts'].fetch('views', 0) + 1
  end
when 'json_decode_bulk'
  1_000.times do
    records = JSON.parse(BULK)
    check(records.length == 256)
    records.each do |record|
      total += record['id'] + record['name'].length + record['tags'].length + record['counts'].fetch('likes', 0)
    end
  end
when 'json_encode'
  records = JSON.parse(BULK)
  expected = JSON.generate(records)
  check(expected == BULK)
  1_000.times do
    check(JSON.generate(records) == expected)
    total += 1
  end
when 'json_defaults'
  100_000.times do
    record = JSON.parse('{"id":42}')
    record['name'] = 'Ada' unless record.key?('name')
    record['email'] = nil unless record.key?('email')
    record['tags'] = ['default', 'fresh'] unless record.key?('tags')
    check(record['name'] == 'Ada')
    check(record['email'].nil?)
    record['tags'] << 'added'
    total += record['id'] + record['tags'].length + 2
  end
else
  raise 'unknown workload'
end
puts total
