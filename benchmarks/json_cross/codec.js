const text = "{\"id\":42,\"name\":\"Ada\\nLovelace\",\"is_active\":true,\"email\":null,\"tags\":[\"compiler\",\"runtime\"],\"counts\":{\"views\":7,\"likes\":3},\"profile\":{\"city\":\"Porto\",\"score\":1.5}}";
const bulk = "[" + Array(256).fill(text).join(",") + "]";
const workload = process.argv[2];
function check(isValid) {
    if (!isValid) {
        throw new Error("unexpected JSON result");
    }
}
let total = 0;
if (workload === "json_decode") {
    for (let i = 0; i < 100000; i++) {
        const record = JSON.parse(text);
        check(record.name === "Ada\nLovelace");
        check(record.email === null);
        check(record.is_active);
        check(record.profile.score === 1.5);
        total += record.id + record.tags.length + (record.counts.views ?? 0) + 1;
    }
} else if (workload === "json_decode_bulk") {
    for (let i = 0; i < 1000; i++) {
        const records = JSON.parse(bulk);
        check(records.length === 256);
        for (const record of records) {
            total += record.id + record.name.length + record.tags.length + (record.counts.likes ?? 0);
        }
    }
} else if (workload === "json_encode") {
    const records = JSON.parse(bulk);
    const expected = JSON.stringify(records);
    check(expected === bulk);
    for (let i = 0; i < 1000; i++) {
        check(JSON.stringify(records) === expected);
        total += 1;
    }
} else if (workload === "json_defaults") {
    for (let i = 0; i < 100000; i++) {
        const record = JSON.parse('{"id":42}');
        record.name ??= "Ada";
        record.email ??= null;
        record.tags ??= ["default", "fresh"];
        check(record.name === "Ada");
        check(record.email === null);
        record.tags.push("added");
        total += record.id + record.tags.length + 2;
    }
} else {
    throw new Error(workload);
}
console.log(total);
