import json
import sys

TEXT = '{"id":42,"name":"Ada\\nLovelace","is_active":true,"email":null,"tags":["compiler","runtime"],"counts":{"views":7,"likes":3},"profile":{"city":"Porto","score":1.5}}'
BULK = "[" + ",".join([TEXT] * 256) + "]"


def main():
    workload = sys.argv[1]
    total = 0
    if workload == "json_decode":
        for _ in range(100_000):
            record = json.loads(TEXT)
            assert record["name"] == "Ada\nLovelace"
            assert record["email"] is None
            assert record["is_active"]
            assert record["profile"]["score"] == 1.5
            total += record["id"] + len(record["tags"]) + record["counts"].get("views", 0) + 1
    elif workload == "json_decode_bulk":
        for _ in range(1_000):
            records = json.loads(BULK)
            assert len(records) == 256
            for record in records:
                total += record["id"] + len(record["name"]) + len(record["tags"]) + record["counts"].get("likes", 0)
    elif workload == "json_encode":
        records = json.loads(BULK)
        expected = json.dumps(records, separators=(",", ":"))
        assert expected == BULK
        for _ in range(1_000):
            encoded = json.dumps(records, separators=(",", ":"))
            assert encoded == expected
            total += 1
    elif workload == "json_defaults":
        for _ in range(100_000):
            record = json.loads('{"id":42}')
            record.setdefault("name", "Ada")
            record.setdefault("email", None)
            record.setdefault("tags", ["default", "fresh"])
            assert record["name"] == "Ada"
            assert record["email"] is None
            record["tags"].append("added")
            total += record["id"] + len(record["tags"]) + 2
    else:
        raise ValueError(workload)
    print(total)


if __name__ == "__main__":
    main()
