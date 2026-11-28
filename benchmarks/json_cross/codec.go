package main

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"
)

const recordText = `{"id":42,"name":"Ada\nLovelace","is_active":true,"email":null,"tags":["compiler","runtime"],"counts":{"views":7,"likes":3},"profile":{"city":"Porto","score":1.5}}`

type Profile struct {
	City  string  `json:"city"`
	Score float64 `json:"score"`
}

type Record struct {
	ID       int64            `json:"id"`
	Name     string           `json:"name"`
	IsActive bool             `json:"is_active"`
	Email    *string          `json:"email"`
	Tags     []string         `json:"tags"`
	Counts   map[string]int64 `json:"counts"`
	Profile  Profile          `json:"profile"`
}

type DefaultRecord struct {
	ID    int64    `json:"id"`
	Name  string   `json:"name"`
	Email *string  `json:"email"`
	Tags  []string `json:"tags"`
}

func check(isValid bool) {
	if !isValid {
		panic("unexpected JSON result")
	}
}

func decode(text []byte, target any) {
	if err := json.Unmarshal(text, target); err != nil {
		panic(err)
	}
}

func main() {
	text := []byte(recordText)
	bulk := []byte("[" + strings.TrimSuffix(strings.Repeat(recordText+",", 256), ",") + "]")
	var total int64
	switch os.Args[1] {
	case "json_decode":
		for i := 0; i < 100000; i++ {
			var record Record
			decode(text, &record)
			check(record.Name == "Ada\nLovelace")
			check(record.Email == nil)
			check(record.IsActive)
			check(record.Profile.Score == 1.5)
			total += record.ID + int64(len(record.Tags)) + record.Counts["views"] + 1
		}
	case "json_decode_bulk":
		for i := 0; i < 1000; i++ {
			var records []Record
			decode(bulk, &records)
			check(len(records) == 256)
			for _, record := range records {
				total += record.ID + int64(len(record.Name)) + int64(len(record.Tags)) + record.Counts["likes"]
			}
		}
	case "json_encode":
		var records []Record
		decode(bulk, &records)
		expected, err := json.Marshal(records)
		if err != nil {
			panic(err)
		}
		check(string(expected) == strings.ReplaceAll(string(bulk), `"views":7,"likes":3`, `"likes":3,"views":7`))
		for i := 0; i < 1000; i++ {
			encoded, err := json.Marshal(records)
			if err != nil {
				panic(err)
			}
			check(string(encoded) == string(expected))
			total++
		}
	case "json_defaults":
		input := []byte(`{"id":42}`)
		for i := 0; i < 100000; i++ {
			record := DefaultRecord{Name: "Ada", Tags: []string{"default", "fresh"}}
			decode(input, &record)
			check(record.Name == "Ada")
			check(record.Email == nil)
			record.Tags = append(record.Tags, "added")
			total += record.ID + int64(len(record.Tags)) + 2
		}
	default:
		panic("unknown workload")
	}
	fmt.Println(total)
}
