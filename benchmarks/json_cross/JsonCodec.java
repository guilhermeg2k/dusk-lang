import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Map;

public final class JsonCodec {
    static final String TEXT = "{\"id\":42,\"name\":\"Ada\\nLovelace\",\"is_active\":true,\"email\":null,\"tags\":[\"compiler\",\"runtime\"],\"counts\":{\"views\":7,\"likes\":3},\"profile\":{\"city\":\"Porto\",\"score\":1.5}}";
    static final String BULK = "[" + String.join(",", java.util.Collections.nCopies(256, TEXT)) + "]";
    static final ObjectMapper MAPPER = new ObjectMapper();

    @JsonPropertyOrder({"city", "score"})
    public static final class Profile {
        public String city;
        public double score;
    }

    @JsonPropertyOrder({"id", "name", "is_active", "email", "tags", "counts", "profile"})
    public static final class Record {
        public long id;
        public String name;
        public boolean is_active;
        public String email;
        public ArrayList<String> tags;
        public Map<String, Long> counts;
        public Profile profile;
    }

    public static final class DefaultRecord {
        public long id;
        public String name = "Ada";
        public String email = null;
        public ArrayList<String> tags = new ArrayList<>(Arrays.asList("default", "fresh"));
    }

    static void check(boolean isValid) {
        if (!isValid) {
            throw new AssertionError("unexpected JSON result");
        }
    }

    public static void main(String[] args) throws Exception {
        long total = 0;
        switch (args[0]) {
            case "json_decode":
                for (int i = 0; i < 100000; i++) {
                    Record record = MAPPER.readValue(TEXT, Record.class);
                    check(record.name.equals("Ada\nLovelace"));
                    check(record.email == null);
                    check(record.is_active);
                    check(record.profile.score == 1.5);
                    total += record.id + record.tags.size() + record.counts.getOrDefault("views", 0L) + 1;
                }
                break;
            case "json_decode_bulk":
                for (int i = 0; i < 1000; i++) {
                    Record[] records = MAPPER.readValue(BULK, Record[].class);
                    check(records.length == 256);
                    for (Record record : records) {
                        total += record.id + record.name.length() + record.tags.size() + record.counts.getOrDefault("likes", 0L);
                    }
                }
                break;
            case "json_encode":
                Record[] records = MAPPER.readValue(BULK, Record[].class);
                String expected = MAPPER.writeValueAsString(records);
                check(expected.equals(BULK));
                for (int i = 0; i < 1000; i++) {
                    check(MAPPER.writeValueAsString(records).equals(expected));
                    total++;
                }
                break;
            case "json_defaults":
                for (int i = 0; i < 100000; i++) {
                    DefaultRecord record = MAPPER.readValue("{\"id\":42}", DefaultRecord.class);
                    check(record.name.equals("Ada"));
                    check(record.email == null);
                    record.tags.add("added");
                    total += record.id + record.tags.size() + 2;
                }
                break;
            default:
                throw new IllegalArgumentException(args[0]);
        }
        System.out.println(total);
    }
}
