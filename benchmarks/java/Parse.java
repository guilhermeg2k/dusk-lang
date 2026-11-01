import java.util.ArrayList;

public class Parse {
    public static void main(String[] args) {
        ArrayList<String> rows = new ArrayList<>();
        for (int i = 0; i < 500000; i++) {
            rows.add(i + "," + ((i * 7) % 1000) + ",item" + i);
        }
        String csv = String.join("\n", rows);
        long total = 0;
        long count = 0;
        for (String row : csv.split("\n")) {
            String[] fields = row.split(",");
            total += Long.parseLong(fields[0]) + Long.parseLong(fields[1]);
            count += 1;
        }
        System.out.println(total + count);
    }
}
