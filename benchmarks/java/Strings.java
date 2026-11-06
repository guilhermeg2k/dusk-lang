public class Strings {
    public static void main(String[] args) {
        String name = "dusk";
        long total = 0;
        for (int i = 0; i < 1000000; i++) {
            String line = "item " + i + ": value=" + (i * 3) + " tag=" + name;
            String[] fields = line.split(" ");
            String joined = String.join(",", fields);
            total += line.length() + fields.length + joined.length();
        }
        System.out.println(total);
    }
}
