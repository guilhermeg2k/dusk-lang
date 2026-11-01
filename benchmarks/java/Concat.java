public class Concat {
    public static void main(String[] args) {
        String text = "";
        for (int i = 0; i < 200000; i++) {
            text = text + "ab";
        }
        System.out.println(text.length());
    }
}
