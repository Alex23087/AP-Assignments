public class Main {
    public static void main(String[] args) {
        XMLSerializer.serialize(new Object[]{new Student("Alan", "Turing", 52380)}, "");
    }
}