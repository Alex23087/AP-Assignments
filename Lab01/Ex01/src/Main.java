public class Main {
    public static void main(String[] args) throws InterruptedException {
        long c = 0;
        while(true){
            c++;
            if(c % 1000 == 0){
                System.out.println(c);
                Thread.sleep(1000);
            }
            Object o = new Object();
        }
    }
}