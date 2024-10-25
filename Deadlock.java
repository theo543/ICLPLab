// Exemplu de deadlock
public class Deadlock {
    private Object resource1 = new Object();
    private Object resource2 = new Object();

    public void method1() {
        synchronized (resource1) {
            System.out.println("1 - sync Resource 1");
            try {
                Thread.sleep(100);
            }
            catch (InterruptedException ex) {
                ex.printStackTrace();
            }

            synchronized (resource2) {
                System.out.println("1 - sync resource 2");
            }
        }
    }

    public void method2() {
        synchronized (resource2) {
            System.out.println("2 - sync Resource 2");
            try {
                Thread.sleep(100);
            }
            catch (InterruptedException ex) {
                ex.printStackTrace();
            }

            synchronized (resource1) {
                System.out.println("2 - sync resource 1");
            }
        }
    }

    public static void main(String[] args) {
        Deadlock main = new Deadlock();
        Thread t1 = new Thread(main::method1);
        Thread t2 = new Thread(main::method2);

        t1.start();
        t2.start();
    }
}
