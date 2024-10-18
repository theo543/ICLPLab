import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.ArrayList;

public class DirTreeProducerConsumer {
    public static void main(String[] args) throws InterruptedException {
        if (args.length != 1) {
            System.err.println("Expected one argument.");
            return;
        }
        Drop drop = new Drop();
        int consumerNr = 2;
        for (int x = 0; x < consumerNr; x++) {
            (new Thread(new Consumer(drop))).start();
        }
        ArrayList<File> folders = new ArrayList<>();
        folders.add(new File(args[0]));
        ArrayList<Thread> producers = new ArrayList<>();
        for (int x = 0; x < folders.size(); x++) {
            File folder = folders.get(x);
            File[] files = folder.listFiles();
            if (files == null) {
                System.err.println("Could not list files in " + folder.getAbsolutePath());
                continue;
            }
            for (File f : files) {
                if (f.isDirectory()) {
                    folders.add(f);
                } else if (f.isFile()) {
                    Thread t = new Thread(new Producer(drop, f));
                    producers.add(t);
                    t.start();
                } else {
                    System.err.println("What is this??? " + f.getPath());
                }
            }
        }
        for (Thread p : producers) {
            p.join();
        }
        System.out.println("All producers done, sending shutdown messages");
        for (int x = 0; x < consumerNr; x++) {
            drop.put(null);
        }
    }
}

class Producer implements Runnable {
    private Drop drop;
    private File f;

    public Producer(Drop drop, File f) {
        this.drop = drop;
        this.f = f;
    }

    @Override
    public void run() {
        Scanner s = null;
        try {
            s = new Scanner(f);
            while (s.hasNextLine()) {
                String message = s.nextLine();
                System.out.println(Thread.currentThread().threadId() + " produced " + message);
                drop.put(message);
            }
        } catch (InterruptedException ex) {
            ex.printStackTrace();
        } catch (FileNotFoundException ex) {
            ex.printStackTrace();
        } finally {
            if (s != null) {
                s.close();
            }
        }
    }
}

class Consumer implements Runnable {
    private Drop drop;

    public Consumer(Drop drop) {
        this.drop = drop;
    }

    @Override
    public void run() {
        try {
            while (true) {
                String message = drop.take();
                if (message == null) {
                    System.out.println("Producer " + Thread.currentThread().threadId() + " shutting down");
                    return;
                }
                System.out.println(Thread.currentThread().threadId() + " received " + message);
            }
        } catch (InterruptedException ex) {
            ex.printStackTrace();
        }
    }
}

class Drop {
    private String message;
    private boolean isEmpty = true;

    public synchronized String take() throws InterruptedException {
        while (isEmpty) {
            wait();
        }
        isEmpty = true;
        notifyAll();
        return message;
    }

    public synchronized void put(String message) throws InterruptedException {
        while (!isEmpty) {
            wait();
        }
        isEmpty = false;
        this.message = message;
        notifyAll();
    }
}
