// Exemplu de implementare pentru Publisher - Subscriber
import java.util.concurrent.CopyOnWriteArrayList;

public class Main {
    public static void main(String[] args) {
        MessagePublisher publisher = new MessagePublisher();

        Subscriber subscriber1 = new MessageSubscriber("Alice");
        Subscriber subscriber2 = new MessageSubscriber("Bob");
        Subscriber subscriber3 = new MessageSubscriber("Eve");

        publisher.subscribe(subscriber1);
        publisher.subscribe(subscriber2);
        publisher.subscribe(subscriber3);

        new Thread(() -> publisher.publish("Hello, subscribers! Please like and share!")).start();
        new Thread(() -> publisher.publish("Second message")).start();
    }
}

interface Subscriber {
    void update(String message);
}

interface Publisher {
    void subscribe(Subscriber subscriber);
    void unsubscribe(Subscriber subscriber);
    void notifySubscribers(String message);
}

class MessagePublisher implements Publisher {
    private CopyOnWriteArrayList<Subscriber> subscribers = new CopyOnWriteArrayList<>();

    @Override
    public void subscribe(Subscriber subscriber) {
        subscribers.add(subscriber);
    }

    @Override
    public void unsubscribe(Subscriber subscriber) {
        subscribers.remove(subscriber);
    }

    @Override
    public void notifySubscribers(String message) {
        for (Subscriber subscriber : subscribers) {
            new Thread(() -> subscriber.update(message)).start();
        }
    }

    public void publish(String message) {
        System.out.println("Publishing message: " + message);
        notifySubscribers(message);
        try {
            Thread.sleep(4000);
        }
        catch (InterruptedException ex) {
            ex.printStackTrace();
        }
    }
}

class MessageSubscriber implements Subscriber {
    private String name;

    public MessageSubscriber(String name) {
        this.name = name;
    }

    @Override
    public void update(String message) {
        System.out.println("Subscriber " + name + " received: " + message);
    }
}
