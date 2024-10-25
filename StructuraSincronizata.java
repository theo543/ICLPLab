// Implementam un scenariu simplu de Producer-Consumer 
// vom avea doar un Producer si doar un Consumer 
// ele (threadurile) sau ei (Producerul si Consumerul)
// comunica printr-o structura de date sincronizata pe care o implementam noi 
// PCDrop 
// Producerul pune mesaje (predefinite) in acest Drop 
// Consumerul le consuma. 
public class StructuraSincronizata
{
	public static void main(String[] args) {
		System.out.println("Hello World");
	}
}

class PCDrop 
{
    private String message; 
    private boolean isEmpty = true; 
    
    public synchronized String take() {
        while (isEmpty) {
            try {
                wait();
            }
            catch (InterruptedException ex) {
                ex.printStackTrace();
            }
        }
        isEmpty = true;
        notifyAll();
        return message; 
    }
    
    public synchronized void put(String message) {
        while (!isEmpty) {
            try {
                wait();
            }
            catch (InterruptedException ex) {
                ex.printStackTrace();
            }
        }
        isEmpty = false; 
        this.message = message; 
        notifyAll();
    }
}