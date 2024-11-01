import java.util.concurrent.CompletableFuture;

public class CompletableFutureFib {
	public static void main(String[] args) {
		int n = 42;
		System.out.println("Fibonacci for n = " + n);

		CompletableFuture<Long> future = CompletableFuture.supplyAsync(() -> fibonacci(n));

		CompletableFuture<Void> printFib = future.thenAccept(result -> {
			System.out.println("Fibo(" + n + ") = " + result);
		});

		System.out.println("Waiting for calculation...");

		printFib.join();
	}

	private static long fibonacci(int n) {
		if (n <= 1) {
			return n;
		}
		return fibonacci(n - 1) + fibonacci(n - 2);
	}
}
