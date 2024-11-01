import java.io.InputStreamReader;
import java.net.URI;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;

import javax.net.ssl.HttpsURLConnection;

public class CompletableFutureCrawler {
    static List<String> urls = List.of(
        "https://google.com",
        "https://jsonplaceholder.typicode.com/posts/1",
        "https://jsonplaceholder.typicode.com/posts/2",
        "https://jsonplaceholder.typicode.com/posts/3"
    );

    public static String crawl(String url) {
        InputStreamReader reader = null;
        HttpsURLConnection connection = null;
        try {
            System.out.println("Started crawling: " + url);

            connection = (HttpsURLConnection) new URI(url).toURL().openConnection();
            connection.setRequestMethod("GET");
            connection.setConnectTimeout(5000);
            connection.setReadTimeout(5000);

            int status = connection.getResponseCode();
            if (status != 200) {
                return "Unexpected status code: " + status;
            }

            StringBuilder html = new StringBuilder();
            char[] buffer = new char[1000000];
            int result;
            reader = new InputStreamReader(connection.getInputStream());
            while ((result = reader.read(buffer)) != -1) {
                html.append(buffer, 0, result);
            }

            return html.toString();
        } catch (Exception ex) {
            return "Error crawling " + url;
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (Exception ex) {
                    System.out.println("Unable to close InputStreamReader");
                    ex.printStackTrace();
                }
            }
            if (connection != null) {
                connection.disconnect();
            }
        }
    }

    public static void main(String args[]) {
        for (String url : urls) {
            CompletableFuture<String> crawlFuture = CompletableFuture.supplyAsync(() -> crawl(url));
            crawlFuture.thenAccept(result -> {
                System.out.println("" + url + " result:\n" + result);
            });
        }

        ForkJoinPool.commonPool().awaitQuiescence(Long.MAX_VALUE, TimeUnit.DAYS);
    }
}
