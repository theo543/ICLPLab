//  Katherine Cox-Buday, Concurrency in GO
// https://go.dev/talks/2012/concurrency.slide
// https://reliasoftware.com/blog/golang-concurrency-patterns

package main

import (
	"fmt"
)

func main() {
	producer := func(s string) <-chan string {
		results := make(chan string)
		go func() {
			defer close(results)
			for i := 0; i <= 9; i++ {
				results <- s
			}
		}()
		return results
	}

	consumer := func(results <-chan string) {
		for result := range results {
			fmt.Printf("Received: %s\n", result)
		}
		fmt.Println("Done receiving!")
	}

	results := producer("A")
	consumer(results)
}
