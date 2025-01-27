//  Katherine Cox-Buday, Concurrency in GO
// https://go.dev/talks/2012/concurrency.slide
// https://reliasoftware.com/blog/golang-concurrency-patterns

package main

import (
	"fmt"
	"time"
)

func main() {
	done := make(chan bool)
	producer := func(s string, done chan bool) <-chan string {
		results := make(chan string)
		go func() {
			defer close(results)
			for {
				select {
				case <-done:
					return
				case results <- s:
				}
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

	results := producer("A", done)
	go consumer(results)

	time.Sleep(1 * time.Second)
	done <- true

	var s string
	fmt.Scanf("%s", s)

}
