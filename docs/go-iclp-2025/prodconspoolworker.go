//  Katherine Cox-Buday, Concurrency in GO
// https://go.dev/talks/2012/concurrency.slide
// https://reliasoftware.com/blog/golang-concurrency-patterns

package main

import (
	"fmt"
	"sync"
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

	consumer := func(i int, results <-chan string, wg *sync.WaitGroup) {
		defer wg.Done()
		for result := range results {
			fmt.Printf("%d received: %s\n", i, result)
		}
		fmt.Println("Done receiving!")
	}

	results := producer("A")

	//consumers are workers
	var wg sync.WaitGroup
	var nrcons int = 3

	for i := 0; i < nrcons; i++ {
		wg.Add(1)
		go consumer(i+1, results, &wg)
	}
	wg.Wait()
}
