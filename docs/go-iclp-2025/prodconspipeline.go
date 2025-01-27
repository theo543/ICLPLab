//  Katherine Cox-Buday, Concurrency in GO
// https://go.dev/talks/2012/concurrency.slide
// https://reliasoftware.com/blog/golang-concurrency-patterns

package main

import (
	"fmt"
	"strings"
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

	processdata1 := func(data <-chan string) <-chan string {
		results := make(chan string)
		go func() {
			defer close(results)
			for item := range data {
				results <- strings.ToLower(item)
			}
		}()
		return results
	}

	processdata2 := func(data <-chan string) <-chan string {
		results := make(chan string)
		go func() {
			defer close(results)
			for item := range data {
				results <- item + item
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

	data := producer("A")
	results1 := processdata1(data)
	results := processdata2(results1)
	consumer(results)
}
