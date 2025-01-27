// parcurge tot canalul fanin
// https://go.dev/talks/2012/concurrency.slide  modificat conform
//https://reliasoftware.com/blog/golang-concurrency-patterns

package main

import (
	"fmt"
)

func main() {
	producer := func(s string, n int) <-chan string {
		results := make(chan string)
		go func() {
			defer close(results)
			for i := 0; i < n; i++ {
				results <- s
			}
		}()
		return results
	}

	consumer := func(results <-chan string) {
		for result := range results {
			fmt.Print(result)
		}
		fmt.Println("\nDone receiving!")
	}

	fanIn := func(c1, c2 <-chan string) <-chan string {
		c := make(chan string)
		go func() {
			defer close(c)
			for (c1 != nil) || (c2 != nil) {
				select {
				case s, ok1 := <-c1:
					if ok1 {
						c <- s
					} else {
						c1 = nil
					}
				case s, ok2 := <-c2:
					if ok2 {
						c <- s
					} else {
						c2 = nil
					}
				}
			}

		}()
		return c
	}

	var nA, nB int
	fmt.Print("nA=")
	fmt.Scan(&nA)
	fmt.Print("nB=")
	fmt.Scan(&nB)

	c1 := producer("A", nA)
	c2 := producer("B", nB)
	results := fanIn(c1, c2)
	consumer(results)

}
