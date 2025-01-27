package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

func f(s string, wg *sync.WaitGroup) {
	defer wg.Done()
	for i := 0; i < 5; i++ {
		fmt.Print(s)
		amt := time.Duration(rand.Intn(250))
		time.Sleep(time.Millisecond * amt)
	}
}

func main() {
	var wg sync.WaitGroup
	wg.Add(2)
	go f("A", &wg)
	go f("B", &wg)
	wg.Wait()
	fmt.Println("gata")
}
