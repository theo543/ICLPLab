package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

var salutation string
var nume string = "Ioana"
var wg sync.WaitGroup

func g() func(s string) {
	var salutation string
	//defer wg.Done()
	salutation = "welcome"
	amt := time.Duration(rand.Intn(250))
	time.Sleep(time.Millisecond * amt)
	return func(s string) {
		defer wg.Done()
		fmt.Println(salutation, s, nume)
	}
}

func main() {
	salutation = "hello"
	wg.Add(1)
	go g()(" you")
	wg.Wait()
	fmt.Println(salutation, nume)

}
