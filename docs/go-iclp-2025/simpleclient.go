package main

import (
	"fmt"
	"net"
)

func main() {
	// Connect to the server
	conn, err := net.Dial("tcp", "localhost:8080")
	if err != nil {
		fmt.Println("Error:", err)
		return
	}
	defer conn.Close()

	// Read incoming data
	buf := make([]byte, 1024)
	_, err = conn.Read(buf)
	if err != nil {
		fmt.Println(err)
		return
	}
	fmt.Printf("Received: %s\n", buf)

	// Send data to the server
	var mes string
	fmt.Scanf("%s\n", &mes)
	data := []byte(mes)
	_, err = conn.Write(data)
	if err != nil {
		fmt.Println("Error:", err)
		return
	}

}
