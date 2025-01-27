// https://pkg.go.dev/net
// https://medium.com/@viktordev/socket-programming-in-go-write-a-simple-tcp-client-server-c9609edf3671
// https://reintech.io/blog/introduction-to-gos-net-package-networking-and-sockets

// serverul se opreste cu Ctrl+C

package main

import (
	"fmt"
	"net"
)

func main() {

	listener, err := net.Listen("tcp", "localhost:8080")
	if err != nil {
		fmt.Println("Error:", err)
		return
	}
	defer listener.Close()

	fmt.Println("Server is listening")

	for {
		conn, err := listener.Accept()
		if err != nil {
			fmt.Println("Error:", err)
			continue
		}

		go handleClient(conn)
	}
}

func handleClient(conn net.Conn) {
	defer conn.Close()
	conn.Write([]byte("Hello Client! Send me your data!"))

	buffer := make([]byte, 1024)

	for {
		n, err := conn.Read(buffer)
		if err != nil {
			fmt.Println("Error:", err)
			return
		}

		fmt.Printf("Received: %s\n", buffer[:n])
	}
}
