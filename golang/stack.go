package main

import (
	"fmt"
	"os"
	"log"
	"bufio"
	"strings"
)

type Stack struct {
	data []string
}

func (s *Stack) Push(n string) {
	s.data = append(s.data, n)
}

func (s *Stack) Pop() string {
	value := s.data[len(s.data) - 1]
	s.data = s.data[:len(s.data) - 1]
	return value
}

func main() {
	file, err := os.Open(os.Args[1])
    if err != nil {
        log.Fatal(err)
    }   
    defer file.Close()
    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        testCase := strings.Fields(scanner.Text())

        stack := Stack{}
        for _, item := range testCase {
        	stack.Push(item)
        }
        var value string
        for i, _ := range stack.data {
        	value = stack.Pop()
        	if i % 2 == 0 {
        		fmt.Printf("%v ", value)
        	}
        }
        fmt.Println()
    }
}