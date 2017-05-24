package main

import "fmt"
import "log"
import "bufio"
import "os"
import "strings"
import "strconv"

var OPEN = 0//this is arbitrary, but Go initializes the []int w 0's
var CLOSED = 1

func getNewState(state int) int{
	newState := 0
	if state == 0 {
		newState = 1
	}
	return newState
}

func changeDoor(corridor []int, index int) {
	corridor[index] = getNewState(corridor[index]) 
}

func iterate(corridor []int) {
	for i := 1; i < len(corridor); i += 2 {
		corridor[i] = CLOSED
	}
	for i := 2; i < len(corridor); i += 3 {
		changeDoor(corridor, i)
	}
}

func walkThrough(size int, numIterations int) int {
	corridor := make([]int, size)
	for i := 0; i < numIterations - 1; i++ {
		iterate(corridor)
	}
	if numIterations > 0 {
		changeDoor(corridor, len(corridor) - 1)
	}
	
	var unlocked int
	for _, state := range corridor {
		if state == OPEN {
			unlocked++
		}
	}
	return unlocked
}

func main() {
	file, err := os.Open(os.Args[1])
    if err != nil {
        log.Fatal(err)
    }   
    defer file.Close()
    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        testCase := strings.Split(scanner.Text(), " ")
        size, _ := strconv.Atoi(testCase[0])
        num, _ := strconv.Atoi(testCase[1])
        fmt.Println(walkThrough(size, num))
    }
}