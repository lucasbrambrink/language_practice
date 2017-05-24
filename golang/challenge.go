
package main

import "fmt"
import "log"
import "bufio"
import "os"


func HasArrow(testCase string) int {
    var rightArrow string = "<--<<"
    var leftArrow string = ">>-->"
    var arrowCount int
    var slice string
    for i := 0; i + 4 < len(testCase); i++ {
        slice = testCase[i:i+5]
        if slice == rightArrow || slice == leftArrow {
            arrowCount++
        }
    }
    return arrowCount
}

func main() {
    file, err := os.Open(os.Args[1])
    if err != nil {
        log.Fatal(err)
    }   
    defer file.Close()
    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        //'scanner.Text()' represents the test case, do something with it
        testCase := scanner.Text()
        fmt.Println(HasArrow(testCase))
    }   
}