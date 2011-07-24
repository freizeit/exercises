package main


import (
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"runtime"
	)


var inputFileName = flag.String("f", "", "input data file")
var numCores = flag.Int("n", 2, "number of CPU cores to use")


func main() {
	flag.Parse()
	data, err := ioutil.ReadFile(*inputFileName)
	if err != nil {
		fmt.Printf("can't open file; err=%s\n",  err.String())
		os.Exit(1)
	}

	runtime.GOMAXPROCS(*numCores)

	var counter int
	inputs := InputChan(data)
	rch := make(chan string)
	for input := range inputs {
		// fmt.Printf("Input #%d: credit: %d, items: %d\n", input.Index,
		// 			  input.Credit, input.Items)
		go FindItems(input, rch)
		counter += 1
	}
	for i := 0; i < counter; i++ {
		result := <-rch
		fmt.Println(result)
	}
}
