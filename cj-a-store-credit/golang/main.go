/*
	A program solving

		http://code.google.com/codejam/contest/dashboard?c=351101#s=p0

	The test cases are contained in a file whose path is to be specified on
	the command line. The caller may also specify the number of CPU cores to
	use (default: 2).

	Example:

		./main -f A-large-practice.in -n 3

	This will process the test cases contained in the 'A-large-practice.in'
	file and use three CPU cores.

	Please note: the results will only be ordered correctly for n=1. For
	n > 1 the results are output in random order.
*/


package main


import (
	"flag"
	"fmt"
	"os"
	"runtime"
)


var inputFileName = flag.String("f", "", "input data file")
var numCores = flag.Int("n", 4, "number of CPU cores to use")
var rchanSize = flag.Uint("s", 10000, "result channel buffer size")


func main() {
	flag.Parse()

	if *inputFileName == "" {
		fmt.Println("Please specify an input file (using '-f')")
		os.Exit(1)
	}

	runtime.GOMAXPROCS(*numCores)

	counter, rchan := ProcessInput(*inputFileName, *rchanSize)
	var i uint

	for i=0; i < counter; i++ {
		result := <-rchan
		fmt.Println(result)
	}
}
