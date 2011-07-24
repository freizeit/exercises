package main


import (
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	)


var inputFileName = flag.String("f", "", "input data file")


func main() {
	flag.Parse()
	data, err := ioutil.ReadFile(*inputFileName)
	if err != nil {
		fmt.Printf("can't open file; err=%s\n",  err.String())
		os.Exit(1)
	}
	inputs := InputChan(data)
	for input := range inputs {
		fmt.Printf("Input #%d: credit: %d, items: %d\n", input.Index,
					  input.Credit, input.Items)
		result := FindItems(input)
		fmt.Println(result)
	}
}
