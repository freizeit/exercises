/*
	Processes the input data sets for

		http://code.google.com/codejam/contest/dashboard?c=351101#s=p0

	Problem

	You receive a credit C at a local store and would like to buy two items.
	You first walk through the store and create a list L of all available
	items. From this list you would like to buy two items that add up to the
	entire value of the credit. The solution you provide will consist of the
	two integers indicating the positions of the items in your list (smaller
	number first).

	Input

	The first line of input gives the number of cases, N. N test cases
	follow. For each test case there will be:

		 - one line containing the value C, the amount of credit you have at
			the store.
		 - one line containing the value I, the number of items in the store.
		 - one line containing a space separated list of I integers. Each
			integer P indicates the price of an item in the store.
		 - each test case will have exactly one solution.
*/


package main


import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)


type Input struct {
	Index  uint   // input number, 1-based
	Credit uint   // store credit
	Items  []uint // store items (prices)
}


// Convert the data contained in the file with the given 'path' to
// 'Input' structs and start a go routine for each of the latter. Return
// the number of inputs processed as well as the channel from which to
// read the results.
func ProcessInput(path string) (int, chan string) {
	file, err := os.Open(path)
	if err != nil {
		fmt.Printf("can't open file; err=%s\n", err.String())
		os.Exit(1)
	}
	defer file.Close()
	reader := bufio.NewReader(file)

	var credit uint

	numRoutines := 0
	numLines := 0
	rchan := make(chan string, 25000)

	// Read the first line with the total number of inputs.
	line, done := readLine(reader)
	numLines += 1

	for done != true {
		line, done = readLine(reader)
		line = strings.TrimSpace(line)

		if line == "" {
			continue
		}

		switch (numLines - 1) % 3 {
		case 0: // credit
			value, err := strconv.Atoui(line)
			if err != nil {
				fmt.Printf("invalid credit '%s' on line %d; err=%s\n",
					line, numLines+1, err.String())
				os.Exit(2)
			}
			credit = value
		case 2: // the store items
			fields := strings.Fields(line)
			items := make([]uint, len(fields))
			for fi, field := range fields {
				value, err := strconv.Atoui(field)
				if err != nil {
					fmt.Printf("invalid store item '%s' on line %d; err=%s\n",
						field, numLines+1, err.String())
					os.Exit(3)
				}
				items[fi] = value
			}
			input := Input{uint(numLines / 3), credit, items}
			go FindItems(input, rchan)
			numRoutines += 1
		}
		numLines += 1
	}
	return numRoutines, rchan
}


func readLine(reader *bufio.Reader) (string, bool) {
	var done bool
	line, err := reader.ReadString('\n')
	if err != nil {
		if err != os.EOF {
			fmt.Printf("can't read line; err=%s\n", err.String())
			os.Exit(4)
		} else {
			done = true
		}
	}
	return line, done
}
