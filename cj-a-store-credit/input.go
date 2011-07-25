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


// Convert the raw inputs contained in the 'data' buffer to 'Input' structs
// and start a go routine for each of the latter. Return the number of
// inputs processed as well as the channel from which to read the results.
func ProcessInput(data []byte) (int, chan string) {
	count := 0
	rchan := make(chan string)

	lines := strings.Split(strings.TrimSpace(string(data)), "\n", -1)
	//fmt.Printf("number of lines: %v\n", len(lines))
	//fmt.Printf("lines: %q\n", lines)

	var credit uint
	var err os.Error

	// skip leading line with the total number of inputs
	for i, line := range lines[1:] {
		switch i % 3 {
		case 0: // credit
			credit, err = strconv.Atoui(line)
			if err != nil {
				fmt.Printf("invalid credit '%s'; err=%s\n",
					line, err.String())
				os.Exit(2)
			}
		case 2: // the store items
			fields := strings.Fields(line)
			items := make([]uint, len(fields))
			for fi, field := range fields {
				items[fi], err = strconv.Atoui(field)
				if err != nil {
					fmt.Printf("invalid store item '%s'; err=%s\n",
						field, err.String())
					os.Exit(3)
				}
			}
			input := Input{uint(i/3 + 1), credit, items}
			go FindItems(input, rchan)
			count += 1
		}
	}
	return count, rchan
}
