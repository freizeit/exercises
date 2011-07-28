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

	numRoutines := 0
	rchan := make(chan string, 25000)

	// Read the first line with the total number of inputs.
	_, done := readLine(reader)

	for done != true {
		// Read the next 3 non-empty lines from the input file
		lines, err := nextThreeLines(reader)
		if err != nil {
			fmt.Printf("invalid input file; err=%s\n", err.String())
			break
		}
		if lines == nil {
			// EOF
			done = true
			continue
		}

		// Parse the credit (from line 1)
		credit, err := strconv.Atoui(lines[0])
		if err != nil {
			fmt.Printf("invalid credit '%s'; err=%s\n", lines[0], err.String())
			break
		}

		// Parse the store item prices (from line 3)
		items, err := parseItems(lines[2])
		if err != nil {
			fmt.Printf("invalid item '%s'; err=%s\n", lines[2], err.String())
			break
		}

		numRoutines += 1
		input := Input{uint(numRoutines), credit, items}
		go FindItems(input, rchan)
	}
	return numRoutines, rchan
}


// Parse the line and return a slice with the store items (prices) found.
func parseItems(line string) ([]uint, os.Error) {
	// Split the line into words
	fields := strings.Fields(line)
	items := make([]uint, len(fields))
	var err os.Error

	for i, field := range fields {
		items[i], err = strconv.Atoui(field)
		if err != nil {
			break
		}
	}
	return items, err
}


// Try reading the next 3 non-empty lines from the 'reader'. In case of succes
// (we got 3 lines) the error will be 'nil'.
func nextThreeLines(reader *bufio.Reader) ([]string, os.Error) {
	i := 0
	done := false
	line := ""
	var lines []string
	var err os.Error

	for done != true && i < 3 {
		line, done = readLine(reader)
		line = strings.TrimSpace(line)
		if line != "" {
			if lines == nil {
				lines = make([]string, 3)
			}
			lines[i] = line
			i += 1
		}
	}
	if lines != nil && i < 3 {
		err = os.NewError(fmt.Sprintf("%d lines of input", i))
	}
	return lines, err
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
