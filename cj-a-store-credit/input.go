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
func ProcessInput(path string) (count int, rchan chan string) {
	file, err := os.Open(path)
	if err != nil {
		panic(fmt.Sprintf("can't open file; err=%s\n", err.String()))
	}
	defer file.Close()
	reader := bufio.NewReader(file)

	// Read the first line with the total number of inputs.
	if _, done := readLine(reader); done == true {
		return
	}

	rchan = make(chan string, 25000)

	for {
		// Read the next 3 non-empty lines from the input file
		lines, err := next3lines(reader)
		if err != nil {
			fmt.Printf("invalid input file; err=%s\n", err.String())
			break
		}
		if lines == nil {
			// EOF
			break;
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

		count += 1
		input := Input{uint(count), credit, items}
		go FindItems(input, rchan)
	}
	return
}


// Parse the line and return a slice with the store items (prices) found.
func parseItems(line string) (items []uint, err os.Error) {
	// Split the line into words
	fields := strings.Fields(line)
	var value uint

	for i, field := range fields {
		value, err = strconv.Atoui(field)
		if err != nil {
			break
		}
		// We have a valid store item price at this point.
		if items == nil {
			items = make([]uint, len(fields))
		}
		items[i] = value
	}
	return
}


// Try reading the next 3 non-empty lines from the 'reader'. In case of succes
// (we got 3 lines) the error will be 'nil'.
func next3lines(reader *bufio.Reader) (lines []string, err os.Error) {
	i := 0
	done := false
	line := ""

	for done != true && i < 3 {
		line, done = readLine(reader)
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		// We have a valid line of input at this point.
		if lines == nil {
			lines = make([]string, 3)
		}
		lines[i] = line
		i += 1
	}
	if lines != nil && i < 3 {
		err = os.NewError(fmt.Sprintf("%d lines of input", i))
	}
	return
}


func readLine(reader *bufio.Reader) (string, bool) {
	done := false
	line, err := reader.ReadString('\n')
	if err != nil {
		if err != os.EOF {
			panic(fmt.Sprintf("can't read line; err=%s\n", err.String()))
		} else {
			done = true
		}
	}
	return line, done
}
