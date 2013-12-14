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
	"errors"
	"fmt"
	"io"
	"os"
	"strings"
)


// Convert the data contained in the file with the given 'path' to
// 'Input' structs and start a go routine for each of the latter. Return
// the number of inputs processed as well as the channel from which to
// read the results.
func ProcessInput(path string, rchanSize uint) (count uint, rchan chan string) {
	file, err := os.Open(path)
	if err != nil {
		panic(fmt.Sprintf("can't open file; err=%s\n", err.Error()))
	}
	defer file.Close()
	reader := bufio.NewReader(file)

	// Read the first line with the total number of inputs.
	if _, done := readLine(reader); done == true {
		return
	}

	rchan = make(chan string, rchanSize)

	for {
		// Read the next 3 non-empty lines from the input file
		lines, err := next3lines(reader)
		if err != nil {
			fmt.Printf("invalid input file; err=%s\n", err.Error())
			break
		}
		if lines == nil {
			// EOF
			break;
		}

		count += 1
		go FindItems(count, lines, rchan)
	}
	return
}


// Try reading the next 3 non-empty lines from the 'reader'. In case of succes
// (we got 3 lines) the error will be 'nil'. If we reach the end of file (i.e.
// not a single non-empty line could be read) 'lines' will be nil.
func next3lines(reader *bufio.Reader) (lines []string, err error) {
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
		err = errors.New(fmt.Sprintf("%d lines of input", i))
	}
	return
}


// Read a single line using the given 'reader'. The 'done' flag will be true
// if we read the last line.
func readLine(reader *bufio.Reader) (string, bool) {
	done := false
	line, err := reader.ReadString('\n')
	if err != nil {
		if err != io.EOF {
			panic(fmt.Sprintf("can't read line; err=%s\n", err.Error()))
		} else {
			done = true
		}
	}
	return line, done
}
