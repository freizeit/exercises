/*
	Solves http://code.google.com/codejam/contest/dashboard?c=351101#s=p0

	Problem

	You receive a credit C at a local store and would like to buy two items.
	A list L of all available items is provided.  From this list you would
	like to buy two items that add up to the entire value of the credit.

	Output

	For each test case, output one line containing "Case #x: " followed by
	the indices of the two items whose price adds up to the store credit. The
	lower index should be output first.
*/


package main


import (
	"fmt"
	"strconv"
	"strings"
)


type Input struct {
	Index  uint   // input number, 1-based
	Credit int   // store credit
	Items  []int // store items (prices)
}


// This function deals with a single case. It finds the two store items
// whose prices add up to the credit granted and writes their indices
// (1-based) to the result channel.
func FindItems(count uint, lines []string, rchan chan string) {
	input, err := parse(count, lines)
	if err != nil {
		fmt.Printf("invalid job data '%s'; err=%s\n", lines, err.Error())
	}
	result := fmt.Sprintf("Case #%d: no solution found", input.Index)
	for i, a := range input.Items {
		for j, b := range input.Items[i+1:] {
			if a+b == input.Credit {
				result = fmt.Sprintf("Case #%d: %d %d", input.Index, i+1, i+j+2)
				break
			}
		}
	}
	rchan <- result
}


// Parse the line and return a slice with the store items (prices) found.
func parseItems(line string) (items []int, err error) {
	// Split the line into words
	fields := strings.Fields(line)
	var value int

	for i, field := range fields {
		value, err = strconv.Atoi(field)
		if err != nil {
			break
		}
		// We have a valid store item price at this point.
		if items == nil {
			items = make([]int, len(fields))
		}
		items[i] = value
	}
	return
}


func parse(count uint, lines []string) (*Input, error) {
	// Parse the credit (from line 1)
	credit, err := strconv.Atoi(lines[0])
	if err != nil {
		fmt.Printf("invalid credit '%s'; err=%s\n", lines[0], err.Error())
		return nil, err
	}

	// Parse the store item prices (from line 3)
	items, err := parseItems(lines[2])
	if err != nil {
		fmt.Printf("invalid item '%s'; err=%s\n", lines[2], err.Error())
		return nil, err
	}

	input := Input{uint(count), credit, items}
	return &input, nil
}
