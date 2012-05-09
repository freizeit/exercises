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
)

// This function deals with a single case. It finds the two store items
// whose prices add up to the credit granted and writes their indices
// (1-based) to the result channel.
func FindItems(input Input, rchan chan string) {
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
