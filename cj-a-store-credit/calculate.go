package main


import (
	"fmt"
	)


func FindItems(input Input) (result string) {
	result = fmt.Sprintf("Case #%d: no solution found", input.Index)
	for i, a := range input.Items {
		for j, b := range input.Items[i+1:] {
			if a + b == input.Credit {
				result = fmt.Sprintf("Case #%d: %d %d", input.Index, i+1, i+j+2)
				break
			}
		}
	}
	return
}
