package main


import (
	"fmt"
	)


func FindItems(input Input) (result string) {
	result = fmt.Sprintf("Case #%d: no solution found", input.Index)
	for i, a := range input.Items {
		for j, b := range input.Items[i+1:] {
			if a + b == input.Credit {
				template := fmt.Sprintf("Case #%d: %%d %%d", input.Index)
				if a < b {
					result = fmt.Sprintf(template, i+1, j+2)
				} else {
					result = fmt.Sprintf(template, j+2, i+1)
				}
				break
			}
		}
	}
	return
}
