package main


import (
	"fmt"
	"os"
	"strconv"
	"strings"
	)


type Input struct {
	Index uint	// input number, 1-based
	Credit uint	// store credit
	Items []uint	// store items (prices)
	}


func InputChan(data []byte) chan Input {
	ch := make(chan Input)
	go func() {
		lines := strings.Split(strings.TrimSpace(string(data)), "\n", -1)
		fmt.Printf("number of lines: %v\n", len(lines))
		fmt.Printf("lines: %v\n", lines)

		var credit uint
		var err os.Error

		for i, line := range lines {
			if i == 0 {	// ignore the total number of inputs
				continue
			}
			i -= 1
			switch i % 3 {
				case 0:	// credit
					credit, err = strconv.Atoui(line)
					if err != nil {
						fmt.Printf("invalid credit '%s'; err=%s\n",
									  line, err.String())
						os.Exit(2)
					}
				case 2:	// the store items
					fields := strings.Fields(line)
					items := make([]uint, len(fields))
					for fi, field := range(fields) {
						items[fi], err = strconv.Atoui(field)
						if err != nil {
							fmt.Printf("invalid store item '%s'; err=%s\n",
										  field, err.String())
							os.Exit(3)
						}
					}
					input := Input{uint((i+1)/3), credit, items}
					ch <- input
			}
		}
		close(ch)
	}()
	return ch
}
