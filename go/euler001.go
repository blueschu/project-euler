package main

import "fmt"

const CAP = 1000

func MultiplesSeq(multiples ...int) func() int {
	total := 0
	return func() int {
		for {
			total++
			for _, factor := range multiples {
				if total%factor == 0 {
					return total
				}
			}
		}
	}
}

func main() {
	seq := MultiplesSeq(3, 5)
	sum, i := 0, 0

    for {
        i = seq()
        if i >= CAP {
            break
        }
        sum += i
    }
	fmt.Println(sum)
}
