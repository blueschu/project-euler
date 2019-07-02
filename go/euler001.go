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
	sum := 0

	for i := seq(); i < CAP; i = seq() {
		sum += i
	}
	fmt.Println(sum)
}
