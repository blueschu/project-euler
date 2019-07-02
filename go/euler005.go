package main

import "fmt"

const CAP = 20

func makeDivisibilityChecker(factors ...int) func(int) bool {
	return func(x int) bool {
		for _, factor := range factors {
			if x%factor != 0 {
				return false
			}
		}
		return true
	}
}

func main() {
	factors := make([]int, CAP)

	for i := 0; i < CAP; i++ {
		factors[i] = i + 1
	}

	checker := makeDivisibilityChecker(factors...)

	counter := 1

	for !checker(counter) {
		counter++
	}

	fmt.Println(counter)
}
