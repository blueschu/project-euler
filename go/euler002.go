package main

import "fmt"

const CAP = 4000000

func FibonacciSeq() func() int {
	a, b := 0, 1
	return func() int {
		a, b = b, a+b
		return a
	}
}

func main() {
	seq := FibonacciSeq()
	sum := 0
	for i := seq(); i <= CAP; i = seq() {
		if i%2 == 0 {
			sum += i
		}
	}
	fmt.Println(sum)
}
