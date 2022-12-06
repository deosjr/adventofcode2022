package main

import (
	"github.com/deosjr/adventofcode2022/lib"
)

func allDistinct(slice string) bool {
    for i, c := range slice {
        for j:= i+1;j<len(slice); j++ {
            if byte(c) == slice[j] {
                return false
            }
        }
    }
    return true
}

func process(input string, n int) int {
    window := input[:n-1]
    for i, c := range input[n-1:] {
        window += string(c)
        if allDistinct(window) {
            return i+n
        }
        window = window[1:]
    }
    return -1
}

func day06() {
    input := lib.ReadFile(6)
    lib.WritePart1("%d", process(input, 4))
    lib.WritePart2("%d", process(input, 14))
}

func main() {
    day06()
}
