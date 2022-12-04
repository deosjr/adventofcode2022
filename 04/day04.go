package main

import (
    "fmt"

	"github.com/deosjr/adventofcode2022/lib"
)

func day04() {
    var p1, p2 int
    lib.ReadFileByLine(4, func(line string) {
        var s1, e1, s2, e2 int
        fmt.Sscanf(line, "%d-%d,%d-%d", &s1, &e1, &s2, &e2)
        switch {
        case s2 >= s1 && e2 <= e1 && e2 >= s1:
            p1++
        case s1 >= s2 && e1 <= e2 && e1 >= s2:
            p1++
        }
        if !(e1 < s2 || e2 < s1) {
            p2++
        }
    })
    lib.WritePart1("%d", p1)
    lib.WritePart2("%d", p2)
}

func main() {
    day04()
}
