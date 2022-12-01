package main

import (
	"sort"
	"strings"

	"github.com/deosjr/adventofcode2022/lib"
)

func day01() {
	split := strings.Split(lib.ReadFile(1), "\n\n")
	n := len(split)
	calories := make([]int, n)
	for i, elf := range split {
		total := 0
		for _, s := range strings.Fields(elf) {
			total += int(lib.MustParseInt(s))
		}
		calories[i] = total
	}
	sort.Ints(calories)
	lib.WritePart1("%d", calories[n-1])
	lib.WritePart2("%d", calories[n-1]+calories[n-2]+calories[n-3])
}

func main() {
    //lib.Test()
	day01()
	//day01_linebyline()
	//day01_mapreduce()
	//lib.Profiled(day01)
	//lib.Profiled(day01_linebyline)
	//lib.Profiled(day01_mapreduce)
}
