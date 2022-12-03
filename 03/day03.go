package main

import (
    "strings"
	"github.com/deosjr/adventofcode2022/lib"
)

func day03() {
    rucksacks := strings.Fields(lib.ReadFile(3))
    lib.WritePart1("%d", part1(rucksacks))
    lib.WritePart2("%d", part2(rucksacks))
}

func priority(c rune) int {
    if c > 90 {
        return int(c) - 96
    }
    return int(c) - 38
}

// for small sets, using array lookup is way faster than a map
// see day03_test for benchmarks
func overlap(in ...string) rune {
    sets := [][]struct{}{}
    for _, s := range in[1:] {
        set := make([]struct{}, 200)
        for _, c := range s {
            set[c] = struct{}{}
        }
        sets = append(sets, set)
    }
Loop:
    for _, k := range in[0] {
        for _, set := range sets {
            if set[int(k)] != struct{}{} {
                continue Loop
            }
        }
        return k
    }
    panic("no overlap")
}

func part1(in []string) int {
    sum := 0
    for _, rucksack := range in {
        n := len(rucksack) / 2
        sum += priority(overlap(rucksack[:n], rucksack[n:]))
    }
    return sum
}

func part2(in []string) int {
    sum := 0
    for i:=0; i<len(in); i+= 3 {
        sum += priority(overlap(in[i], in[i+1], in[i+2]))
    }
    return sum
}

func main() {
    day03()
}
