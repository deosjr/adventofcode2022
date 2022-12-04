package main

import (
    "strings"
    "testing"

    "github.com/deosjr/adventofcode2022/lib"
)

// NOTE on benchmarking: search in array vs in map?
// for small arrays might be faster to not compute hashes..
// see for example https://kokes.github.io/blog/2020/07/20/tiny-maps-arrays-go.html

func Benchmark03Maps(b *testing.B) {
    rucksacks := strings.Fields(lib.ReadFile(3))
    for n := 0; n < b.N; n++ {
        part2_maps(rucksacks)
    }
}

func Benchmark03Arrays(b *testing.B) {
    rucksacks := strings.Fields(lib.ReadFile(3))
    for n := 0; n < b.N; n++ {
        part2_arrays(rucksacks)
    }
}

func Benchmark03Strings(b *testing.B) {
    rucksacks := strings.Fields(lib.ReadFile(3))
    for n := 0; n < b.N; n++ {
        part2_strings(rucksacks)
    }
}

// alternative (original) version of intersection check using map
func overlapMaps(in ...string) rune {
    sets := []map[rune]struct{}{}
    for _, s := range in {
        set := map[rune]struct{}{}
        for _, c := range s {
            set[c] = struct{}{}
        }
        sets = append(sets, set)
    }
Loop:
    for k := range sets[0] {
        for _, set := range sets[1:] {
            if _, ok := set[k]; !ok {
                continue Loop
            }
        }
        return k
    }
    panic("no overlap")
}

func overlapStrings(in ...string) rune {
Loop:
    for _, c := range in[0] {
        for _, s := range in[1:] {
            if !strings.ContainsRune(s, c) {
                continue Loop
            }
        }
        return c
    }
    panic("no overlap")
}

func part2_maps(in []string) int {
    sum := 0
    for i:=0; i<len(in); i+= 3 {
        sum += int(overlapMaps(in[i], in[i+1], in[i+2]))
    }
    return sum
}

func part2_arrays(in []string) int {
    sum := 0
    for i:=0; i<len(in); i+= 3 {
        sum += int(overlap(in[i], in[i+1], in[i+2]))
    }
    return sum
}

func part2_strings(in []string) int {
    sum := 0
    for i:=0; i<len(in); i+= 3 {
        sum += int(overlapStrings(in[i], in[i+1], in[i+2]))
    }
    return sum
}
