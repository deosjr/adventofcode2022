package main

import (
    "sort"
    "strconv"
    "strings"

	"github.com/deosjr/adventofcode2022/lib"
)

type monkey struct {
    items []int64
    operation func(int64) int64
    test func(int64) int64
}

func parseItems(s string) []int64 {
    itemstrs := strings.TrimPrefix(s, "  Starting items: ")
    ns := strings.Split(itemstrs, ",")
    items := make([]int64, len(ns))
    for i:=0; i<len(ns); i++ {
        items[i] = lib.MustParseInt(strings.TrimSpace(ns[i]))
    }
    return items
}

func parseOperation(s string) func(int64) int64 {
    fs := strings.TrimPrefix(s, "  Operation: new = old ")
    op := rune(fs[0])
    n, err := strconv.ParseInt(fs[2:], 10, 64)
    if err != nil {
        if op == '*' {
            return func(x int64) int64 { return x * x }
        }
        return func(x int64) int64 { return x + x }
    }
    if op == '*' {
        return func(x int64) int64 { return x * n }
    }
    return func(x int64) int64 { return x + n }
}

func parseTest(s []string) (func(int64) int64, int64) {
    ds := strings.TrimPrefix(s[3], "  Test: divisible by ")
    n := lib.MustParseInt(strings.TrimSpace(ds))
    ts := strings.TrimPrefix(s[4], "    If true: throw to monkey ")
    tm := lib.MustParseInt(strings.TrimSpace(ts))
    fs := strings.TrimPrefix(s[5], "    If false: throw to monkey ")
    fm := lib.MustParseInt(strings.TrimSpace(fs))
    return func(x int64) int64 {
        if x % n == 0 { return tm }
        return fm
    }, n
}

func parseMonkeys(input string) ([]*monkey, int64) {
    monkeys := []*monkey{}
    var modulo int64 = 1
    for _, ss := range strings.Split(input, "\n\n") {
        ms := strings.Split(ss, "\n")
        testfunc, test := parseTest(ms)
        modulo *= test
        monkeys = append(monkeys, &monkey{
            items: parseItems(ms[1]),
            operation: parseOperation(ms[2]),
            test: testfunc,
        })
    }
    return monkeys, modulo
}

func play(monkeys []*monkey, modulo int64, part1 bool) int64 {
    inspected := map[int]int{}
    rounds := 10000
    if part1 {
        rounds = 20
    }
    for round := 1; round<=rounds; round++ {
        for i, m := range monkeys {
            inspected[i] += len(m.items)
            for _, item := range m.items {
                wl := m.operation(item)
                if part1 {
                    wl = wl / 3
                }
                wl = wl % modulo
                nm := monkeys[m.test(wl)]
                nm.items = append(nm.items, wl)
            }
            m.items = []int64{}
        }
    }
    total := []int{}
    for _, v := range inspected {
        total = append(total, v)
    }
    sort.Ints(total)
    return int64(total[len(total)-1]) * int64(total[len(total)-2])
}

func day11() {
    input := lib.ReadFile(11)
    monkeys, modulo := parseMonkeys(input)
    lib.WritePart1("%d", play(monkeys, modulo, true))
    monkeys, modulo = parseMonkeys(input)
    lib.WritePart2("%d", play(monkeys, modulo, false))
}

func main() {
    day11()
}
