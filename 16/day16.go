package main

import (
    "fmt"
    "strings"

	"github.com/deosjr/adventofcode2022/lib"
)

type pair struct {
    from, to string
}

type move struct {
    me string
    ele string
    meLeft int
    eleLeft int
}

func (m move) normalize() (move, int) {
    if m.meLeft == m.eleLeft {
        return move{m.me, m.ele, 0, 0}, m.meLeft
    }
    if m.meLeft < m.eleLeft {
        return move{m.me, m.ele, 0, m.eleLeft-m.meLeft}, m.meLeft
    }
    return move{m.me, m.ele, m.meLeft-m.eleLeft, 0}, m.eleLeft
}

var (
    tunnels = map[string][]string{}
    flow = map[string]int{}
    transitions = map[pair]int{}
)

func createTransitions(valve string, next []string, x int) {
    if len(next) == 0 {
        return
    }
    for _, n := range next {
        if valve == n {
            continue
        }
        p := pair{valve, n}
        if _, ok := transitions[p]; ok {
            continue
        }
        transitions[p] = x
    }
    newnext := []string{}
    for _, n := range next {
        for _, t := range tunnels[n] {
            if t == valve {
                continue
            }
            if _, ok := transitions[pair{valve, t}]; ok {
                continue
            }
            newnext = append(newnext, t)
        }
    }
    createTransitions(valve, newnext, x+1)
}

func part1(mins int, opened map[string]struct{}, valve string) int {
    if mins <= 2 {
        return 0
    }
    max := 0
    for next, f := range flow {
        if f == 0 {
            continue
        }
        if _, ok := opened[next]; ok {
            continue
        }
        cost := transitions[pair{valve, next}]
        if cost > mins {
            continue
        }
        newmins := mins-cost-1
        score := f * newmins
        newopened := map[string]struct{}{next:{}}
        for k := range opened {
            newopened[k] = struct{}{}
        }
        total := part1(newmins, newopened, next) + score
        if total > max {
            max = total
        }
    }
    return max
}

func part2(mins int, opened map[string]struct{}, m move) int {
    if mins <= 2 {
        return 0
    }
    max := 0
    myTurn := m.meLeft == 0
    newmove := move{}
    var from string
    if myTurn {
        from = m.me
        newmove.ele = m.ele
        newmove.eleLeft = m.eleLeft
    } else {
        from = m.ele
        newmove.me = m.me
        newmove.meLeft = m.meLeft
    }
    for next, f := range flow {
        if f == 0 {
            continue
        }
        if _, ok := opened[next]; ok {
            continue
        }
        cost := transitions[pair{from, next}]
        if cost > mins {
            continue
        }
        newmins := mins-cost-1
        score := f * newmins
        if myTurn {
            newmove.me = next
            newmove.meLeft = cost+1
        } else {
            newmove.ele = next
            newmove.eleLeft = cost+1
        }
        newm, elapsed := newmove.normalize()
        newopened := map[string]struct{}{next:{}}
        for k := range opened {
            newopened[k] = struct{}{}
        }
        total := part2(mins-elapsed, newopened, newm) + score
        if total > max {
            max = total
        }
    }
    return max
}

func day16() {
    lib.ReadFileByLine(16, func(line string) {
        var valve, v string
        var f int
        fmt.Sscanf(line, "Valve %s has flow rate=%d; tunnel leads to valve %s", &valve, &f, &v)
        if v == "" {
            v = strings.Split(line, "valves ")[1]
        }
        flow[valve] = f
        tunnels[valve] = strings.Split(v, ", ")
    })
    // turn this into a maximally connected graph
    // value in the map is pathlength
    for k := range tunnels {
        createTransitions(k, []string{k}, 0)
    }
    ans1 := part1(30, map[string]struct{}{}, "AA")
    lib.WritePart1("%d", ans1)
    ans2 := part2(26, map[string]struct{}{}, move{"AA", "AA", 0, 0})
    lib.WritePart2("%d", ans2)
}

func main() {
    //lib.Test()
    day16()
}
