package main

import (
    "fmt"
    "strings"

	"github.com/deosjr/adventofcode2022/lib"
)

type pair struct {
    from, to uint64
}

type move struct {
    me      uint64
    ele     uint64
    meLeft  uint64
    eleLeft uint64
}

func (m move) normalize() (move, uint64) {
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
    flow = map[string]uint64{}
    transitions = map[pair]uint64{}

    valve2int = map[string]uint64{}
    p1mem = map[uint64]uint64{}
    p2mem = map[uint64]uint64{}
)

func createTransitions(valve string, next []string, x uint64) {
    if len(next) == 0 {
        return
    }
    for _, n := range next {
        if valve == n {
            continue
        }
        p := pair{valve2int[valve], valve2int[n]}
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
            if _, ok := transitions[pair{valve2int[valve], valve2int[t]}]; ok {
                continue
            }
            newnext = append(newnext, t)
        }
    }
    createTransitions(valve, newnext, x+1)
}

func alreadyOpen(valve uint64, mask uint64) bool {
    return ((1 << valve) & mask) != 0
}

func addToOpen(valve uint64, mask uint64) uint64 {
    return mask | (1 << valve)
}

func hash1(mins, valve uint64, opened uint64) uint64 {
    // mins in [0-30], valve in [0,15], opened needs 15 bits
    // which means 5, 4 and 15 bits respectively packed into a uint32
    // but we use a uint64 because thats what we use in part2
    var mask uint64
    mask |= valve << 30
    mask |= mins << 16
    return mask | opened
}

func part1(mins, opened, valve uint64) uint64 {
    key := hash1(mins, valve, opened)
    if v, ok := p1mem[key]; ok {
        return v
    }
    if mins <= 2 || mins > 100 {
        return 0
    }
    var max uint64 = 0
    for next_s, f := range flow {
        if f == 0 {
            continue
        }
        next := valve2int[next_s]
        if alreadyOpen(next, opened) {
            continue
        }
        cost := transitions[pair{valve, next}]
        if cost >= mins {
            continue
        }
        newmins := mins-cost-1
        score := f * newmins
        newopened := addToOpen(next, opened)
        total := part1(newmins, newopened, next) + score
        if total > max {
            max = total
        }
    }
    p1mem[key] = max
    return max
}

func hash2(mins, opened uint64, m move) uint64 {
    // mins in [0-26], opened needs 15 bits, move is 2*valve in [0,15] and 2*left in [0,15]
    // meaning 5, 4, 4, 4, 4 and 15 bits
    var mask uint64
    mask |= m.me << 50
    mask |= m.meLeft << 40
    mask |= m.ele << 30
    mask |= m.eleLeft << 20
    mask |= mins << 16
    return mask | opened
}

func part2(mins, opened uint64, m move) uint64 {
    key := hash2(mins, opened, m)
    if v, ok := p2mem[key]; ok {
        return v
    }
    if mins <= 2 || mins > 100 {
        return 0
    }
    var max uint64
    myTurn := m.meLeft == 0
    newmove := move{}
    var from uint64
    if myTurn {
        from = m.me
        newmove.ele = m.ele
        newmove.eleLeft = m.eleLeft
    } else {
        from = m.ele
        newmove.me = m.me
        newmove.meLeft = m.meLeft
    }
    for next_s, f := range flow {
        if f == 0 {
            continue
        }
        next := valve2int[next_s]
        if alreadyOpen(next, opened) {
            continue
        }
        cost := transitions[pair{from, next}]
        if cost >= mins {
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
        newopened := addToOpen(next, opened)
        total := part2(mins-elapsed, newopened, newm) + score
        if total > max {
            max = total
        }
    }
    p2mem[key] = max
    return max
}

func day16() {
    lib.ReadFileByLine(16, func(line string) {
        var valve, v string
        var f uint64
        fmt.Sscanf(line, "Valve %s has flow rate=%d; tunnel leads to valve %s", &valve, &f, &v)
        if v == "" {
            v = strings.Split(line, "valves ")[1]
        }
        flow[valve] = f
        tunnels[valve] = strings.Split(v, ", ")
        if f != 0 || valve == "AA" {
            valve2int[valve] = uint64(len(valve2int))
        }
    })
    // this ensures the important valve ints fit into 16 bits
    for v := range flow {
        if _, ok := valve2int[v]; !ok {
            valve2int[v] = uint64(len(valve2int))
        }
    }
    // turn this into a maximally connected graph
    // value in the map is pathlength
    for k := range tunnels {
        createTransitions(k, []string{k}, 0)
    }
    aa := valve2int["AA"]
    ans1 := part1(30, 0, aa)
    lib.WritePart1("%d", ans1)
    ans2 := part2(26, 0, move{aa, aa, 0, 0})
    lib.WritePart2("%d", ans2)
}

func main() {
    day16()
}
