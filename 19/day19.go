package main

import (
    "fmt"
    "strings"

	"github.com/deosjr/adventofcode2022/lib"
)

type resource int

const (
    ore resource = iota
    clay
    obsidian
    geode
)

type blueprint struct {
    id              int
    costs           [4][3]int
    maxOre          int
}

var input = []blueprint{}

type state struct {
    minutes         int
    blueprint       int
    resources       [4]int
    robots          [4]int
}

func (s state) advance(minutes int) state {
    b := input[s.blueprint]
    newres := [4]int{}
    for i, r := range s.resources {
        newres[i] = r + minutes * s.robots[i]
        // TODO: normalize: if you have more resources then you can spend
        // then you can throw away the excess to cut down the state space 
        // (thank you u/jonathan_paulson)
        if i == 0 && newres[i] > 2*b.maxOre {
            newres[0] = 2*b.maxOre
        }
        if i == 1 && newres[i] > 3*b.costs[obsidian][clay] {
            newres[1] = 3*b.costs[obsidian][clay]
        }
        if i == 2 && newres[i] > 3*b.costs[geode][obsidian] {
            newres[2] = 3*b.costs[geode][obsidian]
        }
    }
    return state{
        minutes:        s.minutes - minutes,
        blueprint:      s.blueprint,
        resources:      newres,
        robots:         s.robots,
    }
}

func neededMins(left, robots int) int {
    div := left / robots
    if left % robots == 0 {
        return div
    }
    return div + 1
}

func (s state) canBuild(robot resource) (state, bool) {
    b := input[s.blueprint]
    cost := b.costs[robot]
    var other int
    for i, c := range cost {
        if c != 0 {
            other = i
        }
    }

    oreDiff := cost[ore] - s.resources[ore]
    otherDiff := cost[other] - s.resources[other]
    if oreDiff <= 0 && otherDiff <= 0 {
        newstate := s.advance(1)
        newstate.resources[ore] -= cost[ore]
        if other > 0 {
            newstate.resources[other] -= cost[other]
        }
        newstate.robots[robot] += 1
        return newstate, true
    }
    if s.robots[other] <= 0 {
        return state{}, false
    }

    diffMinsOre := neededMins(oreDiff, s.robots[ore])
    diffMinsOther := neededMins(otherDiff, s.robots[other])
    diffMinutes := diffMinsOre
    if diffMinsOther > diffMinutes {
        diffMinutes = diffMinsOther
    }
    if diffMinutes >= s.minutes {
        return state{}, false
    }
    newstate := s.advance(diffMinutes+1)
    newstate.resources[ore] -= cost[ore]
    if other > 0 {
        newstate.resources[other] -= cost[other]
    }
    newstate.robots[robot] += 1
    return newstate, true
}

var mem = map[state]int{}

func plan(s state) int {
    if s.minutes == 0 {
        return s.resources[geode]
    }
    if v, ok := mem[s]; ok {
        return v
    }
    b := input[s.blueprint]
    // NOTE: if we can build a geode robot every turn, we can calculate the optimal score easily
    // doesn't seem to speed things up though, perhaps because Im doing recursion instead of DFS?
    if s.robots[ore] == b.costs[geode][ore] && s.robots[obsidian] == b.costs[geode][obsidian] {
        n := s.resources[geode] + (s.minutes * (s.minutes+1))/2
        mem[s] = n
        return n
    }
    // find possible robots to build
    // branch off recursion for each
    // don't build more than max needed of resource per build
    // if no possible moves: return number of geodes
    moves := []state{}

    // can we build an ore robot in time?
    if s.robots[ore] < b.maxOre {
        move, ok := s.canBuild(ore)
        if ok {
            moves = append(moves, move)
        }
    }
    // can we build a clay robot in time? (only obsidian costs clay)
    if s.robots[clay] < b.costs[obsidian][clay] {
        move, ok := s.canBuild(clay)
        if ok {
            moves = append(moves, move)
        }
    }
    // can we build an obsidian robot in time? (only geode costs obsidian)
    if s.robots[obsidian] < b.costs[geode][obsidian] {
        move, ok := s.canBuild(obsidian)
        if ok {
            moves = append(moves, move)
        }
    }
    // can we build a geode robot in time?
    move, ok := s.canBuild(geode)
    if ok {
        moves = append(moves, move)
    }

    if len(moves) == 0 {
        endstate := s.advance(s.minutes)
        mem[s] = endstate.resources[geode]
        return endstate.resources[geode]
    }
    max := 0
    for _, m := range moves {
        n := plan(m)
        if n > max {
            max = n
        }
    }
    mem[s] = max
    return max
}

func newBlueprint(id, oreore, clayore, obsore, obsclay, gore, gobs int) blueprint {
    maxore := oreore
    if clayore > maxore { maxore = clayore }
    if obsore > maxore { maxore = obsore }
    if gore > maxore { maxore = gore }
    oreCost := [3]int{oreore,0,0}
    clayCost := [3]int{clayore,0,0}
    obsCost := [3]int{obsore,obsclay,0}
    geodeCost := [3]int{gore,0,gobs}
    return blueprint{
        id: id,
        maxOre: maxore,
        costs: [4][3]int{oreCost, clayCost, obsCost, geodeCost},
    }
}

func day19() {
    lib.ReadFileByLine(19, func(line string) {
        dotsplit := strings.Split(line, ". ")
        var id, oreore, clayore, obsore, obsclay, gore, gobs int
        fmt.Sscanf(dotsplit[0], "Blueprint %d: Each ore robot costs %d ore", &id, &oreore)
        fmt.Sscanf(dotsplit[1], "Each clay robot costs %d ore", &clayore)
        fmt.Sscanf(dotsplit[2], "Each obsidian robot costs %d ore and %d clay", &obsore, &obsclay)
        fmt.Sscanf(dotsplit[3], "Each geode robot costs %d ore and %d obsidian", &gore, &gobs)
        input = append(input, newBlueprint(id, oreore, clayore, obsore, obsclay, gore, gobs))
    })

    sum := 0
    for i, b := range input {
        geodes := plan(state{minutes:24, blueprint:i, robots:[4]int{1,0,0,0}})
        sum += b.id * geodes
    }
    lib.WritePart1("%d", sum)

    n1 := plan(state{minutes:32, blueprint:0, robots:[4]int{1,0,0,0}}) // 46
    n2 := plan(state{minutes:32, blueprint:1, robots:[4]int{1,0,0,0}}) // 10
    n3 := plan(state{minutes:32, blueprint:2, robots:[4]int{1,0,0,0}}) // 69
    lib.WritePart2("%d", n1*n2*n3)
}

func main() {
    day19()
}
