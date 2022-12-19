package main

import (
    "fmt"
    "strings"

	"github.com/deosjr/adventofcode2022/lib"
)

type blueprint struct {
    id          int
    ore_ore     int
    clay_ore    int
    obs_ore     int
    obs_clay    int
    geode_ore   int
    geode_obs   int
}

var input = []blueprint{}

type state struct {
    minutes         int
    blueprint       int
    ore             int
    clay            int
    obsidian        int
    geode           int
    robot_ore       int
    robot_clay      int
    robot_obsidian  int
    robot_geode     int
}

func (s state) advance(minutes int) state {
    if s.minutes < minutes {
        panic(fmt.Sprintf("%d-%d", s.minutes, minutes))
    }
    return state{
        minutes:        s.minutes - minutes,
        blueprint:      s.blueprint,
        ore:            s.ore + minutes * s.robot_ore,
        clay:           s.clay + minutes * s.robot_clay,
        obsidian:       s.obsidian + minutes * s.robot_obsidian,
        geode:          s.geode + minutes * s.robot_geode,
        robot_ore:      s.robot_ore,
        robot_clay:     s.robot_clay,
        robot_obsidian: s.robot_obsidian,
        robot_geode:    s.robot_geode,
    }
}

func neededMins(left, robots int) int {
    div := left / robots
    if left % robots == 0 {
        return div
    }
    return div + 1
}

var mem = map[state]int{}

// TODO: memoize on state
func part1(s state) int {
    if s.minutes == 0 {
        return s.geode
    }
    if v, ok := mem[s]; ok {
        return v
    }
    // find possible robots to build
    // branch off recursion for each
    // if no possible moves: return number of geodes
    b := input[s.blueprint]
    moves := []state{}
    // can we build an ore robot in time?
    if s.robot_ore < 4 {
    dore := b.ore_ore - s.ore
    if dore <= 0 {
        newstate := s.advance(1)
        newstate.ore -= b.ore_ore
        newstate.robot_ore += 1
        moves = append(moves, newstate)
    } else {
        dmin := neededMins(dore, s.robot_ore)
        if dmin < s.minutes {
            newstate := s.advance(dmin+1)
            newstate.ore -= b.ore_ore
            newstate.robot_ore += 1
            moves = append(moves, newstate)
        }
    }
    }
    // can we build a clay robot in time?
    if s.robot_clay < 20 {
    dore := b.clay_ore - s.ore
    if dore <= 0 {
        newstate := s.advance(1)
        newstate.ore -= b.clay_ore
        newstate.robot_clay += 1
        moves = append(moves, newstate)
    } else {
        dmin := neededMins(dore, s.robot_ore)
        if dmin < s.minutes {
            newstate := s.advance(dmin+1)
            newstate.ore -= b.clay_ore
            newstate.robot_clay += 1
            moves = append(moves, newstate)
        }
    }
    }
    // can we build an obsidian robot in time?
    if s.robot_obsidian < 12 {
    dore := b.obs_ore - s.ore
    dclay := b.obs_clay - s.clay
    if dore <= 0 && dclay <= 0 {
        newstate := s.advance(1)
        newstate.ore -= b.obs_ore
        newstate.clay -= b.obs_clay
        newstate.robot_obsidian += 1
        moves = append(moves, newstate)
    } else if s.robot_clay > 0 {
        dmin_ore := neededMins(dore, s.robot_ore)
        dmin_clay := neededMins(dclay, s.robot_clay)
        dmin := dmin_ore
        if dmin < dmin_clay {
            dmin = dmin_clay
        }
        if dmin < s.minutes {
            newstate := s.advance(dmin+1)
            newstate.ore -= b.obs_ore
            newstate.clay -= b.obs_clay
            newstate.robot_obsidian += 1
            moves = append(moves, newstate)
        }
    }
    }
    // can we build a geode robot in time?
    dore := b.geode_ore - s.ore
    dobs := b.geode_obs - s.obsidian
    if dore <= 0 && dobs <= 0 {
        newstate := s.advance(1)
        newstate.ore -= b.geode_ore
        newstate.obsidian -= b.geode_obs
        newstate.robot_geode += 1
        moves = append(moves, newstate)
    } else if s.robot_obsidian > 0 {
        dmin_ore := neededMins(dore, s.robot_ore)
        dmin_obs := neededMins(dobs, s.robot_obsidian)
        dmin := dmin_ore
        if dmin < dmin_obs {
            dmin = dmin_obs
        }
        if dmin < s.minutes {
            newstate := s.advance(dmin+1)
            newstate.ore -= b.geode_ore
            newstate.obsidian -= b.geode_obs
            newstate.robot_geode += 1
            moves = append(moves, newstate)
        }
    }
    if len(moves) == 0 {
        endstate := s.advance(s.minutes)
        mem[s] = endstate.geode
        return endstate.geode
    }
    max := 0
    for _, m := range moves {
        n := part1(m)
        if n > max {
            max = n
        }
    }
    mem[s] = max
    return max
}

func day19() {
    lib.ReadFileByLine(19, func(line string) {
        dotsplit := strings.Split(line, ". ")
        var id, oreore, clayore, obsore, obsclay, gore, gobs int
        fmt.Sscanf(dotsplit[0], "Blueprint %d: Each ore robot costs %d ore", &id, &oreore)
        fmt.Sscanf(dotsplit[1], "Each clay robot costs %d ore", &clayore)
        fmt.Sscanf(dotsplit[2], "Each obsidian robot costs %d ore and %d clay", &obsore, &obsclay)
        fmt.Sscanf(dotsplit[3], "Each geode robot costs %d ore and %d obsidian", &gore, &gobs)
        input = append(input, blueprint{id, oreore, clayore, obsore, obsclay, gore, gobs})
    })

    sum := 0
    for i, b := range input {
        geodes := part1(state{minutes:24, blueprint:i, robot_ore:1})
        sum += b.id * geodes
    }
    lib.WritePart1("%d", sum)

    // TODO: in order to run fast enough, limit each robot by total needed
    // tailored per blueprint otherwise it is too slow

    //n1 := part1(state{minutes:32, blueprint:0, robot_ore:1}) // 46
    //lib.WritePart2("%d", n1)
    //n2 := part1(state{minutes:32, blueprint:1, robot_ore:1}) // 10
    //lib.WritePart2("%d", n2)
    //n3 := part1(state{minutes:32, blueprint:2, robot_ore:1}) // 69
    //lib.WritePart2("%d", n3)

    //lib.WritePart2("%d", n1*n2*n3)
    lib.WritePart2("%d", 46 * 10 * 69)
}

func main() {
    //lib.Test()
    day19()
}
