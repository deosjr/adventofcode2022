package main

import (
    "fmt"
    "strings"

	"github.com/deosjr/adventofcode2022/lib"
)

type cell uint8

const (
    air cell = iota
    rock
    sand
)

type coord struct {
    x, y int
}

func parseCoord(s string) coord {
    var x, y int
    fmt.Sscanf(s, "%d,%d", &x, &y)
    return coord{x,y}
}

var source = coord{500, 0}

func day14() {
    grid := map[coord]cell{}
    maxy := 0
    lib.ReadFileByLine(14, func(line string) {
        split := strings.Split(line, " -> ")
        start := parseCoord(split[0])
        for _, s := range split[1:] {
            next := parseCoord(s)
            if next.y > maxy {
                maxy = next.y
            }
            if start.x == next.x {
                from, to := start.y, next.y
                if start.y > next.y {
                    from, to = to, from
                }
                for y:=from; y<=to; y++ {
                    grid[coord{start.x, y}] = rock
                }
            } else if start.y == next.y {
                from, to := start.x, next.x
                if start.x > next.x {
                    from, to = to, from
                }
                for x:=from; x<=to; x++ {
                    grid[coord{x, start.y}] = rock
                }
            }
            start = next
        }
    })
    maxy += 1

    // part1 is destructive so lets use a copy of the grid
    p1grid := map[coord]cell{}
    for k,v := range grid {
        p1grid[k] = v
    }
    lib.WritePart1("%d", part1(p1grid, maxy))
    lib.WritePart2("%d", part2(grid, maxy))
}

func simulate(grid map[coord]cell, sandpos coord) (coord, bool) {
    if _, ok := grid[coord{sandpos.x, sandpos.y+1}]; !ok {
        return coord{sandpos.x, sandpos.y+1}, false
    }
    if _, ok := grid[coord{sandpos.x-1, sandpos.y+1}]; !ok {
        return coord{sandpos.x-1, sandpos.y+1}, false
    }
    if _, ok := grid[coord{sandpos.x+1, sandpos.y+1}]; !ok {
        return coord{sandpos.x+1, sandpos.y+1}, false
    }
    grid[sandpos] = sand
    return sandpos, true
}

func part1(grid map[coord]cell, maxy int) (rested int) {
    for {
        sandpos := source
        for {
            if sandpos.y == maxy {
                return rested
            }
            newpos, rest := simulate(grid, sandpos)
            sandpos = newpos
            if rest {
                rested++
                break
            }
        }
    }
}

func part2(grid map[coord]cell, maxy int) (rested int) {
    for {
        sandpos := source
        for {
            if sandpos.y == maxy {
                grid[sandpos] = sand
                rested++
                break
            }
            newpos, rest := simulate(grid, sandpos)
            sandpos = newpos
            if rest {
                rested++
                if sandpos == source {
                    return rested
                }
                break
            }
        }
    }
}

func main() {
    day14()
}
