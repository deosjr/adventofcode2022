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

    p1grid := map[coord]cell{}
    for k,v := range grid {
        p1grid[k] = v 
    }
    source := coord{500, 0}
    p1 := 0
Loop:
    for {
        sandpos := source
        for {
            if sandpos.y == 1000 {
                break Loop
            }
            if _, ok := p1grid[coord{sandpos.x, sandpos.y+1}]; !ok {
                sandpos = coord{sandpos.x, sandpos.y+1}
                continue
            }
            if _, ok := p1grid[coord{sandpos.x-1, sandpos.y+1}]; !ok {
                sandpos = coord{sandpos.x-1, sandpos.y+1}
                continue
            }
            if _, ok := p1grid[coord{sandpos.x+1, sandpos.y+1}]; !ok {
                sandpos = coord{sandpos.x+1, sandpos.y+1}
                continue
            }
            p1grid[sandpos] = sand
            p1++
            break
        }
    }
    fmt.Println(p1)

    maxy += 1
    p2 := 0
Loop2:
    for {
        sandpos := source
        for {
            if sandpos.y == maxy {
                grid[sandpos] = sand
                p2++
                break
            }
            if _, ok := grid[coord{sandpos.x, sandpos.y+1}]; !ok {
                sandpos = coord{sandpos.x, sandpos.y+1}
                continue
            }
            if _, ok := grid[coord{sandpos.x-1, sandpos.y+1}]; !ok {
                sandpos = coord{sandpos.x-1, sandpos.y+1}
                continue
            }
            if _, ok := grid[coord{sandpos.x+1, sandpos.y+1}]; !ok {
                sandpos = coord{sandpos.x+1, sandpos.y+1}
                continue
            }
            grid[sandpos] = sand
            p2++
            if sandpos == source {
                break Loop2
            }
            break
        }
    }
    fmt.Println(p2)
}

func main() {
    //lib.Test()
    day14()
}
