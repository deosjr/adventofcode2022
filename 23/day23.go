package main

import (
    "fmt"
    "strings"

	"github.com/deosjr/adventofcode2022/lib"
)

type coord struct {
    x, y int
}

func boundingBox(elves map[coord]struct{}) (minx, maxx, miny, maxy int) {
    for elf := range elves {
        if elf.x < minx { minx = elf.x }
        if elf.x > maxx { maxx = elf.x }
        if elf.y < miny { miny = elf.y }
        if elf.y > maxy { maxy = elf.y }
    }
    return
}

func printMap(elves map[coord]struct{}) {
    minx, maxx, miny, maxy := boundingBox(elves)
    for y:=miny; y<=maxy; y++ {
        for x:=minx; x<=maxx; x++ {
            if _, ok := elves[coord{x,y}]; ok {
                fmt.Print("#")
                continue
            }
            fmt.Print(".")
        }
        fmt.Println()
    }
    fmt.Println("---------------------")
}

func day23() {
    input := strings.Split(lib.ReadFile(23), "\n")
    elves := map[coord]struct{}{}
    for y := 0; y < len(input)-1; y++ {
        for x := 0; x < len(input[0]); x++ {
            if input[y][x] == '#' {
                elves[coord{x,y}] = struct{}{}
            }
        }
    }

    dirs := []coord{ {0, -1}, {0, 1}, {-1, 0}, {1, 0} }
    round := -1
    for {
        round++
        moves := map[coord]coord{} // to:from
        collisions := map[coord]struct{}{}
        newm := map[coord]struct{}{}
        for elf := range elves {
            _, nw := elves[coord{elf.x-1, elf.y-1}]
            _, ne := elves[coord{elf.x+1, elf.y-1}]
            _, sw := elves[coord{elf.x-1, elf.y+1}]
            _, se := elves[coord{elf.x+1, elf.y+1}]
            canmove := make([]bool, 4)
            canmove[0] = !nw && !ne
            canmove[1] = !sw && !se
            canmove[2] = !nw && !sw
            canmove[3] = !ne && !se
            for i, d := range dirs {
                if !canmove[i] {
                    continue
                }
                _, ok := elves[coord{elf.x+d.x, elf.y+d.y}]
                canmove[i] = !ok
            }
            if canmove[0] && canmove[1] && canmove[2] && canmove[3] {
                newm[elf] = struct{}{}
                continue
            }

            var c coord
            var proposed bool
            for i:=0; i<4; i++ {
                idx := (i + round) % 4
                if !canmove[idx] {
                    continue
                }
                c = coord{elf.x + dirs[idx].x, elf.y + dirs[idx].y}
                proposed = true
                break
            }
            if !proposed {
                newm[elf] = struct{}{}
                continue
            }
            // if collision, dont move and add both to newm already
            // moves should contain only valid moves after this loop
            if _, ok := collisions[c]; ok {
                // collision happened already on c
                newm[elf] = struct{}{}
                continue
            }
            clash, ok := moves[c]
            if !ok {
                moves[c] = elf
                continue
            }
            newm[elf] = struct{}{}
            newm[clash] = struct{}{}
            delete(moves, c)
            collisions[c] = struct{}{}
        }

        if len(moves) == 0 {
            break
        }
        for elf := range moves {
            newm[elf] = struct{}{}
        }
        elves = newm
        if round == 9 {
            minx, maxx, miny, maxy := boundingBox(elves)
            size := (maxx - minx + 1) * (maxy - miny + 1)
            lib.WritePart1("%d", size-len(elves))
        }
    }
    lib.WritePart2("%d", round+1)
}

func main() {
    day23()
}
