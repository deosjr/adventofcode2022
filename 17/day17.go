package main

import (
    "strings"

	"github.com/deosjr/adventofcode2022/lib"
)

type coord struct {
    x, y int
}

var rocks = map[coord]struct{}{}

func day17() {
    input := strings.TrimSpace(lib.ReadFile(17))
    hbar := []coord{{2,0},{3,0},{4,0},{5,0}}
    plus := []coord{{3,0},{2,1},{3,1},{4,1},{3,2}}
    corn := []coord{{2,0},{3,0},{4,0},{4,1},{4,2}}
    vbar := []coord{{2,0},{2,1},{2,2},{2,3}}
    box  := []coord{{2,0},{3,0},{2,1},{3,1}}
    shapes := [][]coord{hbar, plus, corn, vbar, box}

    maxy := 0
    steps := 0
    shapeIndex := 0
    for i:=0; i<2022; i++ {
        // pick next shape and init at maxy+3
        // TODO: first 3 steps are guaranteed freefall
        shape := []coord{}
        for _, c := range shapes[shapeIndex] {
            shape = append(shape, coord{c.x, c.y+maxy+4})
        }
        shapeIndex = (shapeIndex+1) % len(shapes)
        for {
            move := input[steps]
            steps = (steps+1) % len(input)
            if move == '<' {
                // try move left (left wall at x=-1
                fail := false
                updated := []coord{}
                for _, c := range shape {
                    newc := coord{c.x-1, c.y}
                    if newc.x < 0 {
                        fail = true
                        break
                    }
                    if _, ok := rocks[newc]; ok {
                        fail = true
                        break
                    }
                    updated = append(updated, newc)
                }
                if !fail {
                    shape = updated
                }
            } else {
                // try move right (right wall at x=7)
                fail := false
                updated := []coord{}
                for _, c := range shape {
                    newc := coord{c.x+1, c.y}
                    if newc.x > 6 {
                        fail = true
                        break
                    }
                    if _, ok := rocks[newc]; ok {
                        fail = true
                        break
                    }
                    updated = append(updated, newc)
                }
                if !fail {
                    shape = updated
                }
            }
            // try move down (floor is at y=0)
            fail := false
            updated := []coord{}
            for _, c := range shape {
                newc := coord{c.x, c.y-1}
                if newc.y <= 0 {
                    fail = true
                    break
                }
                if _, ok := rocks[newc]; ok {
                    fail = true
                    break
                }
                updated = append(updated, newc)
            }
            if !fail {
                shape = updated
                continue
            }
            // if fail, we stop and save into rocks
            for _, c := range shape {
                rocks[c] = struct{}{}
                if c.y > maxy {
                    maxy = c.y
                }
            }
            break
        }
    }
    lib.WritePart1("%d", maxy)
}

func main() {
    //lib.Test()
    day17()
}
