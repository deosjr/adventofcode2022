package main

import (
    "strings"

	"github.com/deosjr/adventofcode2022/lib"
)

type coord struct {
    x, y int
}

var rocks = map[coord]struct{}{}
var mem = map[int]map[int]map[uint64]coord{}

func day17() {
    input := strings.TrimSpace(lib.ReadFile(17))
    hbar := []coord{{2,0},{3,0},{4,0},{5,0}}
    plus := []coord{{3,0},{2,1},{3,1},{4,1},{3,2}}
    corn := []coord{{2,0},{3,0},{4,0},{4,1},{4,2}}
    vbar := []coord{{2,0},{2,1},{2,2},{2,3}}
    box  := []coord{{2,0},{3,0},{2,1},{3,1}}
    shapes := [][]coord{hbar, plus, corn, vbar, box}

    for i := range shapes {
        mem[i] = map[int]map[uint64]coord{}
        for j := range input {
            mem[i][j] = map[uint64]coord{}
        }
    }

    maxy := 0
    steps := 0
    shapeIndex := 0
    for i:=0; i<2022; i++ {
    //for i:=0; i<11000; i++ {
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
            // now memoize the top 9 layers (because 9*7 fits into 64)
            // as a single bitstring storing it + maxy
            if maxy < 10 {
                break
            }
            var mask uint64
            for y:=0;y<9;y++{
                for x:=0;x<7;x++ {
                    yy := maxy-y
                    if _, ok := rocks[coord{x,yy}]; !ok {
                        continue
                    }
                    // set 9*y+x 'th bit in mask to 1
                    mask |= 1 << ((9*y)+x)
                }
            }
            /*
            if v, ok := mem[shapeIndex][steps][mask]; ok {
                lib.WritePart2("%v, %d, %d", v, i+1, maxy)
            }
            */
            mem[shapeIndex][steps][mask] = coord{i+1, maxy}
            break
        }
    }
    lib.WritePart1("%d", maxy)

    // calculate loop by hand using print statements...
    /*
    var n int64 = 1000000000000
    //var di int64 = 35
    //var dy int64 = 53
    // ????????
    // there is an earlier loop at the start which is invalid???
    //var di int64 = 2599 - 869
    //var dy int64 = 4073 - 1345
    // {1000 1555}, 2725, 4283
    var di int64 = 2725 - 1000
    var dy int64 = 4283 - 1555
    times := n/di - 4
    dif := n - times*di
    timesy := times * dy
    lib.WritePart2("%d %d", timesy, dif)
    */

    // 1576878609768 1870
    // {1870 2928}
    // 1576878612696

    // 1576878607040 3600
    // {3600 5666}
    // 1576878612706

    // 1576878585216 17440
    // {17440 27550}
    // 1576878612766

    // 1581449261920 8500
    // {8500 13399}
    lib.WritePart2("%d", 1581449261920 + 13399)

}

func main() {
    //lib.Test()
    day17()
}
