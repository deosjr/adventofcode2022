package main

import (
    "fmt"

	"github.com/deosjr/adventofcode2022/lib"
)

type coord struct {
    x, y, z int
}

func neighbours(p coord) []coord {
    return []coord{
        {p.x+1, p.y, p.z},
        {p.x-1, p.y, p.z},
        {p.x, p.y+1, p.z},
        {p.x, p.y-1, p.z},
        {p.x, p.y, p.z+1},
        {p.x, p.y, p.z-1},
    }
}

func checkExternal(external, cubes map[coord]struct{}, fringe []coord, maxx,maxy,maxz int) {
    if len(fringe) == 0 {
        return
    }
    c, rest := fringe[0], fringe[1:]
    for _, n := range neighbours(c) {
        if n.x < -1 || n.y < -1 || n.z < -1 {
            continue
        }
        if n.x > maxx || n.y > maxy || n.z > maxz {
            continue
        }
        if _, ok := cubes[n]; ok {
            continue
        }
        if _, ok := external[n]; ok {
            continue
        }
        external[n] = struct{}{}
        rest = append(rest, n)
    }
    checkExternal(external, cubes, rest, maxx, maxy, maxz)
}

func day18() {
    cubes := map[coord]struct{}{}
    var maxx, maxy, maxz int
    lib.ReadFileByLine(18, func(line string) {
        var x, y, z int
        fmt.Sscanf(line, "%d,%d,%d", &x, &y, &z)
        cubes[coord{x,y,z}] = struct{}{}
        if x > maxx { maxx = x }
        if y > maxy { maxy = y }
        if z > maxz { maxz = z }
    })

    // guaranteed to be positive ints, so check -1 - max
    external := map[coord]struct{}{{-1,-1,-1}:{}}
    checkExternal(external, cubes, []coord{{0,0,0}}, maxx+1, maxy+1, maxz+1)

    var p1, p2 int
    for p := range cubes {
        for _, n := range neighbours(p) {
            if _, ok := cubes[n]; ok {
                continue
            }
            p1++
            if _, ok := external[n]; !ok {
                continue
            }
            p2++
        }
    }
    lib.WritePart1("%d", p1)
    lib.WritePart2("%d", p2)
}

func main() {
    day18()
}
