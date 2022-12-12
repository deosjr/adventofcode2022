package main

import (
    "github.com/deosjr/adventofcode2022/lib"
    "github.com/deosjr/Pathfinding/path"
)

type coord struct {
    x, y int
}

type heightmap struct {
    coords map[coord]rune
}

func (hm *heightmap) Neighbours(n path.Node) []path.Node {
    p := n.(coord)
    h := hm.coords[p]
	x, y := p.x, p.y
	points := []path.Node{}
	points2d := []coord{
		{x - 1, y},
		{x, y - 1},
		{x, y + 1},
		{x + 1, y},
    }
    for _, p2d := range points2d {
		if hn, ok := hm.coords[p2d]; ok {
            if h+1 < hn {
                continue
            }
			points = append(points, p2d)
		}
	}
	return points
}

func (hm *heightmap) G(pn, qn path.Node) float64 {
    return float64(hm.coords[qn.(coord)])
}

func (*heightmap) H(pn, qn path.Node) float64 {
    return 1.0
}

func day12() {
    input := map[coord]rune{}
    y:=0
    var start, end coord
    lib.ReadFileByLine(12, func(line string) {
        for x, c := range line {
            if c == 'S' {
                start = coord{x,y}
                c = 'a'
            }
            if c == 'E' {
                end = coord{x,y}
                c = 'z'
            }
            input[coord{x, y}] = c
        }
        y++
    })

    heightmap := &heightmap{coords:input}
    route, _ := path.FindRoute(heightmap, start, end)
    l := len(route)-1
    lib.WritePart1("%d", l)

    for c, h := range input {
        if h != 'a' {
            continue
        }
        route, _ = path.FindRoute(heightmap, c, end)
        if len(route) != 0 && len(route)-1 < l {
            l = len(route)-1
        }
    }
    lib.WritePart2("%d", l)
}

func main() {
    day12()
}
