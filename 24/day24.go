package main

import (
    "math"
    "strings"

	"github.com/deosjr/adventofcode2022/lib"
    "github.com/deosjr/Pathfinding/path"
)

type coord struct {
    x, y int
}

const (
    up int = iota
    down
    left
    right
)

var dirs = []coord{{0, -1}, {0, 1}, {-1, 0}, {1, 0}}

type valley struct {
    minx, maxx int
    miny, maxy int
    start, end coord
    blizzards map[coord]int
    endmin int
}

type node struct {
    c coord
    minute int
}

var mem = map[node]bool{}

// true if there is a blizzard at coord c at minute m
func (v *valley) blizzard(m int, c coord) bool {
    if ans, ok := mem[node{c, m}]; ok {
        return ans
    }
    for p, d := range v.blizzards {
        dir := dirs[d]
        sizex := v.maxx - v.minx + 1
        sizey := v.maxy - v.miny + 1
        // + 50 * size because modulo doesnt wrap negative numbers...
        x := (p.x + m*dir.x + 50*sizex) % sizex
        y := (p.y + m*dir.y + 50*sizey) % sizey
        if c.x == x && c.y == y {
            mem[node{c, m}] = true
            return true
        }
    }
    mem[node{c, m}] = false
    return false
}

func (v *valley) Neighbours(n path.Node) []path.Node {
    nn := n.(node)
	x, y := nn.c.x, nn.c.y
	points := []path.Node{}
	points2d := []coord{
        {x, y},
		{x - 1, y},
		{x, y - 1},
		{x, y + 1},
		{x + 1, y},
    }
    if manhattan(nn.c, v.end) + nn.minute > v.endmin {
        return points
    }
    for _, p2d := range points2d {
        if p2d.x == v.start.x && p2d.y == v.start.y {
	        points = append(points, node{p2d, nn.minute+1})
            continue
        }
        if p2d.x == v.end.x && p2d.y == v.end.y {
	        points = append(points, node{p2d, nn.minute+1})
            continue
        }
        if p2d.x < v.minx || p2d.x > v.maxx {
            continue
        }
        if p2d.y < v.miny || p2d.y > v.maxy {
            continue
        }
        if v.blizzard(nn.minute+1, p2d) {
            continue
        }
	    points = append(points, node{p2d, nn.minute+1})
	}
	return points
}

func (v *valley) G(pn, qn path.Node) float64 {
    return 1.0
}

func (*valley) H(pn, qn path.Node) float64 {
    return float64(manhattan(pn.(node).c, qn.(node).c))
}

func manhattan(p, q coord) int {
    return int(math.Abs(float64(p.x - q.x)) + math.Abs(float64(p.y - q.y)))
}

func day24() {
    input := strings.Split(lib.ReadFile(24), "\n")
    input = input[:len(input)-1] // strip off newline
    // here the bounds include the walls (minx/maxx make assumptions on sides)
    minx, maxx := 1, len(input[0])-2
    miny, maxy := 0, len(input)-1
    // but for coords, we want to shift so that minx and miny are both 0
    // to ease modulo arithmetic. We also move the grid inside the walls
    blizzards := map[coord]int{}
    var start, end coord
    for x:=minx; x<=maxx; x++ {
        if input[miny][x] == '.' {
            start = coord{x-1, miny-1}
            break
        }
    }
    for x:=minx; x<=maxx; x++ {
        if input[maxy][x] == '.' {
            end = coord{x-1, maxy-1}
            break
        }
    }
    for y := miny+1; y < maxy; y++ {
        for x := minx; x <= maxx; x++ {
            switch input[y][x] {
            case '.':
                continue
            case '^':
                blizzards[coord{x-1, y-1}] = up
            case 'v':
                blizzards[coord{x-1, y-1}] = down
            case '<':
                blizzards[coord{x-1, y-1}] = left
            case '>':
                blizzards[coord{x-1, y-1}] = right
            }
        }
    }
    var p1, back, p2 int
    v := &valley{minx-1, maxx-1, miny, maxy-2, start, end, blizzards, 0}
    for i:=manhattan(start, end); i<1000; i++ {
        v.endmin = i
        route, _ := path.FindRoute(v, node{start, 0}, node{end, i})
        if len(route) != 0 {
            p1 = len(route)-1
            break
        }
    }
    lib.WritePart1("%d", p1)
    v.start, v.end = v.end, v.start
    for i:=p1; i<1000; i++ {
        v.endmin = i
        route, _ := path.FindRoute(v, node{end, p1}, node{start, i})
        if len(route) != 0 {
            back = len(route)-1
            break
        }
    }
    v.start, v.end = v.end, v.start
    for i:=p1+back; i<1000; i++ {
        v.endmin = i
        route, _ := path.FindRoute(v, node{start, p1+back}, node{end, i})
        if len(route) != 0 {
            p2 = len(route)-1
            break
        }
    }
    lib.WritePart2("%d", p1+back+p2)
}

func main() {
    day24()
}
