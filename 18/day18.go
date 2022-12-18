package main

import (
    "fmt"
    "math"

	"github.com/deosjr/adventofcode2022/lib"
    "github.com/deosjr/Pathfinding/path"
)

type coord struct {
    x, y, z int
}

type scan struct {
    coords map[coord]struct{}
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

func (s *scan) Neighbours(n path.Node) []path.Node {
    p := n.(coord)
	points := []path.Node{}
    for _, nb := range neighbours(p) {
		if _, ok := s.coords[nb]; ok {
            continue
        }
		points = append(points, nb)
	}
	return points
}

func (s *scan) G(pn, qn path.Node) float64 {
    return 1.0
}

func (*scan) H(pn, qn path.Node) float64 {
    p, q := pn.(coord), qn.(coord)
    dx := math.Abs(float64(p.x - q.x))
    dy := math.Abs(float64(p.x - q.x))
    dz := math.Abs(float64(p.x - q.x))
    return math.Sqrt(dx*dx + dy*dy + dz*dz)
}

func day18() {
    m := map[coord]struct{}{}
    s := &scan{m}
    lib.ReadFileByLine(18, func(line string) {
        var x, y, z int
        fmt.Sscanf(line, "%d,%d,%d", &x, &y, &z)
        m[coord{x,y,z}] = struct{}{}
    })
    var p1, p2 int
    for p := range m {
        for _, n := range neighbours(p) {
            if _, ok := m[n]; ok {
                continue
            }
            p1++
            // TODO: this is dumb overkill. instead, take bounding box once
            // propagate external and keep in map, then just check that here
            route, _ := path.FindRoute(s, n, coord{0,0,0})
            if len(route) == 0 {
                continue
            }
            p2++
        }
    }
    lib.WritePart1("%d", p1)
    lib.WritePart2("%d", p2)
}

func main() {
    //lib.Test()
    day18()
}
