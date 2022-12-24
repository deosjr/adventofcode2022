package main

import (
    "container/heap"
    "fmt"
    "math"
    "strings"

	"github.com/deosjr/adventofcode2022/lib"
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
        x := (((p.x + m*dir.x) % sizex) + sizex) % sizex
        y := (((p.y + m*dir.y) % sizey) + sizey) % sizey
        if c.x == x && c.y == y {
            mem[node{c, m}] = true
            return true
        }
    }
    mem[node{c, m}] = false
    return false
}

func (v *valley) Neighbours(n node) []node {
	x, y := n.c.x, n.c.y
	points := []node{}
	points2d := []coord{
        {x, y},
		{x - 1, y},
		{x, y - 1},
		{x, y + 1},
		{x + 1, y},
    }
    if manhattan(n.c, v.end) + n.minute > v.endmin {
        return points
    }
    for _, p2d := range points2d {
        if p2d.x == v.start.x && p2d.y == v.start.y {
	        points = append(points, node{p2d, n.minute+1})
            continue
        }
        if p2d.x == v.end.x && p2d.y == v.end.y {
	        points = append(points, node{p2d, n.minute+1})
            continue
        }
        if p2d.x < v.minx || p2d.x > v.maxx {
            continue
        }
        if p2d.y < v.miny || p2d.y > v.maxy {
            continue
        }
        if v.blizzard(n.minute+1, p2d) {
            continue
        }
	    points = append(points, node{p2d, n.minute+1})
	}
	return points
}

func (v *valley) G(p, n node) float64 {
    return 1.0
}

func (*valley) H(p, q node) float64 {
    return float64(manhattan(p.c, q.c))
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
    var v Map[node] = &valley{minx-1, maxx-1, miny, maxy-2, start, end, blizzards, 0}
    for i:=manhattan(start, end); i<1000; i++ {
        v.(*valley).endmin = i
        route, _ := FindRoute(v, node{start, 0}, node{end, i})
        if len(route) != 0 {
            p1 = len(route)-1
            break
        }
    }
    lib.WritePart1("%d", p1)
    v.(*valley).start, v.(*valley).end = v.(*valley).end, v.(*valley).start
    for i:=p1; i<1000; i++ {
        v.(*valley).endmin = i
        route, _ := FindRoute(v, node{end, p1}, node{start, i})
        if len(route) != 0 {
            back = len(route)-1
            break
        }
    }
    v.(*valley).start, v.(*valley).end = v.(*valley).end, v.(*valley).start
    for i:=p1+back; i<1000; i++ {
        v.(*valley).endmin = i
        route, _ := FindRoute(v, node{start, p1+back}, node{end, i})
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

// copied from pathfinding lib!

type Map[T comparable] interface {
	// get neighbours for a node
	Neighbours(node T) []T

	// cost of the path from start to node n
	// G() only returns the cost of moving from n to
	// one of its neighbours
	G(n, neighbour T) float64

	// heuristic cost estimate function:
	// cost of cheapest path from n to goal
	H(n, goal T) float64
}

func FindRoute[T comparable](m Map[T], start, goal T) ([]T, error) {
	openSet := map[T]bool{
		start: true,
	}
	// closedSet := map[Node]bool{}

	cameFrom := map[T]T{}

	gScore := map[T]float64{}
	gScore[start] = 0

	fScore := map[T]float64{}
	fScore[start] = m.H(start, goal)

	pq := priorityQueue[T]{
		&pqItem[T]{
			node:   start,
			fScore: fScore[start],
			index:  0,
		},
	}
	heap.Init(&pq)

	// use goalScore when you want to be able to revisit goal
	// instead of returning the first path found
	goalScore := float64(math.MaxInt64)

	for pq.Len() != 0 {
		item := heap.Pop(&pq).(*pqItem[T])
		if item.fScore == math.MaxInt32 {
			break
		}
		current := item.node
		if current == goal {
			goalScore = gScore[current]
			// return reconstructPath(cameFrom, current), nil
		}

		// note: nodes can never be revisited if closedSet is used
		// closedSet[current] = true
		delete(openSet, current)

		for _, n := range m.Neighbours(current) {
			// if closedSet[n] {
			// 	continue
			// }
			tentativeGscore := gScore[current] + m.G(current, n)
			f := tentativeGscore + m.H(n, goal)

			v, ok := fScore[n]
			// only add to openSet if
			// - not in openSet yet
			// - has never been explored before or if it has, f < previous f score
			// - if goal has been found, only explore nodes for which f < current goal score
			if !openSet[n] && (!ok || f < v) && f < goalScore {
				openSet[n] = true
				item := &pqItem[T]{
					node:   n,
					fScore: f,
				}
				heap.Push(&pq, item)
			} else if tentativeGscore >= gScore[n] {
				continue
			}
			cameFrom[n] = current
			gScore[n] = tentativeGscore
			fScore[n] = f
		}
	}

	if goalScore == math.MaxInt64 {
		return nil, fmt.Errorf("No path found")
	}
	return reconstructPath(cameFrom, goal), nil
}

func reconstructPath[T comparable](m map[T]T, current T) []T {
	path := []T{current}
	for {
		prev, ok := m[current]
		if !ok {
			break
		}
		current = prev
		path = append(path, current)
	}
	return path
}

type pqItem[T comparable] struct {
	node   T
	fScore float64
	index  int
}

type priorityQueue[T comparable] []*pqItem[T]

func (pq priorityQueue[T]) Len() int { return len(pq) }

func (pq priorityQueue[T]) Less(i, j int) bool {
	return pq[i].fScore < pq[j].fScore
}

func (pq priorityQueue[T]) Swap(i, j int) {
	pq[i], pq[j] = pq[j], pq[i]
	pq[i].index = i
	pq[j].index = j
}

func (pq *priorityQueue[T]) Push(x interface{}) {
	n := len(*pq)
	item := x.(*pqItem[T])
	item.index = n
	*pq = append(*pq, item)
}

func (pq *priorityQueue[T]) Pop() interface{} {
	old := *pq
	n := len(old)
	item := old[n-1]
	item.index = -1
	*pq = old[0 : n-1]
	return item
}
