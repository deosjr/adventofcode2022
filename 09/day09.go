package main

import (
    "fmt"

	"github.com/deosjr/adventofcode2022/lib"
)

type coord struct {
    x, y int
}

func (p coord) add (q coord) coord {
    return coord{x: p.x+q.x, y: p.y+q.y}
}

func lookupDir(dir rune) coord {
    switch dir {
    case 'U':
        return coord{0,1}
    case 'L':
        return coord{-1,0}
    case 'D':
        return coord{0,-1}
    case 'R':
        return coord{1,0}
    }
    panic("unknown dir")
}

func abs(n int) int {
    if n < 0 {
        return -n
    }
    return n
}

func move(head, tail coord) coord {
    dx := head.x - tail.x
    dy := head.y - tail.y
    switch {
    case dx == 0 && abs(dy) > 1:
        tail.y += dy - (dy/abs(dy))
    case dy == 0 && abs(dx) > 1:
        tail.x += dx - (dx/abs(dx))
    case dx != 0 && dy != 0 && abs(dx)+abs(dy) > 2:
        tail.x += dx / abs(dx)
        tail.y += dy / abs(dy)
    }
    return tail
}

func day09() {
    rope := [10]coord{}
    pos1 := map[coord]struct{}{coord{0,0}: {}}
    pos9 := map[coord]struct{}{coord{0,0}: {}}
    lib.ReadFileByLine(9, func(line string) {
        var dir rune
        var n int
        fmt.Sscanf(line, "%c %d", &dir, &n)
        d := lookupDir(dir)
        for i:=0; i<n; i++ {
            rope[0] = rope[0].add(d)
            for j:=1; j<10; j++ {
                rope[j] = move(rope[j-1], rope[j])
            }
            pos1[rope[1]] = struct{}{}
            pos9[rope[9]] = struct{}{}
        }
    })
    lib.WritePart1("%d", len(pos1))
    lib.WritePart2("%d", len(pos9))
}

func main() {
    day09()
}
