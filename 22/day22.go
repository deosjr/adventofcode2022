package main

import (
    "strings"

	"github.com/deosjr/adventofcode2022/lib"
)

type coord struct {
    x, y int
}

func (c coord) add(d coord) coord {
    return coord{x:c.x+d.x, y:c.y+d.y}
}

func (c coord) left() coord {
    return coord{x:c.y, y:-c.x}
}

func (c coord) right() coord {
    return coord{x:-c.y, y:c.x}
}

func day22() {
    input := lib.ReadFile(22)
    split := strings.Split(input, "\n\n")
    bs, ps := strings.Split(split[0], "\n"), split[1]
    board := map[coord]bool{}
    var pos coord
    dir := coord{1, 0}
    for y:=0; y<len(bs); y++ {
        for x:=0; x<len(bs[y]); x++ {
            c := bs[y][x]
            if c == ' ' {
                continue
            }
            if pos.x == 0 && pos.y == 0 && c == '.' {
                pos.x = x+1
                pos.y = y+1
            }
            board[coord{x+1,y+1}] = c == '.'
        }
    }
    for ps != "" {
        idx := strings.IndexAny(ps, "LR\n")
        n := int(lib.MustParseInt(ps[:idx]))
        d := rune(ps[idx])
        ps = ps[idx+1:]
        //lib.WritePart1("%v %v", pos, dir)
        for i:=0; i<n; i++ {
            var passable bool
            next := pos
            for {
                next = next.add(dir)
                if v, ok := board[next]; ok {
                    passable = v
                    break
                }
                // TODO: max x and max y times 2 or so
                next.x = (next.x + 999) % 999
                next.y = (next.y + 999) % 999
            }
            if !passable {
                break
            }
            pos = next
        }
        if d == 'L' {
            dir = dir.left()
        }
        if d == 'R' {
            dir = dir.right()
        }
    }
    ans := 1000 * pos.y + 4 * pos.x
    switch dir {
    case coord{1,0}: ans += 0
    case coord{0,1}: ans += 1
    case coord{-1,0}: ans += 2
    case coord{0,-1}: ans += 3
    }
    lib.WritePart1("%v", ans)
}

func main() {
    //lib.Test()
    day22()
}
