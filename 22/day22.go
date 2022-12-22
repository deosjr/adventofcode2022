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

//                 51-100 x 1-50,   101-150 x 1-50
//                 51-100 x 51-100
// 1-50 x 101-150, 51-100 x 101-150
// 1-50 x 151-200
func wrap(p coord, d coord) (coord, coord) {
    x, y := p.x, p.y
    switch d {
    case coord{1,0}:  // RIGHT
        if x == 150 {
            return coord{100, 151-y}, d.left().left()
        }
        if x == 100 && y > 50 && y < 101 {
            return coord{y+50, 50}, d.left()
        }
        if x == 100 && y > 100 {
            return coord{150, 151-y}, d.right().right()
        }
        if x == 50 && y > 150 {
            return coord{y-100, 150}, d.left()
        }
    case coord{0,1}:  // DOWN
        if y == 200 {
            return coord{151-x, 1}, d
        }
        if y == 150 && x > 50 {
            return coord{50, x+100}, d.right()
        }
        if y == 50 && x > 100 {
            return coord{100, x-50}, d.right()
        }
    case coord{-1,0}: // LEFT
        if x == 1 && y < 151 {
            return coord{51, 151-y}, d.left().left()
        }
        if x == 1 && y > 150 {
            return coord{y-100, 1}, d.left()
        }
        if x == 51 && y < 51 {
            return coord{1, 151-y}, d.right().right()
        }
        if x == 51 && y > 50 {
            return coord{y-50, 101}, d.left()
        }
    case coord{0,-1}: // UP
        if y == 1 && x < 101 {
            return coord{1, x+100}, d.right()
        }
        if y == 1 && x > 100 {
            return coord{151-x, 200}, d
        }
        if y == 101 && x < 51 {
            return coord{51, x+50}, d.right()
        }
    }
    return p.add(d), d
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
            next, nextdir := wrap(pos, dir)
            passable := board[next]
            /* PART 1
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
            */
            if !passable {
                break
            }
            pos = next
            dir = nextdir
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
