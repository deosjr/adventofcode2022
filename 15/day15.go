package main

import (
    "fmt"
    "math"

	"github.com/deosjr/adventofcode2022/lib"
)

type coord struct {
    x, y int64
}

type sensor struct {
    pos coord
    beacon coord
    distance int64
}

func newSensor(p, b coord) sensor {
    d := manhattan(p,b)
    return sensor{pos:p, beacon:b, distance: d}
}

func manhattan(p, q coord) int64 {
    return int64(math.Abs(float64(p.x-q.x))) + int64(math.Abs(float64(p.y-q.y)))
}

func check(sensors []sensor, c coord) bool {
    for _, s := range sensors {
        if c == s.beacon {
            return false
        }
        if manhattan(s.pos, c) <= s.distance {
            return true
        }
    }
    return false
}

func fringe(s sensor) []coord {
    coords := []coord{}
    c := coord{0, s.distance + 1}
    for i:=0; i<=int(s.distance); i++ {
        c = coord{c.x-1, c.y-1}
        coords = append(coords, coord{s.pos.x+c.x, s.pos.y+c.y})
    }
    for i:=0; i<=int(s.distance); i++ {
        c = coord{c.x-1, c.y+1}
        coords = append(coords, coord{s.pos.x+c.x, s.pos.y+c.y})
    }
    for i:=0; i<=int(s.distance); i++ {
        c = coord{c.x+1, c.y+1}
        coords = append(coords, coord{s.pos.x+c.x, s.pos.y+c.y})
    }
    for i:=0; i<=int(s.distance); i++ {
        c = coord{c.x+1, c.y-1}
        coords = append(coords, coord{s.pos.x+c.x, s.pos.y+c.y})
    }
    return coords
}

func day15() {
    sensors := []sensor{}
    lib.ReadFileByLine(15, func(line string) {
        var sx, sy, bx, by int64
        fmt.Sscanf(line, "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d", &sx, &sy, &bx, &by)
        sensors = append(sensors, newSensor( coord{sx,sy}, coord{bx,by} ))
    })

    coverage := 0
    million := 1000000
    for x:=-10*million;x<20*million;x++ {
        c := coord{int64(x), int64(2*million)}
        if check(sensors, c) {
            coverage++
        }
    }
    lib.WritePart1("%d", coverage)

Loop:
    for _, s := range sensors {
        for _, c := range fringe(s) {
            if c.x < 0 || c.y < 0 || c.x > 4000000 || c.y > 4000000 {
                continue
            }
            if !check(sensors, c) {
                lib.WritePart2("%d", c.x * 4000000 + c.y)
                break Loop
            }
        }
    }
}

func main() {
    day15()
}
