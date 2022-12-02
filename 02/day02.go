package main

import (
	"github.com/deosjr/adventofcode2022/lib"
)

type shape uint8

const (
    rock shape = iota
    paper
    scissors
)

func readShape(c byte) shape {
    if c < 68 {
        return shape(c - 65)
    }
    return shape(c - 88)
}

func day02() {
    input := []shape{}
    lib.ReadFileByLine(2, func(line string) {
        x := readShape(line[0])
        y := readShape(line[2])
        input = append(input, x, y)
    })
    lib.WritePart1("%d", game1(input))
    lib.WritePart2("%d", game2(input))
}

func game1(rounds []shape) int {
    if len(rounds) == 0 {
        return 0
    }
    opp, you := rounds[0], rounds[1]
    return score(you, opp) + game1(rounds[2:])
}

func game2(rounds []shape) int {
    if len(rounds) == 0 {
        return 0
    }
    opp, outcome := rounds[0], rounds[1]
    var you shape
    switch outcome {
    case rock: // lose
        you = (opp + 2) % 3
    case paper: // draw
        you = opp
    case scissors: // win
        you = (opp + 1) % 3
    }
    return score(you, opp) + game2(rounds[2:])
}

func score(you, opp shape) int {
    score := int(you) + 1
    if opp == you {
        score += 3
    } else if (you + 2) % 3 == opp {
        score += 6
    }
    return score
}

func main() {
    day02()
}
