package main

import (
    "fmt"

	"github.com/deosjr/adventofcode2022/lib"
)

type state struct {
    cycle int
    x int
    s string
}

func (st *state) cyc(n int) {
    c40 := st.cycle % 40
    if c40 == st.x-1 || c40 == st.x || c40 == st.x+1 {
        st.s += "#"
    } else {
        st.s += "."
    }
    cplus1 := st.cycle + 1
    if cplus1 % 40 == 0 {
        p2 = append(p2, st.s)
        st.s = ""
    }
    if cplus1 % 40 == 20 {
        p1 += st.x * cplus1
    }
    st.x += n
    st.cycle += 1
}

var p1 int
var p2 = []string{}

func day10() {
    st := &state{ cycle: 0, x: 1, s: "" }
    lib.ReadFileByLine(10, func(line string) {
        var instr string
        var n int
        fmt.Sscanf(line, "%s %d", &instr, &n)
        switch instr {
        case "noop":
            st.cyc(0)
        case "addx":
            st.cyc(0)
            st.cyc(n)
        }
    })
    lib.WritePart1("%d", p1)
    lib.WritePart2("")
    for _, s := range p2 {
        fmt.Println(s)
    }
}

func main() {
    day10()
}
