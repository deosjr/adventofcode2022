package main

import (
    "fmt"
    "math/rand"
    "os"
    "testing"

	"github.com/deosjr/adventofcode2022/lib"
)

// commands for building with buildinfo, and disassembly
// go build -gcflags -S 01/day01.go 01/day01_alternatives.go
// go tool objdump day01

// go test ./... -bench=.

// not shown: tracing/tuning garbage collection

func init() {
    generateLargeInput()
    lib.NoOutput()
}

func Benchmark01Basic(b *testing.B) {
    for n := 0; n < b.N; n++ {
        day01()
    }
}

func Benchmark01LineByLine(b *testing.B) {
    for n := 0; n < b.N; n++ {
        day01_linebyline()
    }
}

func Benchmark01Concurrent(b *testing.B) {
    for n := 0; n < b.N; n++ {
        day01_mapreduce()
    }
}

// populate the "test" file with input large enough to make the above interesting
// NOTE: assumes run from test, which looks for "test" file locally in /01 folder
func generateLargeInput() {
    f, err := os.Create("test")
    if err != nil {
        panic(err)
    }
    defer f.Close()
    for i:=1; i<1000000; i++ {
        if i%1000 == 0 {
            f.WriteString("\n")
        }
        f.WriteString(fmt.Sprintf("%d\n", rand.Intn(99999999)+1))
    }
}

