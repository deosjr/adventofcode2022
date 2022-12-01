package lib

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strconv"
    "strings"
)

// if set to true readfile will look for filename "test" instead of dayX.input
var test bool

func Test() {
    test = true
}

var w io.Writer = os.Stdout

func NoOutput() {
    w = io.Discard
}

func fileName(day int) string {
    dir := fmt.Sprintf("%02d/", day)
    wd, err := os.Getwd()
    if err != nil {
        panic(err)
    }
    split := strings.Split(wd, "/")
    wd = split[len(split)-1] + "/"
    if wd == dir {
        dir = ""
    }
    if test {
        return fmt.Sprintf("%stest", dir)
    }
    return fmt.Sprintf("%sday%02d.input", dir, day)
}

// ReadFile returns the entire file as one big string
func ReadFile(day int) string {
    bytes, err := os.ReadFile(fileName(day))
    if err != nil {
        panic(err)
    }
    return string(bytes)
}

// ReadFileByLine takes a function fn, which updates a data structure
// for each line in the input file. fn has to typecheck datastruct
func ReadFileByLine(day int, fn func(string)) {
    f, err := os.Open(fileName(day))
    if err != nil {
        panic(err)
    }
    scanner := bufio.NewScanner(f)
    for scanner.Scan() {
        fn(scanner.Text())
    }
}

func MustParseInt(str string) int64 {
	n, err := strconv.ParseInt(str, 10, 64)
	if err != nil {
		panic(err)
	}
	return n
}

func WritePart1(format string, values ...interface{}) {
	answer := "Part 1: " + format + "\n"
	fmt.Fprintf(w, answer, values...)
}

func WritePart2(format string, values ...interface{}) {
	answer := "Part 2: " + format + "\n"
	fmt.Fprintf(w, answer, values...)
}
