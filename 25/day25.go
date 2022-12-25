package main

import (
    "math"
    "strconv"

	"github.com/deosjr/adventofcode2022/lib"
)

func dec2snafu(n int) string {
    base5 := strconv.FormatInt(int64(n), 5)
    return base5snafu(base5)
}

func base5snafu(base5 string) string {
    base5min2 := ""
    var rem bool
    for i:=len(base5)-1; i>-1; i-- {
        r, _ := strconv.ParseInt(base5[i:i+1], 5, 64)
        if rem {
            r++
        }
        rem = false
        if r == 0 || r == 1 || r == 2 {
            base5min2 += strconv.FormatInt(r, 5)
            continue
        }
        rem = true
        if r == 3 {
            base5min2 += "="
            continue
        }
        if r == 4 {
            base5min2 += "-"
            continue
        }
        // r == 5
        base5min2 += "0"
    }
    if rem {
        base5min2 += "1"
    }
    rev := ""
    for i:=len(base5min2)-1; i>-1; i-- {
        rev += base5min2[i:i+1]
    }
    return rev
}

func snafu2dec(snafu string) int {
    sum := 0
    for i:=len(snafu)-1; i>-1; i-- {
        exp := float64(len(snafu)-1-i)
        switch snafu[i] {
        case '=':
            sum -= 2 * int(math.Pow(5, exp))
        case '-':
            sum -= int(math.Pow(5, exp))
        case '0':
            continue
        case '1':
            sum += int(math.Pow(5, exp))
        case '2':
            sum += 2 * int(math.Pow(5, exp))
        }
    }
    return sum
}

func day25() {
    fuel := 0
    lib.ReadFileByLine(25, func(line string) {
        fuel += snafu2dec(line)
    })
    lib.WritePart1(dec2snafu(fuel))
}

func main() {
    //lib.Test()
    day25()
}
