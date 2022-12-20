package main

import (
    "fmt"
	"github.com/deosjr/adventofcode2022/lib"
)

const key int64 = 811589153

type ring struct {
    size int
    head *cell
}

type cell struct {
    num   int64
    idx   int
    left  *cell
    right *cell
}

func newRing(nums []int64) *ring {
    head := &cell{num:nums[0], idx:0}
    prev := head
    for i, n := range nums[1:] {
        next := &cell{num:n, idx:i+1, left:prev}
        prev.right = next
        prev = next
    }
    head.left = prev
    prev.right = head
    return &ring{size: len(nums), head: head}
}

func (r *ring) String() string {
    list := make([]int64, r.size)
    c := r.head
    for i:=0; i<r.size; i++ {
        list[i] = c.num
        c = c.right
    }
    return fmt.Sprintf("%v", list)
}

func (r *ring) findByValue(n int64) (*cell, bool) {
    // NOTE: ring can contain duplicates! returns first match
    c := r.head
    for i:=0; i<r.size; i++ {
        if c.num == n {
            return c, true
        }
        c = c.left
    }
    return nil, false
}

func (r *ring) findByIndex(idx int) (*cell, bool) {
    // NOTE: indices are guaranteed unique and start at 0
    c := r.head
    for i:=0; i<r.size; i++ {
        if c.idx == idx {
            return c, true
        }
        c = c.left
    }
    return nil, false
}

func (r *ring) mix() {
    for i:=0; i<r.size; i++ {
        c, _ := r.findByIndex(i)
        r.move(c)
    }
}

func (r *ring) move(c *cell) {
    cn := c.num
    if cn < 0 {
        cn += 10*key*int64(r.size-1)
    }
    n := int(cn % int64(r.size-1))
    if n == 0 {
        return
    }
    next := c.right
    next.left = c.left
    c.left.right = next
    if c == r.head {
        r.head = next
    }
    for i:=0; i<n; i++ {
        next = next.right
    }
    c.left = next.left
    c.right = next
    next.left.right = c
    next.left = c
}

func (r *ring) lookahead(c *cell, n int) *cell {
    n = n % r.size
    for i:=0; i<n; i++ {
        c = c.right
    }
    return c
}

func day20() {
    input := []int64{}
    lib.ReadFileByLine(20, func(line string) {
        n := lib.MustParseInt(line)
        input = append(input, n)
    })
    r := newRing(input)
    zeroCell, _ := r.findByValue(0)
    r.mix()

    n0 := r.lookahead(zeroCell, 1000).num
    n1 := r.lookahead(zeroCell, 2000).num
    n2 := r.lookahead(zeroCell, 3000).num

    lib.WritePart1("%v", n0+n1+n2)

    for i, n := range input {
        input[i] = n*key
    }
    r2 := newRing(input)
    zeroCell, _ = r2.findByValue(0)
    for i:=0; i<10; i++ {
        r2.mix()
    }

    n0 = r2.lookahead(zeroCell, 1000).num
    n1 = r2.lookahead(zeroCell, 2000).num
    n2 = r2.lookahead(zeroCell, 3000).num

    lib.WritePart2("%v", n0+n1+n2)
}

func main() {
    //lib.Test()
    day20()
}
