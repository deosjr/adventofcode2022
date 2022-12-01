package main

import (
	"sync"

	"github.com/deosjr/adventofcode2022/lib"
)

// for educational purposes only

// should improve in multiple ways:
// - can take larger inputs using int64
// - no longer needs to calculate an array of all calories
func day01_linebyline() {
	var top1, top2, top3 int64
	var total int64
	lib.ReadFileByLine(1, func(line string) {
		if line == "" {
			top1, top2, top3 = add(top1, top2, top3, total)
			total = 0
			return
		}
		total += lib.MustParseInt(line)
	})
	lib.WritePart1("%d", top1)
	lib.WritePart2("%d", top1+top2+top3)
}

// over the top function to get rid of sorts and keeping a total array
func add(top1, top2, top3, n int64) (int64, int64, int64) {
	switch {
	case top1 == 0:
		return n, 0, 0
	case top2 == 0:
		if top1 < n {
			return n, top1, 0
		}
		return top1, n, 0
	case top3 == 0:
		if top1 < n {
			return n, top1, top2
		} else if top2 < n {
			return top1, n, top2
		}
		return top1, top2, n
	}
	if top1 < n {
		return n, top1, top2
	} else if top2 < n {
		return top1, n, top2
	} else if top3 < n {
		return top1, top2, n
	}
	return top1, top2, top3
}

// playing with goroutines: completely unnecessary for such a small example!
// but we can generate a way bigger input :)
// note this is not a complete mapreduce but you get the idea
func day01_mapreduce() {
	// set up the workers
	in := make(chan []string)
	out := make(chan int64)
	defer close(out)
	var wg sync.WaitGroup

	worker := func() {
		defer wg.Done()
		for calories := range in {
			var total int64
			for _, n := range calories {
				total += lib.MustParseInt(n)
			}
			out <- total
		}
	}
	numWorkers := 10
	wg.Add(numWorkers)
	for i := 0; i < numWorkers; i++ {
		go worker()
	}

	// start reading output _before_ sending work
	var top1, top2, top3 int64
	go func() {
		for n := range out {
			top1, top2, top3 = add(top1, top2, top3, n)
		}
	}()

	// input reader
	var cals []string
	lib.ReadFileByLine(1, func(line string) {
		if line == "" {
			in <- cals
			cals = nil
			return
		}
		cals = append(cals, line)
	})

	close(in)
	wg.Wait()
	lib.WritePart1("%d", top1)
	lib.WritePart2("%d", top1+top2+top3)
}
