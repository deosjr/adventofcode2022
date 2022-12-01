package lib

import (
	"log"
	"os"
	"runtime"
	"runtime/pprof"
)

func Profiled(f func()) {
	cpuprof, err := os.Create("cpu.prof")
	if err != nil {
		log.Fatal("could not create CPU profile: ", err)
	}
	defer cpuprof.Close() // error handling omitted for example
	// comment in for trivial programs, like day01, otherwise cpu.prof is empty
	runtime.SetCPUProfileRate(5000) // >500 is not recommended, but this program is trivial
	if err := pprof.StartCPUProfile(cpuprof); err != nil {
		log.Fatal("could not start CPU profile: ", err)
	}
	defer pprof.StopCPUProfile()

	f()

	memprof, err := os.Create("mem.prof")
	if err != nil {
		log.Fatal("could not create memory profile: ", err)
	}
	defer memprof.Close() // error handling omitted for example
	runtime.GC()          // get up-to-date statistics
	if err := pprof.WriteHeapProfile(memprof); err != nil {
		log.Fatal("could not write memory profile: ", err)
	}
}
