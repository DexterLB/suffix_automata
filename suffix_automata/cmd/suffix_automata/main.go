package main

import (
	"fmt"
	"io/ioutil"
	"os"

	"github.com/DexterLB/suffix_automata/suffix_automata"
)

func main() {
	// memf, err := os.Create("mem.heap")
	// if err != nil {
	// 	log.Fatal(err)
	// }
	// defer memf.Close()

	// f, err := os.Create("godo.prof")
	// if err != nil {
	// 	log.Fatal(err)
	// }
	// pprof.StartCPUProfile(f)
	// defer pprof.StopCPUProfile()

	word, _ := ioutil.ReadAll(os.Stdin)

	d := suffix_automata.NewDawg(len(word))
	for _, b := range word {
		d.ProcessCharacter(b)
	}
	// pprof.WriteHeapProfile(memf)

	states, transitions, finals := d.Count()
	fmt.Printf("States: %d\nTransitions: %d\nFinal states: %d\n", states, transitions, finals)
}
