package main

import (
	"fmt"
	"io/ioutil"
	"os"

	"github.com/DexterLB/suffix_automata/suffix_automata"
)

func main() {
	word, _ := ioutil.ReadAll(os.Stdin)

	d := suffix_automata.NewDawg(len(word))
	for _, b := range word {
		d.ProcessCharacter(b)
	}

	states, transitions, finals := d.Count()
	fmt.Printf("States: %d\nTransitions: %d\nFinal states: %d\n", states, transitions, finals)
}
