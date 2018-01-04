package main

import (
	"fmt"
	"io/ioutil"
	"os"

	"github.com/DexterLB/suffix_automata/suffix_automata"
)

func main() {
	// word := "abcbc"
	// bytestring := []byte(word)

	// d.ProcessCharacter(byte('b'))
	// fmt.Printf("%s\n========\n", d)
	// d.Dotify()

	word, _ := ioutil.ReadAll(os.Stdin)

	d := suffix_automata.NewDawg()
	for _, b := range word {
		d.ProcessCharacter(b)
	}

	states, transitions, finals := d.Count()
	fmt.Printf("States: %d\nTransitions: %d\nFinal states: %d\n", states, transitions, finals)
}
