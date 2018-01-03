package main

import (
	"fmt"

	"github.com/DexterLB/suffix_automata/suffix_automata"
)

func main() {
	word := "abcbc"
	bytestring := []byte(word)

	d := suffix_automata.NewDawg()
	for _, b := range bytestring {
		d.ProcessCharacter(b)
	}

	d.ProcessCharacter(byte('b'))
	fmt.Printf("%s\n========\n", d)
	d.Dotify()

	states, transitions, finals := d.Count()

	fmt.Printf("States: %d\nTransitions: %d\nFinal states: %d\n", states, transitions, finals)

}
