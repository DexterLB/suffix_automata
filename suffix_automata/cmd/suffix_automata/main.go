package main

import (
	"fmt"

	"github.com/DexterLB/suffix_automata/suffix_automata"
)

func main() {
	alphabetSize := 3

	word := "abcbc"

	d := suffix_automata.NewDawg(alphabetSize)
	for i, _ := range word {
		d.ProcessCharacter(int(word[i] - 'a'))
	}

	d.ProcessCharacter('b' - 'a')
	fmt.Printf("%s\n========\n", d)
	d.Dotify()

	states, transitions := d.Count()

	fmt.Printf("States: %d\nTransitions: %d\n", states, transitions)

}
