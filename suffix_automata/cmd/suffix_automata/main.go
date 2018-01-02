package main

import (
	"fmt"

	"github.com/DexterLB/suffix_automata/suffix_automata"
)

func main() {
	alphabetSize := 3

	d := suffix_automata.NewDawg(alphabetSize)
	fmt.Printf("%s\n========\n", d)

	d.AddState("a")
	fmt.Printf("%s\n========\n", d)

	d.AddState("b")
	fmt.Printf("%s\n========\n", d)

	// states := []string{"", "a", "b", "c", "ab"}

	// e := suffix_automata.NewState(alphabetSize, 0)

	// a := suffix_automata.NewState(alphabetSize, 1)
	// fmt.Sprintf("%s\n%s\n===============\n", e.Print(states), a.Print(states))

	// e.AddTransition('a', 1, true)
	// fmt.Printf(e.Print(states))

	// a.AddTransition('b', 4, true)
	// fmt.Printf(a.Print(states))

}
