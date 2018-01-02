package suffix_automata

import (
	"bytes"
	"fmt"
)

type State struct {
	index       int
	transitions []Transition
}

func NewState(alphabetSize int, stateIndex int) *State {
	transitions := make([]Transition, alphabetSize)

	for i, _ := range transitions {
		transitions[i].destinationIndex = -1
	}

	return &State{
		index:       stateIndex,
		transitions: transitions,
	}
}

func (s *State) AddTransition(letter rune, destinationIndex int, primary bool) {
	runeIndex := letter - 'a'
	s.transitions[runeIndex].destinationIndex = destinationIndex
	s.transitions[runeIndex].primary = primary
}

func (s *State) Print(states []string) string {
	var buffer bytes.Buffer

	stateName := states[s.index]
	if s.index == 0 {
		buffer.WriteString("ε\n")
	} else {
		buffer.WriteString(fmt.Sprintf("%s\n", stateName))
	}

	for i, tr := range s.transitions {
		if tr.destinationIndex != -1 {
			buffer.WriteString(tr.Print(rune(i+'a'), states))
		}
	}

	return buffer.String()
}

type Transition struct {
	destinationIndex int
	primary          bool
}

func (t *Transition) Print(letter rune, states []string) string {
	destinationName := states[t.destinationIndex]
	if t.destinationIndex == 0 {
		destinationName = "ε"
	}
	return fmt.Sprintf("---%c----> %s (%t)\n", letter, destinationName, t.primary)
}
