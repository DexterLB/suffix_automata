package suffix_automata

import (
	"bytes"
	"fmt"
)

type State struct {
	index              int
	len                int
	transitions        []int
	nonZeroTransitions int
}

func NewState(alphabetSize int, stateIndex int, len int) *State {
	transitions := make([]int, alphabetSize)

	for i, _ := range transitions {
		transitions[i] = -1
	}

	return &State{
		index:              stateIndex,
		len:                len,
		transitions:        transitions,
		nonZeroTransitions: 0,
	}
}

func (s *State) AddTransition(letterIndex int, destinationIndex int) {
	s.transitions[letterIndex] = destinationIndex
	s.nonZeroTransitions += 1
}

func (s *State) String() string {
	var buffer bytes.Buffer

	if s.index == 0 {
		buffer.WriteString("ε(0)\n")
	} else {
		buffer.WriteString(fmt.Sprintf("%d(%d)\n", s.index, s.len))
	}

	for i, destination := range s.transitions {
		if destination != -1 {
			buffer.WriteString(PrintTransition(destination, rune(i+'a')))
		}
	}

	return buffer.String()
}

func PrintTransition(destination int, letter rune) string {
	if destination == 0 {
		return fmt.Sprintf("\t---%c----> ε\n", letter)
	}
	return fmt.Sprintf("\t---%c----> %d\n", letter, destination)
}
