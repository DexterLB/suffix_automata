package suffix_automata

import (
	"bytes"
	"fmt"
)

type Transition struct {
	destinationIndex int32
	letter           byte
}

type State struct {
	index       int32
	len         int32
	transitions []Transition
}

func (s *State) get(letter byte) (int32, int32) {
	for i, transition := range s.transitions {
		if transition.letter == letter {
			return int32(i), transition.destinationIndex
		}
	}
	return -1, -1
}

func NewState(stateIndex int32, len int32) *State {
	return &State{
		index:       stateIndex,
		len:         len,
		transitions: nil,
	}
}

func (s *State) AddTransition(letter byte, destinationIndex int32) {
	s.transitions = append(s.transitions, Transition{letter: letter, destinationIndex: destinationIndex})
}

func (s *State) String() string {
	var buffer bytes.Buffer

	if s.index == 0 {
		buffer.WriteString("ε(0)\n")
	} else {
		buffer.WriteString(fmt.Sprintf("%d(%d)\n", s.index, s.len))
	}

	for _, transition := range s.transitions {
		buffer.WriteString(fmt.Sprintf("%s\n", transition))
	}

	return buffer.String()
}

func (t *Transition) String() string {
	if t.destinationIndex == 0 {
		return fmt.Sprintf("\t---%c----> ε\n", t.letter)
	}
	return fmt.Sprintf("\t---%c----> %d\n", rune(t.letter), t.destinationIndex)
}
