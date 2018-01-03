package suffix_automata

import (
	"bytes"
	"fmt"
	"os"
)

type Dawg struct {
	alphabetSize int
	qW           int
	states       []*State
	slinks       []int
}

func (d *Dawg) Count() (int, int) {
	trCount := 0
	for i, _ := range d.states {
		trCount += d.states[i].nonZeroTransitions
	}

	return len(d.states), trCount
}

func NewDawg(alphabetSize int) *Dawg {
	states := []*State{NewState(alphabetSize, 0, 0)}
	slinks := []int{-1}

	return &Dawg{
		alphabetSize: alphabetSize,
		qW:           0,
		states:       states,
		slinks:       slinks,
	}
}

func (d *Dawg) AddState(stateLen int) int {
	newState := NewState(d.alphabetSize, len(d.states), stateLen)
	d.states = append(d.states, newState)
	d.slinks = append(d.slinks, -1)

	return newState.index
}

// <s, qwa>
func (d *Dawg) FindSLink(letterIndex int) (int, int) {
	qWa := d.AddState(d.states[d.qW].len + 1)
	state := d.qW

	for d.states[state].transitions[letterIndex] == -1 {
		d.states[state].AddTransition(letterIndex, qWa)
		if d.slinks[state] == -1 {
			return -1, qWa
		} else {
			state = d.slinks[state]
		}
	}

	return state, qWa
}

func (d *Dawg) ProcessCharacter(letterIndex int) {
	s, qWa := d.FindSLink(letterIndex)
	d.qW = qWa

	if s == -1 {
		d.slinks[d.qW] = 0
		return
	}

	destination := d.states[s].transitions[letterIndex]
	if destination == d.states[s].len {
		d.slinks[d.qW] = d.states[s].transitions[letterIndex]
		return
	}

	sNew := d.AddSlink(s, letterIndex)
	d.CopyTransitions(sNew, destination)
	d.RedirectTransitions(s, letterIndex, destination, sNew)
}

func (d *Dawg) CopyTransitions(sNew int, destination int) {
	sNewState := d.states[sNew]

	for i := 0; i < d.alphabetSize; i++ {
		if d.states[destination].transitions[i] != -1 {
			sNewState.AddTransition(i, d.states[destination].transitions[i])
		}
	}
}

func (d *Dawg) RedirectTransitions(s int, letterIndex int, destination int, sNew int) {
	for s != -1 && d.states[s].transitions[letterIndex] == destination {
		d.states[s].transitions[letterIndex] = sNew
		s = d.slinks[s]
	}
}

func (d *Dawg) AddSlink(stateIndex int, letterIndex int) int {
	state := d.states[stateIndex]
	destinationIndex := state.transitions[letterIndex]

	newStateIndex := d.AddState(state.len + 1)

	d.slinks[newStateIndex] = d.slinks[destinationIndex]
	d.slinks[destinationIndex] = newStateIndex
	d.slinks[d.qW] = newStateIndex

	return newStateIndex
}

func (d *Dawg) AddTransaction(stateIndex int, letterIndex int, destinationIndex int) {
	d.states[stateIndex].AddTransition(letterIndex, destinationIndex)
	// fmt.Printf("Adding %d ---- %c ----> %d [%t]\n", stateIndex, letter, destinationIndex, primary)
}

func (d *Dawg) DotifyState(ind int) string {
	if ind == 0 {
		return "ε"
	}
	return fmt.Sprintf("%d", ind)
}

func (d *Dawg) Dotify() {
	f, _ := os.Create("gofoo.dot")
	defer f.Close()

	fmt.Fprintf(f, "digraph g{\n")

	for i, _ := range d.states {
		for j, _ := range d.states[i].transitions {
			destination := d.states[i].transitions[j]
			if destination != -1 {
				if d.states[destination].len-d.states[i].len > 1 {
					fmt.Fprintf(f, "%s -> %s [label=\"%c\",style=dotted];\n", d.DotifyState(i), d.DotifyState(destination), rune(j+'a'))
				} else {
					fmt.Fprintf(f, "%s -> %s [label=\"%c\"];\n", d.DotifyState(i), d.DotifyState(destination), rune(j+'a'))
				}
			}
		}
	}

	fmt.Fprintf(f, "}\n")
}

func (d *Dawg) String() string {
	var buffer bytes.Buffer

	buffer.WriteString(fmt.Sprintf("qW: %d\n", d.qW))

	for i, _ := range d.states {
		buffer.WriteString(fmt.Sprintf("State: %s\n", d.states[i]))
		if d.slinks[i] != -1 {
			buffer.WriteString(fmt.Sprintf("slink: %d\n", d.slinks[i]))
		}
		buffer.WriteString("_____________\n")
	}

	return buffer.String()
}
