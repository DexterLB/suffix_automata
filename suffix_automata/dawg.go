package suffix_automata

import (
	"bytes"
	"fmt"
	"os"
)

type Dawg struct {
	qW int32

	transitionsNum int32
	states         []State
	slinks         []int32
}

func NewDawg() *Dawg {
	states := []State{*NewState(0, 0)}
	slinks := []int32{-1}

	return &Dawg{
		qW:             0,
		transitionsNum: 0,
		states:         states,
		slinks:         slinks,
	}
}

func (d *Dawg) Count() (int32, int32, int32) {
	s := d.qW
	var finalSum int32
	for s != -1 {
		finalSum += 1
		s = d.slinks[s]
	}

	return int32(len(d.states)), d.transitionsNum, finalSum
}

func (d *Dawg) AddState(stateLen int32) int32 {
	d.states = append(d.states, State{len: stateLen})
	d.slinks = append(d.slinks, -1)

	return int32(len(d.states)) - 1
}

// <s, qwa>
func (d *Dawg) FindSLink(letter byte) (int32, int32) {
	qWa := d.AddState(d.states[d.qW].len + 1)
	state := d.qW

	_, dest := d.states[state].get(letter)
	for dest == -1 {
		d.states[state].AddTransition(letter, qWa)
		d.transitionsNum += 1

		if d.slinks[state] == -1 {
			return -1, qWa
		} else {
			state = d.slinks[state]
		}
		_, dest = d.states[state].get(letter)
	}

	return state, qWa
}

func (d *Dawg) ProcessCharacter(letter byte) {
	s, qWa := d.FindSLink(letter)
	d.qW = qWa

	if s == -1 {
		d.slinks[d.qW] = 0
		return
	}

	_, destination := d.states[s].get(letter)
	if d.states[destination].len == d.states[s].len+1 {
		d.slinks[d.qW] = destination
		return
	}

	sNew := d.AddSlink(s, letter)
	d.CopyTransitions(sNew, destination)
	d.RedirectTransitions(s, letter, destination, sNew)
}

func (d *Dawg) CopyTransitions(sNew int32, destination int32) {
	sNewState := &d.states[sNew]

	for _, transition := range d.states[destination].transitions {
		sNewState.AddTransition(transition.letter, transition.destinationIndex)
		d.transitionsNum += 1
	}
}

func (d *Dawg) RedirectTransitions(s int32, letter byte, t int32, sNew int32) {
	ind, destination := d.states[s].get(letter)
	for destination == t {
		d.states[s].transitions[ind] = Transition{letter: letter, destinationIndex: sNew}
		s = d.slinks[s]
		if s == -1 {
			break
		}
		ind, destination = d.states[s].get(letter)
	}
}

func (d *Dawg) AddSlink(stateIndex int32, letter byte) int32 {
	state := &d.states[stateIndex]
	_, destinationIndex := state.get(letter)

	newStateIndex := d.AddState(state.len + 1)

	d.slinks[newStateIndex] = d.slinks[destinationIndex]
	d.slinks[destinationIndex] = newStateIndex
	d.slinks[d.qW] = newStateIndex

	return newStateIndex
}

func (d *Dawg) DotifyState(ind int32) string {
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
		for _, tr := range d.states[i].transitions {
			if tr.destinationIndex != -1 {
				if d.states[tr.destinationIndex].len-d.states[i].len > 1 {
					fmt.Fprintf(f, "%s -> %s [label=\"%c\",style=dotted];\n", d.DotifyState(int32(i)), d.DotifyState(tr.destinationIndex), rune(tr.letter))
				} else {
					fmt.Fprintf(f, "%s -> %s [label=\"%c\"];\n", d.DotifyState(int32(i)), d.DotifyState(tr.destinationIndex), rune(tr.letter))
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
