package suffix_automata

import "bytes"

type Dawg struct {
	alphabetSize int
	stateNames   []string

	states []*State
}

func NewDawg(alphabetSize int) *Dawg {
	stateNames := []string{""}
	states := []*State{NewState(alphabetSize, 0)}

	return &Dawg{
		alphabetSize: alphabetSize,
		stateNames:   stateNames,
		states:       states,
	}
}

func (d *Dawg) AddState(name string) {
	d.stateNames = append(d.stateNames, name)
	newState := NewState(d.alphabetSize, len(d.stateNames)-1)

	d.states = append(d.states, newState)
}

func (d *Dawg) String() string {
	var buffer bytes.Buffer

	for i, _ := range d.states {
		buffer.WriteString(d.states[i].Print(d.stateNames))
	}

	return buffer.String()
}
