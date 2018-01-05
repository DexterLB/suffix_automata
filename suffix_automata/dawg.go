package suffix_automata

type Dawg struct {
	qW int32

	lastTransition int32
	states         []State
	slinks         []int32
	transitions    []Transition
}

func NewDawg(len int) *Dawg {
	states := make([]State, 1, 2*len-1)
	states[0].len = 0
	states[0].lastTransition = -1
	slinks := make([]int32, 1, 2*len-1)
	slinks[0] = -1

	transitions := make([]Transition, 0, 3*len-4)

	return &Dawg{
		qW:          0,
		states:      states,
		slinks:      slinks,
		transitions: transitions,
	}
}

func (d *Dawg) get(state int32, letter byte) (int32, int32) {
	i := d.states[state].lastTransition
	for i != -1 {
		if d.transitions[i].letter == letter {
			return int32(i), d.transitions[i].destinationIndex
		}
		i = d.transitions[i].prev
	}
	return -1, -1
}

func (d *Dawg) AddTransition(state int32, letter byte, destinationIndex int32) {
	if d.states[state].lastTransition != -1 {
		d.transitions = append(d.transitions, Transition{letter: letter, destinationIndex: destinationIndex, prev: d.states[state].lastTransition})
	} else {
		d.transitions = append(d.transitions, Transition{letter: letter, destinationIndex: destinationIndex, prev: -1})
	}

	d.states[state].lastTransition = int32(len(d.transitions)) - 1
}

func (d *Dawg) Count() (int, int, int) {
	s := d.qW
	finalSum := 0
	for s != -1 {
		finalSum += 1
		s = d.slinks[s]
	}

	return len(d.states), len(d.transitions), finalSum
}

func (d *Dawg) AddState(stateLen int32) int32 {
	d.states = append(d.states, State{len: stateLen, lastTransition: -1})
	d.slinks = append(d.slinks, -1)

	return int32(len(d.states)) - 1
}

// <s, qwa>
func (d *Dawg) FindSLink(letter byte) (int32, int32) {
	qWa := d.AddState(d.states[d.qW].len + 1)
	state := d.qW

	_, dest := d.get(state, letter)
	for dest == -1 {
		d.AddTransition(state, letter, qWa)

		if d.slinks[state] == -1 {
			return -1, qWa
		} else {
			state = d.slinks[state]
		}
		_, dest = d.get(state, letter)
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

	_, destination := d.get(s, letter)
	if d.states[destination].len == d.states[s].len+1 {
		d.slinks[d.qW] = destination
		return
	}

	sNew := d.AddSlink(s, letter)
	d.CopyTransitions(sNew, destination)
	d.RedirectTransitions(s, letter, destination, sNew)
}

func (d *Dawg) CopyTransitions(sNew int32, destination int32) {
	i := d.states[destination].lastTransition
	for i != -1 {
		d.AddTransition(sNew, d.transitions[i].letter, d.transitions[i].destinationIndex)
		i = d.transitions[i].prev
	}
}

func (d *Dawg) RedirectTransitions(s int32, letter byte, t int32, sNew int32) {
	ind, destination := d.get(s, letter)

	for destination == t {
		d.transitions[ind] = Transition{letter: letter, destinationIndex: sNew, prev: d.transitions[ind].prev}
		s = d.slinks[s]
		if s == -1 {
			break
		}
		ind, destination = d.get(s, letter)
	}
}

func (d *Dawg) AddSlink(stateIndex int32, letter byte) int32 {
	_, destinationIndex := d.get(stateIndex, letter)

	newStateIndex := d.AddState(d.states[stateIndex].len + 1)

	d.slinks[newStateIndex] = d.slinks[destinationIndex]
	d.slinks[destinationIndex] = newStateIndex
	d.slinks[d.qW] = newStateIndex

	return newStateIndex
}
