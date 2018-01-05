package suffix_automata

type Transition struct {
	destinationIndex int32
	letter           byte
	prev             int32
}

type State struct {
	len            int32
	lastTransition int32
}
