#![feature(vec_resize_default)]

use std::str::Chars;

struct Blumer {
    states: Vec<State>,
    word_state: i32
}

impl Blumer {
    fn process_chars(&mut self, chars: Chars) {
        for c in chars {
            self.process_char(c);
        }
    }

    fn process_char(&mut self, c: char) {

    }

    fn find_slink(&mut self, c: char) {
        self.word_state = self.add_state()
    }

    fn add_state(&mut self) -> i32 {
        let state = self.states.len();
        self.states.resize_default(state + 1);

        state as i32
    }
}

struct State {
    len:    i32,
    slink:  i32,
    transitions: Vec<(char, i32)>
}

impl Default for State {
    fn default() -> State {
        State {
            len: 0,
            slink: -1,
            transitions: Vec::new()
        }
    }
}

impl State {
    fn transition(&self, with: char) -> Option<i32> {
        self.transitions.iter().find(|&&(c, _)| c == with).map(|&(_, s)| s)
    }

    fn transition_index(&self, with: char) -> Option<usize> {
        self.transitions.iter().position(|&(c, _)| c == with)
    }

    fn set_transition(&mut self, with: char, to: i32) {
        match self.transition_index(with) {
            Some(index) => self.set_transition_at(index, to),
            None        => self.add_transition(with, to)
        }
    }

    fn add_transition(&mut self, with: char, to: i32) {
        self.transitions.push((with, to))
    }

    fn set_transition_at(&mut self, at: usize, to: i32) {
        self.transitions[at].1 = to
    }
}


fn main() {
    println!("Hello, world!");
}
