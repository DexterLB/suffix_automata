#![feature(vec_resize_default)]
#![feature(alloc_system)]

extern crate alloc_system;

use std::io;
use std::io::Read;
use std::process;

extern crate cpuprofiler;
use cpuprofiler::PROFILER;


struct Blumer {
    states: Vec<State>,
    word_state: i32
}

impl Default for Blumer {
    fn default() -> Blumer {
        Blumer {
            states: vec![State::default()],     // epsilon state
            word_state: 0
        }
    }
}

const EPSILON_STATE: i32 = 0;

impl Blumer {
    fn process_chars<I>(&mut self, chars: I)
        where
            I: Iterator<Item = u8>
    {
        for c in chars {
            self.process_char(c);
        }
    }

    fn process_char(&mut self, c: u8) {
        let (s, word_state, target) = self.find_slink(c);
        self.word_state = word_state;

        if s == -1 {
            self.mutstate(word_state).slink = EPSILON_STATE;
        } else {
            if self.mutstate(s).len + 1 == self.state(target).len { // primary edge
                self.mutstate(word_state).slink = target;
            } else {
                let s_new = self.add_slink(s, target);
                self.copy_transitions(target, s_new);
                self.redirect_transitions(s, c, target, s_new)
            }
        }
    }

    fn add_slink(&mut self, s: i32, target: i32) -> i32 {
        let new_len: i32 = self.state(s).len + 1;   // remove

        let s_new = self.add_state(new_len);
        self.mutstate(s_new).slink = self.state(target).slink;
        self.mutstate(target).slink = s_new;

        let word_state: i32 = self.word_state;      // remove
        self.mutstate(word_state).slink = s_new;

        s_new
    }

    fn copy_transitions(&mut self, from: i32, to: i32) {
        self.mutstate(to).transitions = self.state(from).transitions.clone()
        // maybe a loop would be more performant?
    }

    fn redirect_transitions(&mut self, from_state: i32, c: u8, target: i32, word_state: i32) {
        let mut from_state = from_state;

        loop {
            match self.state(from_state).transition_to(c, target) {
                Some(neighbour_transition_index) => {
                    self.mutstate(from_state)
                        .transitions[neighbour_transition_index].1
                            = word_state;

                    if self.mutstate(from_state).slink == -1 {
                        return;
                    }

                    from_state = self.state(from_state).slink;
                },

                None => return
            }
        }
    }

    fn find_slink(&mut self, c: u8) -> (i32, i32, i32) {
        let mut s = self.word_state;

        let new_len: i32 = self.state(s).len + 1;       // remove
        let new_word_state = self.add_state(new_len);

        loop {
            match self.state(s).transition_index(c) {
                Some(index) =>          // There is a transition with char c
                    return (s, new_word_state, self.state(s).transitions[index].1),

                None => {               // There is no transition with char c
                    self.mutstate(s).add_transition(c, new_word_state);
                    if self.state(s).slink == -1 {
                        return (-1, new_word_state, new_word_state);
                    } else {
                        s = self.state(s).slink;
                    }
                }
            }
        }
    }

    fn add_state(&mut self, len: i32) -> i32 {
        let state = self.states.len();
        self.states.resize_default(state + 1);
        self.states[state].len = len;   // todo: make this faster

        state as i32
    }

    fn state(&self, index: i32) -> &State {
        unsafe {
            self.states.get_unchecked(index as usize)
        }
    }

    fn mutstate(&mut self, index: i32) -> &mut State {
        unsafe {
            self.states.get_unchecked_mut(index as usize)
        }
    }
}

#[derive(Clone)]
#[repr(packed)]
struct Transition(u8, i32);

#[repr(packed)]
struct State {
    len:    i32,
    slink:  i32,
    transitions: Vec<Transition> // todo: split this in two
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
    // fn transition(&self, with: u8) -> Option<i32> {
    //     self.transitions.iter().find(|&&(c, _)| c == with).map(|&(_, s)| s)
    // }

    fn transition_index(&self, with: u8) -> Option<usize> {
        self.transitions.iter().position(|ref t| t.0 == with)
    }

    fn transition_to(&self, with: u8, to: i32) -> Option<usize> {
        self.transitions.iter().position(|ref t| t.0 == with && t.1 == to)
    }

    // fn set_transition(&mut self, with: u8, to: i32) {
    //     match self.transition_index(with) {
    //         Some(index) => self.set_transition_at(index, to),
    //         None        => self.add_transition(with, to)
    //     }
    // }

    fn add_transition(&mut self, with: u8, to: i32) {
        self.transitions.push(Transition(with, to))
    }

    // fn set_transition_at(&mut self, at: usize, to: i32) {
    //     self.transitions[at].1 = to
    // }
}


fn main() {
    let mut buf = vec![0; 10];

    match io::stdin().read_to_end(&mut buf) {
        Ok(_) => (),
        Err(e) => {
            println!("error reading data: {:?}", e);
            return;
        }
    }

    let mut b = Blumer::default();

    PROFILER.lock().unwrap().start("./blumer.profile").expect("Couldn't start");
    b.process_chars(buf.into_iter().filter(|&c| c <= 'z' as u8 && c >= 'a' as u8));
    PROFILER.lock().unwrap().stop().expect("Couldn't stop");

    let transitions: usize = b.states.iter().map(|s| s.transitions.len()).sum();

    println!("{:?} {:?}", b.states.len(), transitions);

    process::exit(0);
}
