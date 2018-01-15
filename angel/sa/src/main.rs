#![feature(vec_resize_default)]
#![feature(alloc_system)]
#![feature(option_filter)]

extern crate alloc_system;

use std::env;
use std::fs::File;
use std::io::Read;
use std::process;

// extern crate cpuprofiler;
// use cpuprofiler::PROFILER;

const ALPHABET_SIZE: u8 = 26;
const ALPHABET_OFFSET: u8 = 'a' as u8;

struct Blumer {
    states: Vec<State>,
    word_state: i32,
    transitions: Vec<[i32; ALPHABET_SIZE as usize]>
}

impl Default for Blumer {
    fn default() -> Blumer {
        Blumer {
            states: vec![State::default()],     // epsilon state
            word_state: 0,
            transitions: Vec::new()
        }
    }
}

const EPSILON_STATE: i32 = 0;

impl Blumer {
    fn new(size: usize) -> Blumer {
        let mut states = Vec::with_capacity(size * 2);
        states.resize_default(1);

        Blumer {
            states: states,
            word_state: 0,
            transitions: Vec::with_capacity(size / 2)
        }
    }

    fn count_states(&self) -> usize {
        self.states.len()
    }

    fn count_transitions(&self) -> usize {
        self.states.iter().map(|s| self.count_state_transitions(s)).sum()
    }

    fn count_finals(&self) -> usize {
        let mut n: usize = 0;
        let mut state = self.word_state;

        while state != -1 {
            n = n + 1;
            state = self.state(state).slink;
        }

        n
    }

    fn count_state_transitions(&self, state: &State) -> usize {
        if state.transition_with == 0 {
            if state.transition_to == -1 {
                0
            } else {
                unsafe {
                    self.transitions.get_unchecked(state.transition_to as usize)
                    .iter().filter(|&&x| x != -1).count()
                }
            }
        } else {
            1
        }
    }

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
                self.copy_transitions_to_blank_state(target, s_new);
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

    fn redirect_transitions(&mut self, from_state: i32, c: u8, target: i32, word_state: i32) {
        let mut from_state = from_state;

        let mut slink;

        loop {
            slink = self.state(from_state).slink;
            match self.transition_to(from_state, c, target) {
                Some(transition_target) => {
                    *transition_target = word_state;

                    if slink == -1 {
                        return;
                    }

                    from_state = slink;
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
            match self.transition_with(s,c).map(|&mut x| x) {   // drop mutability
                Some(transition_target) =>          // There is a transition with char c
                    return (s, new_word_state, transition_target),

                None => {               // There is no transition with char c
                    self.add_transition(s, c, new_word_state);
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

    fn transition_to(&mut self, from: i32, with: u8, to: i32) -> Option<&mut i32> {
        // why doesn't filter work?
        self.transition_with(from, with).and_then(|target| if *target == to {Some(target)} else {None})
    }

    fn transition_with(&mut self, from: i32, with: u8) -> Option<&mut i32> {
        if self.state(from).transition_with == 0 {  // state has 0 or 1 transitions
            let target_index = self.state(from).transition_to;
            if target_index == -1 { // state has no transitions
                None
            } else {
                unsafe {
                    let target: &mut i32 =
                        self.transitions
                        .get_unchecked_mut(target_index as usize)
                        .get_unchecked_mut(encode_letter(with) as usize);

                    if *target == -1 {
                        None
                    } else {
                        Some(target)
                    }
                }
            }
        } else {    // state has >1 transitions
            if self.state(from).transition_with == with {
                Some(&mut self.mutstate(from).transition_to)
            } else {
                None
            }
        }
    }

    fn add_transition(&mut self, from: i32, with: u8, to: i32) {
        if self.state(from).transition_with == 0 {
            if self.state(from).transition_to == -1 {
                // state has no transitions. make its single transition be
                // to the given one
                self.mutstate(from).transition_with = with;
                self.mutstate(from).transition_to = to;
            } else {
                // state has many transitions
                let target = self.state(from).transition_to as usize;
                unsafe {
                    *self.transitions
                        .get_unchecked_mut(target)
                        .get_unchecked_mut(encode_letter(with) as usize)
                        = to;
                }
            }
        } else {
            // state has exactly one transition. need to create a multiple
            // transition array and move the transition there
            unsafe {
                let letter = self.state(from).transition_with;
                let i = self.grow_transitions();

                *self.transitions
                    .get_unchecked_mut(i)
                    .get_unchecked_mut(encode_letter(with) as usize)
                    = to;
                *self.transitions
                    .get_unchecked_mut(i)
                    .get_unchecked_mut(encode_letter(letter) as usize)
                    = self.state(from).transition_to;

                self.mutstate(from).transition_with = 0;
                self.mutstate(from).transition_to = i as i32;
            }
        }
    }

    fn grow_transitions(&mut self) -> usize {
        let i = self.transitions.len();
        self.transitions.push([-1 as i32; ALPHABET_SIZE as usize]);

        i
    }

    fn copy_transitions_to_blank_state(&mut self, from: i32, to: i32) {
        if self.state(from).transition_with == 0 {
            let i = self.grow_transitions();
            unsafe {
                let target = self.state(from).transition_to as usize;
                *self.transitions.get_unchecked_mut(i)
                    = self.transitions.get_unchecked_mut(target).clone();
            }
            self.mutstate(to).transition_with = 0;
            self.mutstate(to).transition_to = i as i32;
        } else {
            self.mutstate(to).transition_with = self.state(from).transition_with;
            self.mutstate(to).transition_to = self.state(from).transition_to;
        }
    }
}

fn encode_letter(c: u8) -> u8 {
    c - ALPHABET_OFFSET
}

#[derive(Clone)]
#[repr(packed)]
struct Transition(u8, i32);

#[repr(packed)]
struct State {
    len:    i32,
    slink:  i32,
    transition_with: u8,
    transition_to: i32,
}

impl Default for State {
    fn default() -> State {
        State {
            len: 0,
            slink: -1,
            transition_with: 0,
            transition_to: -1
        }
    }
}


fn main() {
    let mut args = env::args();

    args.next().expect("no zeroth argument?!");
    let filename = args.next().expect("no input file provided");
    let mut f = File::open(filename).expect("unable to open file");
    let size = f.metadata().expect("unable to get file metadata").len();

    let mut buf = Vec::with_capacity(size as usize);

    f.read_to_end(&mut buf).expect("unable to read file");

    let mut b = Blumer::new(size as usize);

    // PROFILER.lock().unwrap().start("./blumer.profile").expect("Couldn't start");
    // b.process_chars(buf.into_iter().filter(|&c| c <= 'z' as u8 && c >= 'a' as u8));
    b.process_chars(buf.into_iter());
    // PROFILER.lock().unwrap().stop().expect("Couldn't stop");

    println!(
        "Уважаеми гл. ас. Митанкин,\n\nАвтоматът ми има {:?} състояния, от които {:?} финални.\nПреходите са {:?}.\n\nБлагодаря!",
        b.count_states(),
        b.count_finals(),
        b.count_transitions()
    );

    process::exit(0);
}
