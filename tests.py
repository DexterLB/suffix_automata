from itertools import chain
from collections import defaultdict

def get_infixes_size_n(word, n):
    return (word[i:i+n] for i in range(len(word) - n + 1))

def get_infixes(word):
    return chain.from_iterable((get_infixes_size_n(word, i) for i in range(2,len(word) +1)))

def get_suffixes(word):
    return (word[len(word) - 1 -i:] for i in range(len(word)))

def get_contexts(infixes):
    contexts = defaultdict(lambda: set())
    for infix in infixes:
        contexts[infix[1:]].add(infix[0])
    return dict(contexts)

def get_number_of_states(word, contexts):
    # Number of prefixes
    num_states = len(word) + 1
    # Number of infixes in different left contexts
    num_states += sum((1 for k, v in contexts.items() if len(v) > 1))
    return num_states

def get_number_of_final(word, contexts):
    suffixes = set(get_suffixes(word))
    final_states = 1

    print(contexts.keys())
    print(set(suffixes))

    final_states += sum((1 for k in contexts.keys() if k in suffixes))
    return final_states

def get_states(word):
    infixes = get_infixes(word)
    contexts = get_contexts(infixes)

    prefixes = {word[0:i] for i in range(len(word) + 1)}
    different_contexts_states = {k for k, v in contexts.items() if len(v) > 1}
    return different_contexts_states | prefixes


def get_transitions(word):
    states = sorted(list(get_states(word)), key=len)
    transitions = {}

    for state in states:
        for letter in set(word):

            new_state = state + letter

            shortest_state = None

            for other in states:
                if other.endswith(new_state):
                    shortest_state = other
                    break;
            if shortest_state:
                transitions[(state, letter)] = shortest_state
    return transitions

def dotify(p):
    if p == "":
        return "ε"
    else:
        return p

def draw_dawg(word, filename):
    states = get_states(word)
    transitions = get_transitions(word)

    with open(filename, "w") as f:
        f.write("digraph g {")
        f.write("\n")

        for (p, a), q in transitions.items():
            if len(q) - len(p) > 1:
                f.write('{} -> {} [label="{}",style=dotted];'.format(dotify(p), dotify(q), a))
                f.write("\n")
            else:
                f.write('{} -> {} [label="{}"];'.format(dotify(p), dotify(q), a))
                f.write("\n")

        for p in states:
            if word.endswith(p):
                f.write('{} [shape=doublecircle];'.format(dotify(p), dotify(q), a))
                f.write("\n")

        f.write("}\n")

def get_nums(word):
    infixes = get_infixes(word)
    contexts = get_contexts(infixes)
    return (get_number_of_states(word, contexts), get_number_of_final(word, contexts))