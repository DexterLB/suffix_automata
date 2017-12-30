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
    return (get_number_of_states(word, contexts), get_number_of_final(word, contexts))