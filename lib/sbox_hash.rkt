#lang dssl2

def HashFunctionC(X): FunC(X, int?)

def make_sbox_hash() -> HashFunctionC(str?):
    let sbox = [ random_bits(64) for i in 256 ]
    def hash(input_string):
        let result = 0
        for c in input_string:
            let svalue = sbox[ord(c)]
            result = result ^ svalue
            result = (3 * result) % (2 ** 64)
        return result
    hash