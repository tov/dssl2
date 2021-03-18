#lang dssl2

let _N_CHARS      = 256
let _CHAR_MASK    = _N_CHARS - 1
let _WORD_MASK    = (1 << 64) - 1
let _UINT16_LIMIT = 1 << 16


# Interface for an object providing a hash function.
interface HASHER:
    # Hashes `input`.
    def hash(self, input: AnyC) -> nat?
 
       
###   
### Random number and sbox generation
###
    
def _random_uint16():
    return random(_UINT16_LIMIT)
    
def _random_uint64():
    return _random_uint16()         \
        ^ (_random_uint16() << 16)  \
        ^ (_random_uint16() << 32)  \
        ^ (_random_uint16() << 48)
        
def _random_sbox():
    return [ _random_uint64() for _ in range(_N_CHARS) ]


# Each instance is a randomly generated 64-bit hash function. The
# only method, `hash`, hashes the string representation of its
# input to an unsigned 64-bit integer.
class SboxHash64 (HASHER):
    let _start:   nat?
    let _sbox:    vec?

    def __init__(self):
        self._start = _random_uint64()
        self._sbox  = _random_sbox()
        
    # Applies the hash function.
    def hash(self, input: AnyC) -> nat?:
        let hash_code = self._start
        for c in input if str?(input) else str(input):
            let substituted = self._sbox[int(c) & _CHAR_MASK]
            let combined    = hash_code ^ substituted
            let mixed       = 3 * combined
            hash_code = mixed & _WORD_MASK
        return hash_code

    # Two `SboxHash`es are only equal if they have the same object
    # identity.
    def __eq__(self, other) -> bool?:
        return self is other

    # Prints an `SboxHash` without showing the sbox (because it's big).
    def __print__(self, print):
        print("#<object:SboxHash _start=%p _sbox=...>", self._start)

def SboxHash():
    return SboxHash64()

def SboxHash?(o):
    return SboxHash64?(o)

def make_sbox_hash():
    return SboxHash64().hash
