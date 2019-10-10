#lang dssl2

import uint64

# Interface for an object providing a hash function.
interface HASHER:
    # Hashes `input`.
    def hash(self, input: AnyC) -> nat?

def _random_uint64():
    uint64(random_bits(64))

# Each instance is a randomly generated 64-bit hash function. The 
# only method, `hash`, hashes the string representation of its
# input to an unsigned 64-bit integer.
class SboxHash64 (HASHER):
    let _start:   uint64?
    let _sbox:    vec?

    def __init__(self):
        self._start   = _random_uint64()
        self._sbox    = [ _random_uint64() for _ in 256 ]

    # Maps a character to its randomized hash code.
    def _substitute(self, c: char?) -> uint64?:
        self._sbox[int(c) % self._sbox.len()]
     
    # Combines a character with the hash code, by `xor`ing its
    # substitution value into the hash code.
    def _combine(self, hash_code: uint64?, c: char?) -> uint64?:
        hash_code ^ self._substitute(c)
    
    # Mixes a hash code to ensure that each combined character affects
    # different bits.    
    def _mix(self, hash_code: uint64?) -> uint64?:
        3 * hash_code 

    # Applies the hash function.
    def hash(self, input: AnyC) -> nat?:
        let hash_code = self._start
        for c in str(input):
            hash_code = self._mix(self._combine(hash_code, c))
        return hash_code.as_int()
      
    # Two `SboxHash`es are only equal if they have the same object
    # identity.
    def __eq__(self, other) -> bool?:
        self is other
        
    # Prints an `SboxHash` without showing the sbox (because it's big).
    def __print__(self, print):
        print("#<object:SboxHash _start=%p _sbox=...>", self._start)

def SboxHash(_ignored): SboxHash64()

def make_sbox_hash(): SboxHash().hash
