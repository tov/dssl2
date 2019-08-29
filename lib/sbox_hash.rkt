#lang dssl2

# Each instance is a randomly generated hash function. The constructor
# takes the bit length for the resulting hash codes. The only method,
# `apply`, hashes the string representation of its input to an integer
# of the requested bit length.
class SboxHash:
    let hash_max: nat?
    let start:    nat?
    let sbox:     vec?

    def __init__(self, hash_bits: nat?):
        self.hash_max = 2 ** hash_bits - 1
        self.start    = random_bits(hash_bits)
        self.sbox     = [ random_bits(hash_bits) for _ in 256 ]

    # Maps a character to its randomized hash code.
    def _substitute(self, c: char?) -> nat?:
        self.sbox[int(c) % self.sbox.len()]
     
    # Combines a character with the hash code, by `xor`ing its
    # substitution value into the hash code.
    def _combine(self, hash_code: nat?, c: char?) -> nat?:
        hash_code ^ self._substitute(c)
    
    # Mixes a hash code to ensure that each combined character affects
    # different bits.    
    def _mix(self, hash_code: nat?) -> nat?:
        3 * hash_code 

    # Applies the hash function.
    def apply(self, input: AnyC) -> nat?:
        let input_string = '%d'.format(input)
        let hash_code = self.start
        for c in input_string:
            hash_code = self._mix(self._combine(hash_code, c)) & self.hash_max
        return hash_code
      
    # Two `SboxHash`es are only equal if they have the same object
    # identity.
    def __eq__(self, other) -> bool?:
        self is other
        
    # Prints an `SboxHash` without showing the sbox (because it's big).
    def __print__(self, print):
        print("#<object:SboxHash hash_max=%p start=%p sbox=...>",
              self.hash_max, self.start)

# Creates a 64-bit hash function
def SboxHash64(): SboxHash(64)

def HashFunctionC(X): FunC[X, int?]
def make_sbox_hash(): return SboxHash64().apply
