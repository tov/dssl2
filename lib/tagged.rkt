#lang dssl2

class Tagged:
    let _value
    let _tag: str?

    def __init__(self, value, tag: str?):
        self._value = value
        self._tag   = tag

    def __print__(self, print):
        print('%p:%s', _V(self), _T(self))
    
    def value(self):
        return self._value
        
    def tag(self):
        return self._tag
               
    def __cmp__(a, b): cmp(_V(a), _V(b))
    def __rcmp__(b, a): cmp(_V(a), _V(b))
    def __add__(a, b): _L('+', _add, a, b)
    def __radd__(b, a): _L('+', _add, a, b)
    def __sub__(a, b): _L('-', _sub, a, b)
    def __rsub__(b, a): _L('-', _sub, a, b)
    def __mul__(a, b): _L('*', _mul, a, b)
    def __rmul__(b, a): _L('*', _mul, a, b)
    def __div__(a, b): _L('/', _div, a, b)
    def __rdiv__(b, a): _L('/', _div, a, b)
    def __idiv__(a, b): _L('//', _idiv, a, b)
    def __ridiv__(b, a): _L('//', _idiv, a, b)
    def __mod__(a, b): _L('%', _mod, a, b)
    def __rmod__(b, a): _L('%', _mod, a, b)
    def __pow__(a, b): _L('**', _pow, a, b)
    def __rpow__(b, a): _L('**', _pow, a, b)
    def __and__(a, b): _L('&', _and, a, b)
    def __rand__(b, a): _L('&', _and, a, b)
    def __or__(a, b): _L('|', _or, a, b)
    def __ror__(b, a): _L('|', _or, a, b)
    def __xor__(a, b): _L('^', _xor, a, b)
    def __rxor__(b, a): _L('^', _xor, a, b)
    def __lshift__(a, b): _L('<<', _lshift, a, b)
    def __rlshift__(b, a): _L('<<', _lshift, a, b)
    def __rshift__(a, b): _L('>>', _rshift, a, b)
    def __rrshift__(b, a): _L('>>', _rshift, a, b)
    def __neg__(a): _M('-', _neg, a)
    def __pos__(a): _M('+', _pos, a)
    def __invert__(a): _M('~', _invert, a)
    

def _L(nm, op, a, b):
    let value = op(_V(a), _V(b))
    let tag   = '(%s %s %s)'.format(nm, _T(a), _T(b))
    return Tagged(value, tag)

def _M(nm, op, a):
    let value = op(_V(a))
    let tag   = '(%s %s)'.format(nm, _T(a))
    return Tagged(value, tag)
    
def _V(o):
    while Tagged?(o): o = o.value()
    return o

def _T(o):
    return o.tag() if Tagged?(o) else '_'

def _add(a, b): a + b
def _sub(a, b): a - b
def _mul(a, b): a * b
def _div(a, b): a / b
def _idiv(a, b): a // b
def _mod(a, b): a % b
def _pow(a, b): a ** b

def _and(a, b): a & b
def _or(a, b): a | b
def _xor(a, b): a ^ b
def _lshift(a, b): a << b
def _rshift(a, b): a >> b

def _pos(a): +a
def _neg(a): -a
def _invert(a): ~a
