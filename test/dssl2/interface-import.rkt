#lang dssl2

#import 'interface.rkt'

#interface J (I):
#    def m3(self, _, _)
    
class MyJ: # (J):
    let _inner
    def __init__(self): pass # self._inner = False
    def m(self): self._inner
#    def m1(self, x): pass
#    def m2(self, x): pass
#    def m3(self, x, y): self._inner