#lang dssl2

import 'interface.rkt'

interface J (I):
    def m3(self, _, _)
    
class MyJ (J):
    def __init__(self): pass
    def m1(self, x): pass
    def m2(self, x): pass
    def m3(self, x, y): pass