#lang dssl2

import cons

let Vertex = nat?
let Weight = num?
let MaybeWeight = OrC(False, Weight)

interface WU_GRAPH:
    def get_size(self) -> nat?
    def get_edge(self, u: Vertex, v: Vertex) -> MaybeWeight
    def set_edge!(self, u: Vertex, v: Vertex, w: MaybeWeight) -> VoidC
    def get_adjacent(self, u: Vertex) -> ListOfC(Vertex)

class WuGraph (WU_GRAPH):
    let size: nat?
    let matrix: vec?

    def __init__(self, size):
        self.size = size
        self.matrix = [ [False; size] for _ in size ]

    def get_size(self): self.size

    def get_edge(self, u, v):
        self.matrix[u][v]

    def set_edge!(self, u, v, w):
        self.matrix[u][v] = w
        self.matrix[v][u] = w
  
    def get_adjacent(self, u):
        let result = nil()
        for v in self.size:
            if self.get_edge(u, v):
                result = cons(v, result)
        result

test "WuGraph":
    let g = WuGraph(8)
    assert_eq False, g.get_edge(0, 2)
    g.set_edge!(0, 2, 5)
    assert_eq 5, g.get_edge(0, 2)
    assert_eq 5, g.get_edge(2, 0)
    assert_eq False, g.get_edge(0, 1)
    assert_eq cons(2, nil()), g.get_adjacent(0)    
