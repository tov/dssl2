#lang dssl2

class Timing:
    let _n_elems
    let _cpu_time
    def __init__(self, n_elems, cpu_time):
        self._n_elems  = n_elems
        self._cpu_time = cpu_time
    def __print__(self, print):
        print('%p: %p', self._n_elems, self._cpu_time)

class Vector:
    let _data
    def __init__(self, n: nat?):
        self._data = vec(n, λ i: i)
    def len(self):
        return len(self._data)
    def __index_ref__(self, i):
        return self._data[i]
    def __index_set__(self, i, v):
        self._data[i] = v
    def __print__(self, print):
        self._data.__print(print)

def bench_shuffles(sizes: VecC[nat?], m_trials: nat?):
    return [ bench_shuffle_avg(n_elems, m_trials) for n_elems in sizes ]

def bench_shuffle_avg(n_elems: nat?, m_trials: nat?):
    let v = Vector(n_elems)
    let t = 0
    for _ in range(m_trials):
        t = t + time_one_shuffle(v).cpu
    return Timing(n_elems, float(t) / m_trials)

def time_one_shuffle(v: Vector?):
    return time 'shuffle(%p)'.format(len(v)):
        shuffle(v)

# Randomly shuffles the contents of `v`.
def shuffle(v: Vector?):
    let n = v.len()
    def swap(i, j):
        let vi = v[i]
        let vj = v[j]
        v[i] = vj
        v[j] = vi
    for i in range(n - 1):
        swap(i, random(i, n))

bench_shuffles([100 * (1 << i) for i in range(5)], 5)

