#lang dssl2

import plot

# _fn_time : FunC[real?, AnyC] real? -> positive?
# Times `fn(n)`, noisily.
def _fn_time(fn, n):
    eprint(' %p…', n)
    let cpu = (time: fn(n)).cpu
    return max(cpu, 1e-5)

# _fn_times : FunC[real?, AnyC] VecC[real?] -> VecC[Vec[real?, nat?]]
# Times `fn` (noisily) applied to each number in `ns`.
def _fn_times(fn, ns):
    return [ [n, _fn_time(fn, n)] for n in ns ]


# _fn_line : FunC[real?, AnyC] VecC[real?] -> Vec[str?, VecC[Vec[real?, nat?]]]
# Times `fn` applied to each number in `ns` (noisily), and returns the
# timings paired with `fn`’s name.
def _fn_line(fn, ns):
    let label = '%s'.format(fn)
    eprint(' • %s at', label)
    let times = _fn_times(fn, ns)
    eprintln(' done.')
    return [ label, times ]


# benchmark_functions(VecC[FunC[real?, AnyC]],
#                     VecC[real?]) ->
#                    VecC[Vec[str?, VecC[Vec[real?, nat?]]]]
#
# Times all of the functions in `fns` applied to all of the input
# sizes in `ns`, returning the timing data in at form suitable
# for passing to `plot` from `import plot`.
def benchmark_functions(fns, ns):
    eprintln('Timing:')
    return [ _fn_line(fn, ns) for fn in fns ]


# plot_benchmarks(string?, VecC[FunC[real?, AnyC]], VecC[real?]) -> snip%
#
# Times all of the functions in `fns` applied to all of the input
# sizes in `ns`, and returns a plot of the results.
def plot_benchmarks(title, fns, ns):
    return plot(title, benchmark_functions(fns, ns),
                'input size', 'running time (ms)')


# log_plot_benchmarks(string?, VecC[FunC[real?, AnyC]], VecC[real?]) -> snip%
#
# Times all of the functions in `fns` applied to all of the input
# sizes in `ns`, and returns a logarithmic plot of the results.
def log_plot_benchmarks(title, fns, ns):
    return plot(title, benchmark_functions(fns, ns),
                'input size', 'log running time (ms)')
