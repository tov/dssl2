#lang scribble/manual

@require["common.rkt"]

@title{The @tt{csv} library}

This library provides a function for reading comma-separated values (CSV) files.

@defprocform[read_csv]{(input: @racket[str?]): @racket[VecC][@racket[VecC][@racket[str?]]]}

Reads the CSV file @racket[input] and returns its contents as a vector of
vectors of strings. The outer vector has one element for each row in the file,
and each row has one element for each column in the row. Individual cells are
represented as strings.

For example, a CSV file containing the following:

@verbatim{
state,initial,capital
Illinois,IL,Springfield
Massachusetts,MA,Boston
Missouri,MO,Jefferson City
}

would be read into:

@dssl2block|{
[['state', 'initial', 'capital'],
 ['Illinois', 'IL', 'Springfield'],
 ['Massachusetts', 'MA', 'Boston'],
 ['Missouri', 'MO', 'Jefferson City']]
}|

@defprocform[read_csv_headers]{(input: @racket[str?]): @racket[VecC][@racket[str?]]}

Reads the CSV file @racket[input] and returns the contents of its first row,
which are assumed to be column headers.

@defprocform[read_csv_data]{(input: @racket[str?]): @racket[VecC][@racket[VecC][@racket[str?]]]}

Reads the CSV file @racket[input] and returns the contents of its second and
later rows, skipping the first (which is assumed to contain column headers).
