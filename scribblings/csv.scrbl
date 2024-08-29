#lang scribble/manual

@require["common.rkt"]

@title{The @tt{csv} library}

This library provides a function for reading comma-separated values (CSV) files.

@defprocform[read_csv]{(input: @racket[str?]): @racket[VecC][@racket[VecC][@racket[AnyC]]}

Reads the CSV file @racket[input] and returns its contents as a vector of
vectors. The outer vector has one element for each row in the file, and each
element has one element for each column in the row.

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
