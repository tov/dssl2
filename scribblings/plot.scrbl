#lang scribble/manual

@require["common.rkt"]

@title{The @tt{plot} library}

This library provides a basic wrapper around the line plots provided by @link["https://docs.racket-lang.org/plot/index.html"]{Racket's Plot library}.

@defprocform[plot]{(title: @racket[str?], data: @code{PlotSpec},
                    [x: @code{AxisSpec}, y: @code{AxisSpec}]):
                    @racket[AnyC]}

The main function in this library: draws a line plot from the given data, and
displays it. Run inside DrRacket for best results.

The format of each argument is documented along with their contracts, below.
The last two arguments describing axes are both optional.

At its simplest, a plot consisting of a single line can be drawn as follows,
with a vector of 2-dimensional points forming the line as the second argument:

@dssl2block|{
plot("line", [[1,1], [2,2], [3,3]])
}|

Axes may optionally be labeled:

@dssl2block|{
plot("line", [[1,1], [2,2], [3,3]], "abscissas", "ordinates")
}|

More complex plots with multiple lines must label each line---see
the definition of @code{LineSpec} below.

@dssl2block|{
plot("lines, plural",
     [["flat", [[1,2], [2,2], [3,2]]],
      ["steep", [[1,0], [2,10], [3,20]]]])
}|

@; could I render them? using scribble/example? (maybe, but sounds tricky)

@defconstform[PlotSpec]{@code{contract?}}

A @racket[PlotSpec] is one of:
@itemlist[
@item{A @racket[LineSpec], for a graph with a single (unlabeled) line.}
@item{A non-empty vector whose elements are 2-element vectors, the first of which is the name of a line, and the second is a @racket[LineSpec] for each line in the graph.}
]

For example, this is a @racket[PlotSpec] for a plot with a single line:
@dssl2block|{
[[1,1], [2,2], [3,3]]
}|

And this is also a @racket[PlotSpec], this one for a plot with two labeled lines:
@dssl2block|{
[["flat", [[1,2], [2,2], [3,2]]],
 ["steep", [[1,0], [2,10], [3,20]]]]
}|


@defconstform[LineSpec]{@code{contract?}}

A @racket[LineSpec] is a non-empty vector of @racket[PointSpec], and describes
the points in a single line within a line plot.

@defconstform[PointSpec]{@code{contract?}}

A @racket[PointSpec] is a vector of two @racket[num?], which correspond to the
@emph{x} and @emph{y} coordinate of a point, respectively.

@defconstform[AxisSpec]{@code{contract?}}

An @racket[AxisSpec] is a description of an axis---@emph{x} or @emph{y}. It is either:

@itemlist[
@item{A @racket[str?], which is the label for the axis.}
@item{A 2-element vector of a @racket[str?] and a @racket[TransformSpec], for labels which have both a label and a transformation applied.}
]

@defconstform[TransformSpec]{@code{contract?}}

Describing an axis with a @racket[TransformSpec] applies a transformation to the axis.
The @racket[TransformSpec]s currently supported are:

@itemlist[
@item{@racket["id"], the identity transformation (i.e., no transformation).}
@item{@racket["log"], which displays the axis in log-scale.}
]
