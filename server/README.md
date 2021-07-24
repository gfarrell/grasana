# Grasana

Grasana is a tool for representing Asana projects as graphs (well, mostly as
trees actually). It can output the following formats:

- `DOT`: the graphviz DOT language
- `HTML`: (TODO) an HTML page complete with D3 to render the graph nicely and
  interactively
- `JSON`: representing either in graph form or tree form in JSON

There are two representations Grasana uses for projects: "graph" form and "tree"
form. The graph form represents the project as a tuple containing a list of
tasks and a list of edges (relations between tasks). Edges can represent either
subtask relations or dependencies (NB. Grasana does not currently parse
dependencies, only subtasks). The tree form represents the project as a tree in
which each node has an `id`, a `name`, and `children` (a list of child nodes).

The `DOT` output format uses the graph representation, whereas the `HTML` format
technically uses the tree representation under the hood. You can output either
graph or tree form when using the `JSON` format

## Installation

!TODO

## Usage

    grasana format [-t token] projectid

* `format`: can be one of `html`, `dot`, `jsontree`, or `jsongraph`;
* `token`: your [Asana PAT][asana-pat] (optionally as an environment variable
  instead to keep it out of your command history);
* `projectid`: the id of the project you want to represent graphically.

[asana-pat]: https://developers.asana.com/docs/personal-access-token

### Example: rendering immediately with graphviz

If you have [graphviz](https://graphviz.org/) installed, you can use it to
immediately render your project as an SVG.

    grasana dot <projectid> | dot -Tsvg > ./project.svg

## Testing

Some (but not all) of grasana has unit tests, just run `stack test` to go
through the spec. All tests specs are written using `Hspec` and can be found in
the `/test` directory.

    stack test

### Mocking HTTP Requests

I haven't yet worked out a good way to do this in Haskell so everything which
touches the Asana API via HTTP requests lacks unit tests. I have some ideas,
however, inspired by the following resources:

- https://making.pusher.com/unit-testing-io-in-haskell/
- https://lexi-lambda.github.io/blog/2017/06/29/unit-testing-effectful-haskell-with-monad-mock/

This might look something along the lines of a specialised Monad for HTTP:

    class Monad m => MonadHTTP m where
      httpJSON :: Network.HTTP.Simple.Request -> m (Network.HTTP.Simple.Response Data.ByteString.Lazy.ByteString)

## Contributing

!TODO
