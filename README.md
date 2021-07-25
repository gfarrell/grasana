# Grasana

Grasana is a tool for representing Asana projects as graphs (well, mostly as
trees actually). It can output the following formats:

- DOT: the graphviz DOT language
- HTML: an HTML page complete with D3 to render the graph nicely and
  interactively
- JSON: representing either in graph form or tree form in JSON

There are two representations Grasana uses for projects: "graph" form and "tree"
form. The graph form represents the project as a tuple containing a list of
tasks and a list of edges (relations between tasks). Edges can represent either
subtask relations or dependencies (NB. Grasana does not currently parse
dependencies, only subtasks). The tree form represents the project as a tree in
which each node has an `id`, a `name`, and `children` (a list of child nodes).

The `DOT` output format uses the graph representation, whereas the `HTML` format
technically uses the tree representation under the hood. You can output either
graph or tree form when using the `JSON` format

## Building and installing

There are two parts of Grasana: a typescript part and a Haskell part. The
typescript part is for the HTML output to make a nice tree viewer.

You can build the `grasana` executable by running `make dist/grasana`.
If it already exists, run `make clean` first. The executable will then
be copied, somewhat unsurprisingly, to `dist/grasana`.

If you want to install the binary you can run `make install` which will
clean, build, and then copy the binary to your local binary path (you
can find out what this is by running `stack path --local-bin`, the
default is `$HOME/.local/bin`). You will now be able to run `grasana` from
wherever you like!

## Usage

    grasana format [-t token] projectid

* `format`: can be one of `html`, `dot`, `jsontree`, or `jsongraph`;
* `token`: your [Asana PAT][asana-pat] (optionally as an environment variable
  instead to keep it out of your command history);
* `projectid`: the id of the project you want to represent graphically.

[asana-pat]: https://developers.asana.com/docs/personal-access-token

### Example: using environment variables

To avoid having to pass your Asana personal access token explicitly on
the command line you can set it in your shell's environment, for example
if you had a file called `secrets.env` with the following contents, you
could run `. ./secrets.env && grasana format projectid`.

    ASANA_PAT="<your PAT>"

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

Grasana was a toy project because I was frustrated by how bad Asana is at
handling deeply nested projects (which seems to me to be a good way of
representing [outcome maps][outcome-maps]).

If you would like to help make Grasana better, here is my TODO list which I'm
sure is missing many things. I have not been as good as I would have liked at
adding tests for Grasana (as you can see above) but please do add them where
possible when you are submitting features or fixes. Fork the repo, make some
changes in a branch, push it up and open a PR explaining the changes and the
reasoning behind the implementation and I'll review and merge (or reject).

[outcome-maps]: http://www.aaronsw.com/weblog/theoryofchange

### TODO

- [x] Asana API interactions
- [x] Graph representation
- [x] Tree representation
- [x] DOT output
- [x] Incorporate built JS for HTML output
- [x] Update JS app to handle incl. json
- [ ] Write tests for DOT rendering
- [ ] Write tests for HTML rendering
- [ ] Handle unsound graphs and exit properly
- [ ] Handle Asana errors nicely (auth, 404, etc.)
- [x] Handle missing PAT nicely
- [ ] Find a way to mock HTTP requests
- [x] Add installation instructions
- [x] Add contribution instructions
- [x] Allow passing of asana token as an option
- [ ] Zoom to mouse point not origin in `InteractiveSVGViewer`
- [ ] Add bounds around labels in the HTML visualisation
- [ ] Wrap text labels in the HTML visualisation

### Developing the HTML viewer

In order to make development of the HTML viewer easier, you can run a
local webpack server with live-reloading of changes. Just work inside
the `js` directory and run `yarn serve`. This will fire up a development
server on `localhost:8080` which you can use to test your changes.

Note that the application expects a JSON representation of the project
in tree form to be accessible at `window.treejson`. The `index.html` has
a dummy project copied in there for convenience of development, but if
you want to test something specific you can replace it with a project of
your choice. The easiest way to do this is to get Grasana to generate
the JSON of an actual project using `grasana jsontree -t <your token>
<your project id>` which you can then copy to your clipboard (e.g.
piping into `xclip -selection clipboard`) and pasting into `index.html`.

### Developing the CLI

Stack's `run` command is very useful for running the programme as you
develop. The project is organised into some representations (`TaskGraph`
and `TaskTree` modules) and renderers (`Html` and `Dot`). The `Asana`
module contains the functions for actually tasking to the Asana API.

In order to include the typescript project output into the generated
HTML, Grasana uses Template Haskell via `blaze-html` and `shakespeare`.
