# Grasana Server

This package provides the server backend for the Grasana project.
Grasana allows you to explore an Asana project graphically, which makes
it easier to understand the dependencies for a set of tasks (both as
subtasks and explicit dependencies).

The Server backend talks to the Asana API and transforms a project into
a graph representation which the frontend can then easily render.

## TODO

- [x] Create Asana API interface to extract data from Asana
- [x] Create transformer to turn an initial set of tasks into a dependency tree
- [x] Parse the dependency tree into a list of tasks and a list of edges
- [ ] Create an HTTP backend server
- [ ] Host the frontend from the server
- [ ] Move from Asana PAT (hardcoded) to Oauth

## Installation and Usage

!TODO

## Testing

!TODO

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
