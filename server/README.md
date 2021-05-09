# Grasana Server

This package provides the server backend for the Grasana project.
Grasana allows you to explore an Asana project graphically, which makes
it easier to understand the dependencies for a set of tasks (both as
subtasks and explicit dependencies).

The Server backend talks to the Asana API and transforms a project into
a graph representation which the frontend can then easily render.

## TODO

- [ ] Create Asana API interface to extract data from Asana
- [ ] Create transformer to turn an initial set of tasks into a dependency tree
- [ ] Parse the dependency tree into a list of tasks and a list of edges
- [ ] Create an HTTP backend server
- [ ] Host the frontend from the server
- [ ] Move from Asana PAT (hardcoded) to Oauth

## Installation and Usage

!TODO

## Testing

!TODO

## Contributing

!TODO
