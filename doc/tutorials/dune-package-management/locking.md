# Locking Your Dependencies

In the default use-case Dune will automatically determine which packages to
install, by reading the package constraints, determining compatible versions and
installing the dependencies automatically.

For many projects this is a good and acceptable behavior as users often want to
use new versions of their dependencies. However some projects might want to
keep a fixed set of (transitive) dependencies that is only updated manually.

## Create a lock directory manually

If a lock directory exists in the source, Dune will use that to fix the exact
version and source of dependencies. The default name of said lock directory is
`dune.lock`. Lock directories are created with:

```
$ dune pkg lock
Solution for dune.lock

Dependencies common to all supported platforms:
duniverse (dune-built):
- base.v0.16.3
- ocaml.5.2.0
- ocaml-base-compiler.5.2.0
- sexplib0.v0.16.0

opam sandbox:
- conf-gmp.4
```

The output shows packages grouped by how they will be built:

* **duniverse (dune-built)**: These packages use Dune as their build system and
  will be built alongside your project code, providing full editor navigation
  and the ability to edit dependency source.

* **opam sandbox**: These packages use other build systems and will be built
  in a separate sandbox.

Whenever Dune encounters a `dune.lock` folder, it will use the set of
dependencies defined in the lock. It contains all the metadata about package
names and versions, their dependencies and source locations that are necessary
to build the project's dependencies.

On the next build, Dune will read the stored solver solution from the
`dune.lock` directory, download and build the dependencies and then continue on
building the project as usual.

The lock directory will not be updated until `dune pkg lock` is rerun.

:::{note}
This approach is similar to using `opam switch export --full --freeze` to
export the configuration of a switch.
:::

Deleting the lock directory will cause Dune to fall back to automatically
determining dependency versions via the declared package constraints.

## Fetching duniverse packages

For duniverse packages, you can optionally pre-fetch the sources before
building:

```
$ dune pkg fetch
Fetching base.v0.16.3 to duniverse/base.v0.16.3
Fetching sexplib0.v0.16.0 to duniverse/sexplib0.v0.16.0
Fetched 2 duniverse package(s) to duniverse/
```

This places package sources in the `duniverse/` directory where they can be:

* Navigated by your editor (Merlin/LSP "go to definition" works into dependencies)
* Modified directly for local patches or debugging
* Optionally committed to version control for fully reproducible builds
