---
title: "Dependencies"
weight: 5
excerpt: ""
---

{{% blocks/callout type="warning" title="Dependencies and Profiles" %}}
 Dependencies will always be compiled with the `prod` profile applied to their configuration. No other (besides `default`, of course) is used on any dependency. Even though they are configured for `prod` the dependency will still be fetched to the profile directory for the profile it is declared under. For example, a dependency in the top level `deps` will be under `_build/default/lib` and  dependency under the profile `test` will be fetched to `_build/test/lib`, and both will be compiled with their `prod` profile configuration applied. 
{{< /blocks/callout >}}

{{< blocks/callout type="warning" title="Conflict Resolution">}}
 Unlike previous versions of rebar there is a strict set of rules for which dependencies are chosen which does not change depending on when they are fetched or updated. The algorithm for this is described below. 
{{< /blocks/callout >}}

Rebar3 considers dependency versions to be informational only. Given the existing open source landscape in the Erlang community, trying to impose [semantic versioning](http://semver.org/) or any other similar scheme is usually dead in the water: 

- People update *some* versions but not all of them (git tags vs. branch names vs. OTP application versions) and they may contradict each other;

- Some people never update their versions;

- Not everyone subscribes to the same version schemes;

- People make errors in subscribing to semantic versioning;

- Many applications are stuck in versions smaller than `1.0.0`, and therefore considered unstable forever;

- Source dependencies are used in a majority at the time of this writing: figuring out version conflicts therefore requires downloading all transitive dependencies from all dependencies to figure out whether they conflict or not, every time.

Any other format for dependencies would require an entire overhaul of how open source Erlang development takes place before Rebar3 gets any kind of adoption.

Instead, Rebar3 will fetch and download dependencies in a level-order traversal. This means that the dependencies closest to the root of the dependency tree are those that will be chosen, regardless of their versions.

This means any dependency declared in your project's `rebar.config` will never be overwritten by a transitive dependency, and a transitive dependency will never be overridden by a later encountered conflicting transitive dependency.

This also means that if you prefer a version to be used above all else, you can just add it to your `rebar.config` file and pick what will be kept.

After each run of dependency fetching and resolving the list of final dependencies are written to `rebar.lock`.

{{< blocks/callout type="warning" title="Treating Conflicts as Errors">}}
 If you ever want rebar3 to abort as soon as it detects a dependency conflict, instead of skipping the file and proceeding as usual, add the line `{deps_error_on_conflict, true}.` to your rebar configuration file. 
{{< /blocks/callout >}}

## Declaring Dependencies

Dependencies can be declared in a top-level `rebar.config` file, and inspected with the `rebar3 tree` command.

In general, Rebar3 supports two types of dependencies:

- Source dependencies

- Package dependencies

Both of these may be handled slightly differently (packages offer more information than source dependencies before downloading them, and will be cached locally in `~/.cache/rebar3/`), but they will generally behave the same.

All dependencies are to be project-local. This is usually a good choice in order avoid the common problems of global libraries having version conflicts. It also helps with the general Erlang mechanism of [Releases](doc:releases), which builds standalone systems.

Dependencies fit any of the following formats:

```erlang
{deps,[
  %% Packages
  rebar,
  {rebar,"1.0.0"},
  {rebar, {pkg, rebar_fork}}, % rebar app under a different pkg name
  {rebar, "1.0.0", {pkg, rebar_fork}},
  %% Source Dependencies
  {rebar, {git, "git://github.com/erlang/rebar3.git"}},
  {rebar, {git, "http://github.com/erlang/rebar3.git"}},
  {rebar, {git, "https://github.com/erlang/rebar3.git"}},
  {rebar, {git, "git@github.com:erlang/rebar3.git"}},
  {rebar, {hg, "https://othersite.com/erlang/rebar3"}},
  {rebar, {git, "git://github.com/erlang/rebar3.git", {ref, "aef728"}}},
  {rebar, {git, "git://github.com/erlang/rebar3.git", {branch, "master"}}},
  {rebar, {git, "git://github.com/erlang/rebar3.git", {tag, "3.0.0"}}},
  %% Legacy support -- added parts such as [raw] are ignored
  {rebar, "3.*", {git,"git://github.com/erlang/rebar3.git"}},
  {rebar, {git, "git://github.com/erlang/rebar3.git"}, [raw]},
  {rebar, "3.*", {git, "git://github.com/erlang/rebar3.git"}, [raw]}
]}.
```

As the example above shows, for the current versions, only packages, git sources, and mercurial sources are supported. Custom dependency sources can be added by [implementing the resource behaviour](doc:custom-dep-resources) and including it like a plugin.

Rebar3 will fetch whatever else is required. 

However, the dependency handling done by Erlang/OTP to boot and shut down applications, and tools (even part of Rebar3) to build releases and scripts, depend on a more granular dependency declaration, specifying which of each application in a project depend on others.

You should add each dependency to your `app`  or `app.src` files:

```erlang
{application, <APPNAME>,
 [{description, ""},
  {vsn, "<APPVSN>"},
  {registered, []},
  {modules, []},
  {applications, [kernel
                 ,stdlib
                 ,cowboy
                 ]},
  {mod, {<APPNAME>_app, []}},
  {env, []}
 ]}.
```

This will allow the flexibility to write and generate software where various disjoint applications can coexist in a virtual machine without their dependencies being entirely tangled together. For example, you may want your web server to be able to run independently of administrative and debugging tools, even if they should be available in production.

If more formats require support, Rebar3 can be extended via the [`rebar_resource` behaviour](https://github.com/erlang/rebar3/blob/master/src/rebar_resource.erl), and sent to the maintainers with a [pull request](https://github.com/erlang/rebar3/blob/master/CONTRIBUTING.md).

## Source Dependencies

For a regular dependency tree, such as:

```
  A
 / \
B   C

```

the dependencies `A`, `B`, and `C` will be fetched.

However, for more complex trees, such as:

```
   A
 /   \
B    C1
|
C2

```
The dependencies `A`, `B`, and `C1` will be fetched. When Rebar3 will encounter the requirement for `C2`, it will instead display the warning: `Skipping C2 (from $SOURCE) as an app of the same name has already been fetched`.

Such a message should let the user know which dependency has been skipped.

What about cases where two transitive dependencies have the same name and are on the same level?

```
   A
 /   \
B     C
|     |
D1    D2

```

In such a case, `D1` will take over `D2`, because `B` lexicographically sorts before `C`. It's an entirely arbitrary rule, but it is at least a rule that ensures repeatable fetches.

In the event users disagree with the outcome, they can bring `D2` to the top level and ensure it will be chosen early:

```
  A D2
 /   \
B     C
|     |
D1    D2

```

Which will yield `A`, `B`, `C`, and `D2`.

Rebar3 will perform that same algorithm with packages, and will also detect circular dependencies and error out on these. However, Source dependencies *always* take precedence over packages.

Dependencies in the `_checkouts` directory will be left untouched.

## Package Manager Dependencies

Rebar3 uses [hex.pm](https://hex.pm) to provide a managed set of packages and their dependencies, which you can get a list of with the following command: 

```shell
$ rebar3 update
===> Updating package index...
```

A package dependency can then be included as shown earlier:

```erlang
{deps, [
        {cowboy, "1.0.0"}
       ]
}.
```

The package manager will fetch a minimal set of dependencies to comply with the build rules based on their dependency graphs, sorted topologically. To support proper behaviour with source dependencies, the set of package dependencies will be merged and handled along with source dependencies' level-order traversal. The end result to pick winners in case of conflicts will be similar to source dependencies, with warnings displayed.

The main difference is that package dependencies should be much faster by virtue of being able to resolve everything with pre-computed data, make use of resources such as CDNs and local caches, and support compressed archive formats. They also are stable and do not risk changing at any moment's notice.

To use a CDN other than the default, such as one of the [official mirrors](https://hex.pm/docs/mirrors) add to your project's `rebar.config` or to `~/.config/rebar3/rebar.config`:

```erlang
{rebar_packages_cdn, "https://s3-eu-west-1.amazonaws.com/s3-eu.hex.pm"}. 
```

## Checkout Dependencies

To handle the case of dependencies you wish to work on locally, there is the `_checkouts` directory. Simply make a symlink or copy your dependency to `_checkouts` at the top level of your project. Any application/plugin in `_checkouts` will take precedence over the same application if it is additionally listed in the `rebar.config`'s `deps`, `plugins` or `project_plugins`. This also overrides anything 'plugins' . This also overrides anything already fetched to `_build`.

Note that `_checkout` is an override, this means that for it to work an dep/plugin entry in rebar.config needs to exist

Note that `_checkout` is an override, this means that for it to work an dep/plugin entry in rebar.config needs to exist

```
_checkouts
└── depA
    └── src
```

## Upgrading Dependencies

Whenever a dependency is fetched and locked, Rebar3 will extract a reference from the source file to pin it to a specific version in time. The dependency should be forced to follow that version on following builds.

Rebar3 allows to upgrade previously installed dependencies to newer versions. This can take two forms: forwarding a branch's reference to its latest version (e.g. an old `master` up to the new `master`'s `HEAD` for source files, or the newest versions for packages that didn't specify one), or crushing an existing locked dependency with a new version from the `rebar.config` file.

In the following dependency tree:

```
A  B
|  |
C  D
```
The user can upgrade either dependency (`rebar3 upgrade A` and `rebar3 upgrade B`) or both at once (`rebar3 upgrade A,B` or `rebar3 upgrade`, which upgrades *all* dependencies).

Upgrading only `A` means that `A` and `C` may be upgraded. Upgrades to `B` and `D` will be ignored.

Upgrading source dependencies is fraught with peril, though, and interesting corner cases arise. Consider the following dependency tree:

```
    A       B       C1
   / \     / \     / \
  D   E   F   G   H   I2
  |   |
  J   K
  |
  I1

```

After fetching the dependency tree above, `I2` would be chosen before `I1`. However, following an upgrade from `C1` to `C2` where `C2` no longer needs to depend on `I2`, Rebar3 will automatically go fetch `I1` under the `A` tree (even if no upgrade in `A` was required) to provide a correct new tree:

```
    A       B     C2
   / \     / \    |
  D   E   F   G   H
  |   |
  J   K
  |
  I1
```

Where `I2` no longer exists in the project, and `I1` now does.

## Lock Files

Lock files (`rebar.lock`) are one of the only artifacts that will be generated by rebar3 that will live outside of `_build/`, and that should be checked into source control. The lock file contains information regarding code dependencies, including immutable references for source dependencies like those in git or hg, and their versions along with expected hashes for packages.

The objective is to use more accurate information regarding found dependencies than what would be obtained through the config file on its own, allowing, for example, to configure a dependency to be updated from 'master', but to be locked to a stable tested version in the meanwhile. Only unlocking or upgrading the dependency will allow it to move it to a newer or different version. The idea is to allow repeatable builds, even if, for example, git tags or branches are destructively modified by someone.

Rebar3 will also use the lock file as the true source of authority for dependencies when switching branches or fetching transitive deps (it will pick data from the lock file if any is available) rather than the `rebar.config` file. This allows to more easily carry a safe tested state into other applications when loose references or versions are used in the `rebar.config` file.

The format is expected to be forwards and backwards compatible since version 3.0.0 (actually, beta-4). Rebar3 will annotate lock file versions according to the format of metadata stored, and warn when an old version of rebar3 is used to read a newer version of lock files. This can tell the user that some metadata that is judged important at a later date will be lost by using an older copy of the tool.

## Dependency Lock Management

The status of locks and dependencies can be inspected with `rebar3 deps`:

```shell
→ rebar3 deps
cowboy* (package)
recon* (git source)
erlware_commons (locked git source)
getopt* (locked git source)
providers (locked hg source)
relx (locked git source)
```

The dependencies are compared with the lock file to report on their status. Those whose presence on disk does not match the lock file are annotated with an asterisk (`*`).

If a dependency is locked but no longer required nor in your config file, you can unmark it with `rebar3 unlock <app>` (`rebar3 unlock <app1>,<app2>,...,<app3>` for many apps).

Calling `rebar3 unlock` will flush the lock file entirely.

Alternatively, `rebar3 tree` can be used to display the tree of current dependencies:


```shell
→ rebar3 tree
...
|- bootstrap-0.0.2 (git repo)
|- dirmon-0.1.0 (project app)
|- file_monitor-0.1 (git repo)
|- peeranha-0.1.0 (git repo)
|  |- gproc-git (git repo)
|  |- interclock-0.1.2 (git repo)
|  |  |- bitcask-1.7.0 (git repo)
|  |  |  |- lager-2.1.1 (hex package)
|  |  |  |  |- goldrush-0.1.6 (hex package)
|  |  |- itc-1.0.0 (git repo)
|  |- merklet-1.0.0 (git repo)
|- recon-2.2.2 (git repo)
|- uuid-1.5.0 (git repo)
|  |- quickrand-1.5.0 (git repo)
```

## Elixir Dependencies

Elixir Dependencies are supported starting with Rebar3 version 3.7.0, and Elixir version 1.7.4 through the use of a plugin. See [the relevant plugin section](https://www.rebar3.org/v3/docs/using-available-plugins#section-elixir-dependencies) for details
