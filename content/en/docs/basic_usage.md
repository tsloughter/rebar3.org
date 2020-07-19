---
title: "Basic Usage"
weight: 2
excerpt: ""
---

## New App or Release

There are two main ways to organize code with rebar3 projects: either as a single application, or as an umbrella project.

Single application projects contain a lone top-level application at the root of the directory, with its Erlang source modules directly inside a `src/` directory. This format is applicable to libraries to be published on github or in hex with the objective of making them shareable to the world, but can also be used with [Releases](doc:releases), which allow to ship an Erlang runtime system that boots the application directly.

Umbrella projects' defining characteristic is that they can contain multiple top-level Erlang/OTP applications, usually within a top-level `apps/` or `lib/` directory. Each of these applications may contain its own rebar.config file. This format is applicable to only for releases with one or more top-level applications.

Rebar3 comes with templates for creating either types of project, callable through the `rebar3 new <template> <project-name>` command. The `<template>` value can be any of:

- `app`: a stateful OTP application with a supervision tree, as a single application project

- `lib`: a library OTP application (without supervision trees), useful for grouping together various modules, as a single application project

- `release`: an umbrella project ready to be released

- `escript`: a special form of single application project that can be built as a runnable script

- `plugin`: structure for a rebar3 plugin.


For example:

```shell
$ rebar3 new app myapp
===> Writing myapp/src/myapp_app.erl
===> Writing myapp/src/myapp_sup.erl
===> Writing myapp/src/myapp.app.src
===> Writing myapp/rebar.config
===> Writing myapp/.gitignore
===> Writing myapp/LICENSE
===> Writing myapp/README.md
```

For more information on `new` and available options check the docs on [commands](/docs/commands) and to learn how to create and use custom templates go to the [templates tutorial](/docs/using-templates).

## Adding Dependencies

Dependencies are listed in `rebar.config` file under the `deps` key:

```erlang
{deps, [
        {cowboy, "1.0.1"}, % package
        {cowboy, {git, "git://github.com/ninenines/cowboy.git", {tag, "1.0.1"}}} % alternatively, source
        ]
}.
```

Now you can add the dep to one of your project's application's .app.src file under applications so that Erlang knows the dependency is required for yours to work:

```erlang
{application, <APPNAME>,
 [{description, ""},
  {vsn, "<APPVSN>"},
  {registered, []},
  {modules, []},
  {applications, [
                 kernel
                 ,stdlib
                 ,cowboy
                 ]},
  {mod, {<APPNAME>_app, []}},
  {env, []}
 ]}.
```

For more information on dependency handling view the [dependency documentation](/docs/dependencies) 

## Building

Only one command, `compile`, is required to fetch dependencies and compile all applications.

```shell
$ rebar3 compile
==> Verifying dependencies...
==> Fetching cowboy
==> Fetching ranch
==> Fetching cowlib
==> Compiling cowlib
==> Compiling ranch
==> Compiling cowboy
==> Compiling myapp
```

## Output Format

Output for installing dependencies, building releases and any other output written to disk is found in the `_build` directory at the root of the project.

```shell
_build/
└── default
  └── lib  
    ├── cowboy
    ├── cowlib
    └── ranch
```
More about profiles and the `_build` directory can be found in the [profiles documentation page](/docs/profiles).

## Testing

Tests by default are expected to be found under the `test/` directory, aside from `eunit` found within individual modules.

Dependencies that are only needed for running tests can be placed in the `test` profile:

```erlang
{profiles, [
    {test, [
        {deps, [
            {meck, {git, "git://github.com/eproxus/meck.git", {tag, "0.8.2"}}}
        ]}
    ]}
]}.
```
	 
Now the first time `rebar3 ct` is run `meck` will be installed to `_build/test/lib/`. But it will not be added to `rebar.lock`.

```shell
_build/
   └── test
     └── lib
       └── meck
```

## Releases and Target Systems

Releases are built using [relx](https://github.com/erlware/relx).

Creating a new project with a release structure and default `relx` config in the `rebar.config` file run:


```shell
$ rebar3 new release myrel
===> Writing myrel/apps/myrel/src/myrel_app.erl
===> Writing myrel/apps/myrel/src/myrel_sup.erl
===> Writing myrel/apps/myrel/src/myrel.app.src
===> Writing myrel/rebar.config
===> Writing myrel/config/sys.config
===> Writing myrel/config/vm.args
===> Writing myrel/.gitignore
===> Writing myrel/LICENSE
===> Writing myrel/README.md
```
Looking in `rebar.config` we find a couple elements that were not there in our application example.

```erlang
{relx, [{release, {myrel, "0.0.1"},
         [myrel]},

        {dev_mode, true},
        {include_erts, false},

        {extended_start_script, true}
       ]
}.

{profiles, [
    {prod, [{relx, [{dev_mode, false},
                    {include_erts, true}]}
     ]}
]}.
```

This configuration provides some nice defaults for building a release with relx for development (default profile) and for production (prod profile). When building a release for production we'll most likely want to create a target system (include erts) and definitely will not want the release to contain symlinks to apps (dev_mode false).

```shell
$ rebar3 release
===> Verifying default dependencies...
===> Compiling myrel
===> Starting relx build process ...
===> Resolving OTP Applications from directories:          
          _build/default/lib
          /usr/lib/erlang/lib
===> Resolved myrel-0.1.0
===> Dev mode enabled, release will be symlinked
===> release successfully created!
```
	 
With the default `rebar.config`, creating a compressed archive of the release as a target system is as simple as setting the profile to `prod` and running `tar`:

```shell
$ rebar3 as prod tar
===> Verifying default dependencies...
===> Compiling myrel
===> Starting relx build process ...
===> Resolving OTP Applications from directories:
          .../myrel/apps
          /usr/lib/erlang/lib
===> Resolved myrel-0.1.0
===> Including Erts from /usr/lib/erlang
===> release successfully created!
===> tarball myrel/_build/rel/myrel/myrel-0.1.0.tar.gz successfully created!

```

For more details go to the [release section](/docs/releases).
