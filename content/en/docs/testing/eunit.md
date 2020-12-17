---
title: "EUnit"
excerpt: ""
weight: 2
---

Running `eunit` tests is as easy as:

```shell
$ rebar3 eunit 
```

Rebar3 will compile all project modules with the macros `{d, TEST, true}` and `{d, EUNIT, true}` defined so you can safely hide your test code within `-ifdef(TEST).` or `-ifdef(EUNIT).` sections. It will also automatically compile any source files in your application's `test` directory if present. It then runs tests by calling `eunit:test([{application, App}])` for each application in your project.

If you want to call `eunit:test/1` with your own arguments, you can use the `rebar.config` key `eunit_opts`. The arguments are the same as those for running `eunit` from the shell, as documented [here](http://www.erlang.org/doc/man/eunit.html). Rebar3 will do its best to ensure any modules specified in tests are compiled and made available on the code path.

You can also run only tests for specific apps:

```shell
$ rebar3 eunit --application=some_app,some_other_app 
```

Or for only specific modules:

```shell
$ rebar3 eunit --module=a,b,c 
```

Or for specific test files:

```shell
$ rebar3 eunit --file="test/my_tests.erl" 
```

Or specific directories:

```shell
$ rebar3 eunit --dir="test" 
```

The `eunit` command runs as the `test` profile by default. See [Profiles](/docs/configuration/profiles) for details.

For available options and their usage see [Commands](/docs/commands) or:

```shell
$ rebar3 help eunit 
```

