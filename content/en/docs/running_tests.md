---
title: "Running Tests"
excerpt: ""
weight: 7
---

Rebar3 has built in `eunit` and `ct` (`common_test`) test runners. By following a few conventions you can run test suites with a single Rebar3 command.

## Eunit

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

The `eunit` command runs as the `test` profile by default. See [Profiles](/docs/profiles) for details.

For available options and their usage see [Commands](/docs/commands) or:

```shell
$ rebar3 help eunit 
```

## Common Test

To run `common_test` suites:

```shell
$ rebar3 ct 
```

Rebar3 will look in all your applications' `test` directories and compile and run any source files named `*_SUITE.erl`.

To run only specific test suites:

```shell
$ rebar3 ct --suite=test/first_SUITE,test/second_SUITE 
```

Rebar3 has a built in `common_test` runner that supports most test suites and `common_test` options. If your test suites require use of test specs or cover specs be aware Rebar3 keeps seperate build artifacts for each profile so you may need to adjust paths to point to the modules and directories in the relevant profile directory under `_build` for them to work as expected. If you need to use an unsupported `common_test` option the following command can be used to run `common_test` with the path to the compiled beam files Rebar3 generates

```shell
$ ct_run -pa `rebar3 path` ... 
```

The `ct` command runs as the `test` profile by default. See [Profiles](/docs/profiles) for details.

For available options and their usage see [Commands](/docs/commands) or:

```shell
$ rebar3 help ct 
```

## Code Coverage

The test runs of all built-in test tools will generate cover data. Calling `rebar3 cover` at any later point will generate a general code coverage report by merging all the individual reports:

```shell
$ rebar3 ct --dir test/suites1 --cover --cover_export_name=suites1
===> Running Common Test suites...
...
$ rebar3 ct --dir test/suites2 --cover --cover_export_name=suites2
===> Running Common Test suites...
...
$ ls _build/test/cover
cover.log    suite1.coverdata    suite2.coverdata
$ rebar3 cover --verbose
===> Performing cover analysis...
  |----------------------------|------------|
  |                    module  |  coverage  |
  |----------------------------|------------|
  |                     ....   |       Y%   |
  |----------------------------|------------|
  |                     total  |       X%   |
  |----------------------------|------------|
  coverage calculated from:
    _build/test/cover/suites1.coverdata
    _build/test/cover/suites2.coverdata
  cover summary written to: _build/test/cover/index.html
```
