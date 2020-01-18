---
title: "Commands"
excerpt: ""
---
#  Commands


Each command represents a task which runs one or more providers to fulfill the task.

## as

Higher order task which takes a profile name and list of tasks to run under that profile. 

## compile

After ensuring all dependencies are available, and fetching them if they are not, compile will compile the needed depdendencies and the project's apps .app.src and .erl files.

## clean

Removes compiled beam files from apps.



The clean command by default removes the beam files for top-level applications. It does so while respecting profiles, which means that 'rebar3 clean' will only clean the default profile, and 'rebar3 as test clean' will only clean the test profile.



[block:parameters]

{

  "data": {

    "0-1": "none",

    "h-0": "Option",

    "h-1": "Type",

    "h-2": "Description",

    "0-2": "Clean all apps, including the dependencies",

    "1-1": "string",

    "1-2": "Specify a profile (alternative to `rebar3 as <profile> clean`)"

  },

  "cols": 3,

  "rows": 2

}

[/block]



## ct

Runs common tests for the project located under the `test/` directory.



Most Common Test [options](http://www.erlang.org/doc/man/ct_run.html) as described in the Erlang documentation for `ct_run` are available. Some common ones are described below:

[block:parameters]

{

  "data": {

    "h-0": "Option",

    "h-1": "Type",

    "h-2": "Description",

    "0-1": "Comma separated list of strings",

    "0-2": "Compile and run all test suites in the specified directories.",

    "1-1": "Comma separated list of strings",

    "1-2": "Compile and run all test suites specified. Must be specified by full path, either absolute or relative to the current directory.",

    "2-1": "Comma separated list of strings",

    "2-2": "Test groups to run. See the [Common Test Documentation](http://www.erlang.org/doc/apps/common_test/index.html).",

    "14-1": "Comma separated list of strings",

    "14-2": "Config files to use when running tests. See the [Common Test Documentation](http://www.erlang.org/doc/apps/common_test/index.html).",

    "18-1": "String",

    "18-2": "The directory in which test logs will be written. See the [Common Test Documentation](http://www.erlang.org/doc/apps/common_test/index.html). Default: `_build/test/logs`",

    "21-1": "Boolean",

    "21-2": "Enable verbose output. Default: false",

    "23-1": "Boolean",

    "23-2": "Generate cover data",

    "3-1": "Comma separated list of strings",

    "3-2": "List of test cases to run. See the [Common Test Documentation](http://www.erlang.org/doc/apps/common_test/index.html).",

    "25-1": "String",

    "25-2": "Set a test label",

    "4-1": "Comma separated list of strings",

    "4-2": "List of [Test Specifications](http://erlang.org/doc/apps/common_test/run_test_chapter.html#test_specifications)",

    "5-1": "Comma separated list of strings",

    "15-2": "Allow user defined config values in config files. See the [Common Test Documentation](http://www.erlang.org/doc/apps/common_test/index.html).",

    "15-1": "Boolean",

    "19-1": "Comma separated list of strings",

    "19-2": "Set common test logging options. See the [Common Test Documentation](http://www.erlang.org/doc/apps/common_test/index.html). Default: `_build/test/logs`",

    "22-1": "Integer",

    "22-2": "Set the level of Common Test verbosity",

    "20-1": "Boolean",

    "20-2": "Adds test names with results  on a per-test basis, and only displays common-test logs in the terminal on failing tests. Default: true",

    "24-1": "String",

    "24-2": "Change the name of the code coverage file",

    "6-1": "Integer",

    "6-2": "How often to repeat the tests",

    "7-1": "String (format: HHMMSS)",

    "7-2": "Max allowed duration of the test run",

    "8-1": "String (format: HHMMSS)",

    "8-2": "Time until which to run the tests",

    "9-1": "`true | false | skip_rest`",

    "9-2": "Force termination on test timeout",

    "26-1": "Boolean",

    "26-2": "show basic HTML",

    "27-1": "String",

    "27-2": "CSS stylesheet to apply to HTML output",

    "16-1": "String",

    "16-2": "If the configuration file is encrypted, set the key to decrypt it",

    "17-1": "String",

    "17-2": "If the configuration file is encrypted, point to the file containing the key to decrypt it",

    "12-1": "Boolean",

    "12-2": "Abort the test run if a test suite is missing (Default: true)",

    "10-1": "Integer",

    "10-2": "Extends the timeout values for tests by a given multiplier value",

    "11-1": "Boolean",

    "11-2": "Enables automatic timeout value scaling, when using code coverage or tracing",

    "28-1": "`auto_per_run | auto_per_tc | manual_per_tc`",

    "28-2": "change the behaviour of the private (scratch) directories creation done by Common Test",

    "29-1": "String",

    "29-2": "Additional directories containing include files. Option added for parity with ct_run, usually rebar3 should take care of include file paths",

    "30-1": "String",

    "30-2": "Start a distributed node with a given name",

    "31-1": "String",

    "31-2": "Set a value for the distributed cookie",

    "13-1": "String",

    "13-2": "List of OTP application config files (like `sys.config`) that should be applied by Rebar3 before the test run.",

    "32-1": "Boolean",

    "32-2": "Compile the project with the test configuration specified, but without running the tests"

  },

  "cols": 3,

  "rows": 33

}

[/block]

Runs in the `test` profile.

## cover

Performs coverage analysis on modules called by Common Test or Eunit test suites. Call as `rebar3 do ct, cover`, `rebar3 do eunit, cover` or the combination of both with `rebar3 do eunit, ct, cover` while the `{cover_enabled, true}` option is in your rebar config file or if the cover flags were used with these commands individually.



An HTML report is generated.

[block:parameters]

{

  "data": {

    "h-0": "Option",

    "h-1": "Type",

    "h-2": "Description",

    "0-1": "none",

    "0-2": "Resets all cover data",

    "1-1": "none",

    "1-2": "Prints coverage analysis in the terminal."

  },

  "cols": 3,

  "rows": 2

}

[/block]

Specific modules can be blacklisted from code coverage by adding `{cover_excl_mods, [Modules]}` to the config file. Specific applications can be blacklisted by adding `{cover_excl_apps, [AppNames]}` to the config file.

## deps

Lists dependencies, whether they're source or package dependencies, and whether they're locked or not. Those that are locked but not matching the lock file are followed by an asterisk (`*`)

## do

Higher order provider for running multiple tasks in a sequence, separated by commas. Example: `rebar3 do a, b, c`

## dialyzer

Builds and keeps up-to-date a suitable PLT, and uses it to carry out success typing analysis on the current project.

[block:parameters]

{

  "data": {

    "h-0": "Option",

    "h-1": "Type",

    "h-2": "Description",

    "0-1": "boolean",

    "0-2": "Enable updating the PLT. Default: true",

    "1-1": "boolean",

    "1-2": "Enable success typing analysis. Default: true",

    "h-3": "Default",

    "0-3": "true",

    "1-3": "true"

  },

  "cols": 4,

  "rows": 2

}

[/block]

For instructions on suppressing warnings see the [Requesting or Suppressing Warnings in Source Files](http://erlang.org/doc/man/dialyzer.html) section of the Dialyzer documentation.



PLT files are named `<prefix>_<otp_release>_plt`; The base PLT is a PLT containing the core applications often required for a project's PLT. One base PLT is created per OTP version and stored in `base_plt_location`. A base PLT is then used to build project PLTs.



The following (optional) configurations can be added to a `proplist` of options `dialyzer` in rebar.config:

[block:parameters]

{

  "data": {

    "h-0": "Option",

    "h-1": "Description",

    "0-0": "`warnings`",

    "0-1": "a list of dialyzer warnings",

    "1-0": "`get_warnings`",

    "1-1": "display warnings when altering a PLT file (boolean)",

    "2-1": "the strategy for determining the applications which are included in the PLT file, `top_level_deps` to include just the direct dependencies or `all_deps` to include all nested dependencies (the direct dependent applications are listed in `applications` and `included_applications` of their .app files.)",

    "2-0": "`plt_apps`",

    "3-0": "`plt_extra_apps`",

    "3-1": "a list of applications to include in the PLT file (the applications in `base_plt_apps` will already be in the list)",

    "4-0": "`plt_location`",

    "4-1": "the location of the PLT file, `local` to store in the profile's base directory (default) or a custom directory.",

    "5-0": "`plt_prefix`",

    "5-1": "the prefix to the PLT file, defaults to \"rebar3\"",

    "6-0": "`base_plt_apps`",

    "6-1": "a list of applications to include in the base PLT file",

    "7-0": "`base_plt_location`",

    "7-1": "the location of base PLT file, `global` to store in $HOME/.cache/rebar3 (default) or  a custom directory",

    "8-0": "`base_plt_prefix`",

    "8-1": "the prefix to the base PLT file, defaults to \"rebar3\""

  },

  "cols": 2,

  "rows": 9

}

[/block]



## edoc

Generates documentation using doc.

Runs in the `docs` profile.

## escriptize

Generates an [escript](http://www.erlang.org/doc/man/escript.html) executable containing the project's and its dependencies' BEAM files.

[block:parameters]

{

  "data": {

    "h-0": "Config Option",

    "h-1": "Type",

    "h-2": "Description",

    "2-0": "escript_incl_apps",

    "2-1": "list of atoms",

    "2-2": "List of applications to include in the escript archive aside from the main app and its dependencies (from the app file). Defaults to `[]`",

    "0-0": "escript_main_app",

    "0-1": "atom",

    "0-2": "Name of the application to turn to an escript. Defaults to the top-level app if there is only one.\n\nWhen using an umbrella repository (with multiple top-level apps), this value *must* be specified.",

    "1-0": "escript_name",

    "1-1": "string",

    "1-2": "Name of the generated escript, and default module name to boot (`Module:main(_)`).\nDefaults to the value for `escript_main_app`",

    "3-0": "escript_emu_args",

    "3-1": "string",

    "3-2": "Escript emulator arguments (after `%%!` in escript declarations). \n\nThe string must begin with `%%!` and end with a line break. An example string would be `\"%%! +sbtu +A0\\n\"`.\n\nThe Default value is `\"%%! -escript main MainApp\\n\"`",

    "4-0": "escript_shebang",

    "4-1": "string",

    "4-2": "Location of escript file to run. Defaults to `\"#!/usr/bin/env escript\\n\"`. The end of line marker must be included in the string.",

    "5-0": "escript_comment",

    "5-1": "string",

    "5-2": "Arbitrary comment to put into the generated escript. Must include a newline marker at the end.\n\nDefaults to `%%\\n`."

  },

  "cols": 3,

  "rows": 6

}

[/block]

To override the default module name for the escript (which is expected to be the same as the `escript_name`), add `-escript main Module` to `escript_emu_args`



Example escript configuration from `relx`:

	 {escript_emu_args, "%%! +sbtu +A0 -noinput\n"}.
	{escript_incl_apps, [getopt, erlware_commons, bbmustache, providers, relx]}.
	 


## eunit

Runs eunit tests on project apps.

[block:parameters]

{

  "data": {

    "h-0": "Option",

    "h-1": "Type",

    "h-2": "Description",

    "0-1": "Boolean",

    "0-2": "Generate cover data",

    "1-1": "Boolean",

    "1-2": "Verbose output",

    "2-1": "Comma separated list of strings",

    "2-2": "List of applications to run tests for. Equivalent to EUnit's `[{application, App}]`.",

    "3-1": "Comma separated list of strings",

    "3-2": "List of test suites to run. Equivalent to EUnit's `[{module, Suite}]`.",

    "4-1": "Comma separated list of strings",

    "4-2": "List of files to run (such as `test/my_tests.beam`), equivalent to Eunit's `[{file, File}]`."

  },

  "cols": 3,

  "rows": 5

}

[/block]

Runs in the `test` profile.

## get-deps



## ! Not Required !

	 Unlike rebar2 this command is not required for fetching dependencies. The compile command will result in dependencies being fetched and then built if they aren't already. This command is useful if you have a specific use case that requires fetching dependencies separate from compilation. 

Fetch project dependencies.

## help

Displays a list of tasks or help for a given task or subtask.

[block:parameters]

{

  "data": {

    "h-0": "Option",

    "h-1": "Type",

    "h-2": "Description",

    "0-0": "<task>",

    "0-1": "string",

    "0-2": "Task to print help for.",

    "1-0": "<namespace> <task>",

    "1-1": "string",

    "1-2": "Task within `<namespace>` to print help for"

  },

  "cols": 3,

  "rows": 2

}

[/block]



## new

Creates a new project from templates. See a list of available templates by providing no arguments.

[block:parameters]

{

  "data": {

    "h-0": "Option",

    "h-1": "Type",

    "h-2": "Description",

    "0-2": "Overwrite existing files.",

    "0-1": "none",

    "1-0": "help <template>",

    "1-1": "none",

    "1-2": "Display all variables and arguments for each template"

  },

  "cols": 3,

  "rows": 2

}

[/block]



## path

Print paths to build dirs in current profile.

[block:parameters]

{

  "data": {

    "0-2": "Comma separated list of applications to return paths for.",

    "1-2": "Return the `base` path of the current profile.",

    "0-1": "Comma separated list of strings",

    "1-1": "none",

    "2-2": "Return the `bin` path of the current profile.",

    "2-1": "none",

    "3-2": "Return all `ebin` paths of the current profile's applications.",

    "3-1": "none",

    "4-2": "Return the `lib` path of the current profile.",

    "4-1": "none",

    "5-2": "Return the `priv` path of the current profile.",

    "6-2": "In case of multiple return paths, the separator character to use to join them.",

    "6-1": "string",

    "5-1": "none",

    "7-2": "Return the `src` path of the current profile's applications.",

    "8-2": "Return the `rel` path of the current profile.",

    "7-1": "none",

    "8-1": "none"

  },

  "cols": 3,

  "rows": 9

}

[/block]



## pkgs

Lists available packages.

## release

Builds release of project. Call `rebar3 help release` for arguments.

## relup

Creates a relup from 2 releases. Call `rebar3 help relup` for arguments.

## report

Generates contextual data to include in bug reports



## shell

Runs a shell with project apps and deps in path.



The shell booted with this command has an agent running allowing to run rebar3 commands dynamically, such as `r3:compile()` or `r3:upgrade()`, and have new modules automatically reloaded. Specific namespaces can be reached by calling `r3:do(Namespace, Command)`. No arguments can be passed to these commands.

[block:parameters]

{

  "data": {

    "h-0": "Option",

    "h-1": "Type",

    "h-2": "Description",

    "0-1": "string",

    "0-2": "Allows to load a [config file](http://www.erlang.org/doc/man/config.html), if any.\n\nDefaults to the `sys_config` entry defined for `relx` if present.",

    "1-1": "atom",

    "1-2": "Starts the node in network mode. Equivalent to `erl`'s `-name` and `-sname` options.",

    "2-1": "string",

    "2-2": "Sets the cookie for a distributed node. Equivalent to `erl`'s `-setcookie` option",

    "3-1": "string",

    "3-2": "path to an escript to be evaluated before applications are started",

    "4-1": "string",

    "4-2": "Comma-separated list of application names to be booted. \n\nDefaults to the apps in the relx release if present.",

    "6-1": "atom",

    "6-2": "If multiple releases are present, specify which one to pick",

    "7-1": "string",

    "7-2": "If multiple releases are present, specify which version to use",

    "5-2": "When specified, no apps are booted by the shell; useful to override release or shell tuple configurations in rebar.config"

  },

  "cols": 3,

  "rows": 8

}

[/block]



## tar

Builds a compressed tar archive of release built of project. Call `rebar3 help tar` for arguments.

## tree

Prints a tree of dependencies and transitive dependencies of the project.

## lock

Get unbuilt dependencies to be added to the `rebar.lock` file. They will just have been downloaded, but none of their build script should have run. Though this is not necessarily true with pre/post hooks and dep plugins.

## unlock

Unlocks dependencies. If no dependency is mentioned, the command unlocks all of them. If any specific top-level dependencies (separated by commas) are listed as argument, those are unlocked. 



A new lock file is then generated, or the existing lock file is removed in case no locks remain.



This command should be used when one or more dependencies have been taken out of rebar.config, but remain in the lock file.

## update

Updates the package index.

## upgrade

Upgrades dependencies and updates the lock file accordingly.

[block:parameters]

{

  "data": {

    "h-0": "Option",

    "h-1": "Type",

    "h-2": "Description",

    "0-0": "<dependency>",

    "0-1": "string",

    "0-2": "Dependencies to upgrade (comma-separated).\nIf no dependency is mentioned, all dependencies are upgraded."

  },

  "cols": 3,

  "rows": 1

}

[/block]



## version

Prints version for rebar3 and current Erlang.

## xref

Runs cross reference analysis.
