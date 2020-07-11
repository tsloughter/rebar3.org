---
title: "Building Plugins"
excerpt: ""
---

Rebar3's system is based on the concept of *[providers](https://github.com/tsloughter/providers)*. A provider has three callbacks:


- `init(State) -> {ok, NewState}`, which helps set up the state required, state dependencies, etc.

- `do(State) -> {ok, NewState} | {error, Error}`, which does the actual work.

- `format_error(Error) -> String`, which allows to print errors when they happen, and to filter out sensitive elements from the state.



A provider should also be an OTP Library application, which can be fetched as any other Erlang dependency, except for Rebar3 rather than your own system or application.



This document contains the following elements:



- [Using a Plugin](#section-using-a-plugin)

- [Reference](#section-reference)

  - [Provider Interface](#section-provider-interface)

  - [List of Possible Dependencies](#section-list-of-possible-dependencies)

  - [Rebar API](#section-rebar-api)

  - [Rebar State Manipulation](#section-rebar-state-manipulation)

  - [Namespaces](#section-namespaces)

- [Tutorial](#section-tutorial)

  - [First Version](#section-first-version)

  - [Optionally Search Deps](#section-optionally-search-deps)



## Using a Plugin ##



To use the a plugin, add it to the rebar.config:

	 {plugins, [
	  {plugin_name, {git, "git@host:user/name-of-plugin.git", {tag, "1.0.0"}}}
	]}. 
Then you can just call it directly:



```

→ rebar3 plugin_name

===> Fetching plugin_name

===> Compiling plugin_name

<PLUGIN OUTPUT>

```



## Reference ##



### Provider Interface ###



Each provider has the following options available:



- *name*: The 'user friendly' name of the task.

- *module*: The module implementation of the task.

- *hooks*: A two-tuple of provider names for pre and post-hooks (`{Pre, Post}`).

- *bare*: Indicates whether task can be run by users or not. Should be `true`.

- *deps*: The list of dependencies, providers that need to run before this one. You do not need to include the dependencies of your dependencies.

- *desc*: The description for the task, used by `rebar3 help`

- *short_desc*: A one line short description of the task, used in lists of providers

- *example*: An example of the task usage, such as `"rebar3 my-provider args"`

- *opts*: The list of options that the task requires/understands. The form of each option is `{Key, $Character, "StringName", Spec, HelpText}`, where:

  - `Key` is an atom, to be used to fetch the value later;

  - `$Character` is the short form of the option. So if the command is to be entered as a `-c Arg`, `$c` is the value of this field

  - `Spec` is either a type (`atom`, `binary`, `boolean`, `float`, `integer`, or `string`), a type with a default value (`{Type, Val}`), or the atom `undefined`.

- *profiles*: Profiles to use for provider. Default to `[default]`.

- *namespace*: namespace the provider is registered in. Defaults to `default`, which is the main namespace.



These options are to be added to the provider when creating it. 



A provider has the following implementation:

	 -module(provider_template).
	-behaviour(provider).
	
	-export([init/1, do/1, format_error/1]).
	
	
	%% ===================================================================
	%% Public API
	%% ===================================================================
	
	%% Called when rebar3 first boots, before even parsing the arguments
	%% or commands to be run. Purely initiates the provider, and nothing
	%% else should be done here.
	-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
	init(State) ->
	    Provider = providers:create([Options]),
	    {ok, rebar_state:add_provider(State, Provider)}.
	
	%% Run the code for the plugin. The command line argument are parsed
	%% and dependencies have been run.
	-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
	do(State) ->
	    {ok, State}.
	
	%% When an exception is raised or a value returned as
	%% `{error, {?MODULE, Reason}}` will see the `format_error(Reason)`
	%% function called for them, so a string can be formatted explaining
	%% the issue.
	-spec format_error(any()) -> iolist().
	format_error(Reason) ->
	    io_lib:format("~p", [Reason]). 
### List of Possible Dependencies ###



All dependencies are in the default namespace until indicated otherwise

[block:parameters]

{

  "data": {

    "h-0": "Name",

    "h-1": "Function",

    "h-2": "Profile",

    "0-0": "app_discovery",

    "0-1": "Explore user applications and loads their configuration.",

    "1-0": "clean",

    "1-1": "Remove compiled beam files from apps.",

    "1-2": "default",

    "4-0": "ct",

    "h-3": "Also depends on",

    "1-3": "app_discovery",

    "0-2": "default",

    "4-1": "Run common test suites.",

    "4-2": "test",

    "4-3": "compile",

    "5-0": "deps",

    "5-1": "List dependencies.",

    "5-2": "default",

    "5-3": "app_discovery",

    "6-0": "dialyzer",

    "7-0": "edoc",

    "6-1": "Run the Dialyzer analyzer on the project.",

    "6-2": "default",

    "6-3": "compile",

    "7-1": "Generate documentation using edoc.",

    "7-2": "default",

    "7-3": "app_discovery",

    "2-0": "compile",

    "2-1": "Compile apps .app.src and .erl files.",

    "2-2": "default",

    "2-3": "lock",

    "8-0": "eunit",

    "8-1": "Run EUnit Tests.",

    "8-2": "test",

    "8-3": "compile",

    "9-0": "help",

    "9-1": "Display a list of tasks or help for a given task or subtask.",

    "9-2": "default",

    "10-0": "install_deps",

    "10-1": "Download dependencies",

    "10-2": "default",

    "10-3": "app_discovery",

    "11-0": "lock",

    "11-1": "Lock dependencies and add rebar.lock",

    "11-2": "default",

    "11-3": "install_deps",

    "12-0": "new",

    "12-1": "Create new project from templates.",

    "12-2": "default",

    "13-0": "pkgs",

    "13-1": "List available packages.",

    "13-2": "default",

    "14-0": "release",

    "14-1": "Build release of project.",

    "14-2": "default",

    "14-3": "compile",

    "16-0": "shell",

    "16-1": "Run shell with project apps and deps in path.",

    "16-2": "default",

    "16-3": "compile",

    "17-0": "tar",

    "17-1": "Tar archive of release built of project.",

    "17-2": "default",

    "17-3": "compile",

    "18-0": "update",

    "18-1": "Update package index.",

    "18-2": "default",

    "19-0": "upgrade",

    "19-1": "Upgrade dependencies.",

    "19-2": "default",

    "20-0": "version",

    "20-1": "Print version for rebar and current Erlang.",

    "20-2": "default",

    "21-0": "xref",

    "21-1": "Run cross reference analysis",

    "21-2": "default",

    "21-3": "compile",

    "15-0": "report",

    "15-1": "Provide a crash report to be sent to the rebar3 issues page",

    "15-2": "default",

    "3-0": "cover",

    "3-1": "Analyze cover-compiled files",

    "3-2": "default",

    "3-3": "lock"

  },

  "cols": 4,

  "rows": 22

}

[/block]

* Note that you can depend on more than one provider, but they *must be in the same namespace*





### Rebar API ###



Rebar comes with a module called `rebar_api` exporting commonly needed functions when writing providers. Functions include:

[block:parameters]

{

  "data": {

    "h-0": "Function",

    "h-1": "Usage",

    "0-0": "`abort()`",

    "0-1": "Interrupts program flow",

    "1-0": "`abort(FormatString, Args)`",

    "1-1": "Interrupts program flow; allows to display an `ERROR` message along with it.\n\nEquivalent to calling `rebar_api:error(FormatString, Args)` followed by `rebar_api:abort()`",

    "2-0": "`console(FormatString, Args)`",

    "2-1": "Prints to the console.",

    "3-0": "`info(FormatString, Args)`",

    "3-1": "Logs with the severity `INFO`",

    "4-0": "`warn(FormatString, Args)`",

    "4-1": "Logs with the severity `WARNING`",

    "5-0": "`error(FormatString, Args)`",

    "5-1": "Logs with the severity `ERROR`",

    "7-0": "`expand_env_variable(InStr, VarName, RawVarValue)`",

    "7-1": "Given the env variable `FOO`, we want to expand all references to it in `InStr`.\n\nReferences can have two forms: `$FOO` and `${FOO}`. The form `$FOO` is delimited by whitespace characters or the end of a line (eol).",

    "8-0": "`get_arch()`",

    "9-0": "`wordsize()`",

    "9-1": "Returns the true wordsize of the emulator, i.e. the size of a pointer, in bytes as an string.",

    "8-1": "Returns the 'architecture' as a string of the form `\"$OTP_VSN-$SYSTEM_$ARCH-WORDSIZE`.\n\nFinal strings will look like `\"17-x86_64-apple-darwin13.4.0-8\"` or `\"17-x86_64-unknown-linux-gnu-8\"`",

    "10-0": "`add_deps_to_path(RebarState)`",

    "10-1": "The project's dependencies are added to the code path. Useful when a tool is invoked and needs to have global stateful access to libraries.",

    "11-0": "`restore_code_path(RebarState)`",

    "11-1": "Revert the code path to only include the libraries required to run Rebar3 and its plugins. This is the desired state for Rebar3 to avoid conflicts with user-provided tools.",

    "12-0": "`ssl_opts(Url)`",

    "12-1": "Returns the [ssl](https://hosting.review/web-hosting-glossary/#12) options to use with `httpc` to make a secure and verified HTTP request.",

    "6-0": "`debug(FormatString, Args)`",

    "6-1": "Logs with the severity `DEBUG`"

  },

  "cols": 2,

  "rows": 13

}

[/block]

Do note that all logging functions automatically add a new line (`~n`) to every expression logged.



### Rebar State Manipulation ###



The `State` argument passed to the plugin provider can be operated on with the `rebar_state` module through the following interface:

[block:parameters]

{

  "data": {

    "h-0": "Function",

    "h-1": "Usage",

    "0-0": "`get(State, Key, [DefaultValue]) -> Value`",

    "0-1": "When a `rebar.config` element is of the form `{Key, Value}.`, allows you to fetch the value for it",

    "1-0": "`set(State, Key, Value) -> *NewState*`",

    "1-1": "Adds a configuration value to the rebar state.",

    "2-0": "`lock(State) -> ListOfLocks`",

    "3-0": "`escript_path(State) -> Path`",

    "4-0": "`command_args(State) -> RawArgs`",

    "2-1": "Returns a list of locked dependencies",

    "3-1": "Returns the Rebar3 escript location",

    "4-1": "Returns the arguments passed to rebar3",

    "5-0": "`command_parsed_args(State) -> Args`",

    "5-1": "Returns the arguments passed to rebar3, parsed.",

    "6-0": "`deps_names(State) -> DepsNameList`",

    "7-0": "`project_apps(State) -> AppList`",

    "8-0": "`all_deps(State) -> DepsList`",

    "6-1": "Returns a list of dependencies' names",

    "7-1": "Returns a list of applications. These can be handled using [`rebar_app_info`](https://github.com/erlang/rebar3/blob/master/src/rebar_app_info.erl).",

    "8-1": "Returns a list of dependencies. These can be handled using [`rebar_app_info`](https://github.com/erlang/rebar3/blob/master/src/rebar_app_info.erl).",

    "10-0": "`add_resource(State, {Key, Module}) -> NewState`",

    "11-0": "",

    "10-1": "Registers a new resource type (such as `git`, `hg`, and so on) with the module used to handle it. The resource must implement the `rebar_resource` behaviour.\n\nTo be effective, this function must be called as part of a provider's `init/1` function.",

    "9-0": "`add_provider(State, Provider) -> NewState`",

    "9-1": "Registers a new provider, where `Provider` is the result of calling `providers:create(Options)`.\n\nTo be effective, this function must be called as part of a provider's `init/1` function. It can be called multiple times, allowing a plugin to register multiple commands."

  },

  "cols": 2,

  "rows": 11

}

[/block]



## Manipulate Application State

Each application being built (project applications and dependencies). All AppInfo records can be found in the State and accessed through `project_apps/1` and `all_deps/1`

[block:parameters]

{

  "data": {

    "h-0": "Function",

    "h-1": "Usage",

    "0-0": "`get(AppInfo, Key, [DefaultValue]) -> Value`",

    "1-0": "`set(AppInfo, Key, Value) -> *NewState*`",

    "0-1": "Fetch value of `Key` as defined for the application `AppInfo`",

    "1-1": "Adds a configuration value to the application's record"

  },

  "cols": 2,

  "rows": 2

}

[/block]

### Namespaces ###



For plugins that might require multiple commands all adapted to a single type of task (such as implementing a suite of tools for a BEAM language other than Erlang), rather than having multiple commands polluting the command space or requiring prefixes such as `rebar3 mylang_compile`, rebar3 introduces support for namespaces.



A plugin can be declared to belong to a given namespace. For example, the [ErlyDTL compiler plugin](https://github.com/tsloughter/rebar3_erlydtl_plugin) introduces the `compile` command under the `erlydtl` namespace. It can therefore be invoked as `rebar3 erlydtl compile`. If the `erlydtl` namespace had other commands such as `clean`, they could be chained as `rebar3 erlydtl clean, compile`.



In other ways, a namespace acts like `do` (`rebar3 do compile, edoc`), but operating on a non-default set of commands.



To declare a namespace, an provider needs only to use the `{namespace, Namespace}` option in its configuration list. The provider will automatically register the new namespace and be available under this term. 

{{% blocks/callout type="warning" title="Namespaces also apply to provider dependencies and hooks" %}}
 If a provider is part of a given namespace, its dependencies will be searched within that same namespace. Therefore if `rebar3 mytool rebuild` depends on `compile`, the `compile` command will be looked for in the `mytool` namespace.

To use the default `compile` command, the dependency must be declared as `{default, compile}`, or more generally `{NameSpace, Command}`.

The same mechanism is applied for hooks. 

{{% /blocks/callout %}}`

## Tutorial ##

### First version ###

In this tutorial, we'll show how to start from scratch, and get a basic plugin written. The plugin will be quite simple: it will look for instances of 'TODO:' lines in comments and report them as warnings. The final code for the plugin can be found on [bitbucket](https://bitbucket.org/ferd/rebar3-todo-plugin).

The first step is to create a new OTP Application that will contain the plugin:



    → rebar3 new plugin todo desc="example rebar3 plugin"

    ...

    → cd todo

    → git init

    Initialized empty Git repository in /Users/ferd/code/self/todo/.git/

The `src/todo.erl` file will be used to call call the initialization of all commands. For now we'll only have one `todo` command. Open up the `src/todo_prv.erl` file that will contain the command implementation, and make sure you have the following skeleton in place:

	 -module(todo_prv).
	-behaviour(provider).
	
	-export([init/1, do/1, format_error/1]).
	
	-define(PROVIDER, todo).
	-define(DEPS, [app_discovery]).
	
	%% ===================================================================
	%% Public API
	%% ===================================================================
	-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
	init(State) ->
	    Provider = providers:create([
	            {name, ?PROVIDER},          % The 'user friendly' name of the task
	            {module, ?MODULE},          % The module implementation of the task
	            {bare, true},               % The task can be run by the user, always true
	            {deps, ?DEPS},              % The list of dependencies
	            {example, "rebar provider_todo"}, % How to use the plugin
	            {opts, []}                  % list of options understood by the plugin
	            {short_desc, "example rebar3 plugin"},
	            {desc, ""}
	    ]),
	    {ok, rebar_state:add_provider(State, Provider)}.
	
	
	-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
	do(State) ->
	    {ok, State}.
	
	-spec format_error(any()) -> iolist().
	format_error(Reason) ->
	    io_lib:format("~p", [Reason]). 
This shows all the basic content needed. Note that we leave the `DEPS` macro to the value `app_discovery`, used to mean that the plugin should at least find the project's source code (excluding dependencies).



In this case, we need to change very little in `init/1`. Here's the new provider description:

	         Provider = providers:create([
	            {name, ?PROVIDER},       % The 'user friendly' name of the task
	            {module, ?MODULE},       % The module implementation of the task
	            {bare, true},            % The task can be run by the user, always true
	            {deps, ?DEPS},           % The list of dependencies
	            {example, "rebar todo"}, % How to use the plugin
	            {opts, []},              % list of options understood by the plugin
	            {short_desc, "Reports TODOs in source code"},
	            {desc, "Scans top-level application source and find "
	                   "instances of TODO: in commented out content "
	                   "to report it to the user."}
	    ]), 
Instead, most of the work will need to be done directly in `do/1`. We'll use the `rebar_state` module to fetch all the applications we need. This can be done by calling the `project_apps/1` function, which returns the list of the project's top-level applications.

	 do(State) ->
	    lists:foreach(fun check_todo_app/1, rebar_state:project_apps(State)),
	    {ok, State}. 
This, on a high level, means that we'll check each top-level app one at a time (there may often be more than one top-level application when working with releases)



The rest is filler code specific to the plugin, in charge of reading each app path, go read code in there, and find instances of 'TODO:' in comments in the code:

	 check_todo_app(App) ->
	    Paths = rebar_dir:src_dirs(rebar_app_info:dir(App), ["src"]),
	    Mods = find_source_files(Paths),
	    case lists:foldl(fun check_todo_mod/2, [], Mods) of
	        [] -> ok;
	        Instances -> display_todos(rebar_app_info:name(App), Instances)
	    end.
	find_source_files(Paths) ->
	    find_source_files(Paths, []).
	
	find_source_files([], Files) ->
	    Files;
	find_source_files([Path | Rest], Files) ->
	    find_source_files(Rest, [filename:join(Path, Mod) || Mod <- filelib:wildcard("*.erl", Path)] ++ Files).
	
	check_todo_mod(ModPath, Matches) ->
	    {ok, Bin} = file:read_file(ModPath),
	    case find_todo_lines(Bin) of
	        [] -> Matches;
	        Lines -> [{ModPath, Lines} | Matches]
	    end.
	
	find_todo_lines(File) ->
	    case re:run(File, "%+.*(TODO:.*)", [{capture, all_but_first, binary}, global, caseless]) of
	        {match, DeepBins} -> lists:flatten(DeepBins);
	        nomatch -> []
	    end.
	
	display_todos(_, []) -> ok;
	display_todos(App, FileMatches) ->
	    io:format("Application ~s~n",[App]),
	    [begin
	      io:format("\t~s~n",[Mod]),
	      [io:format("\t  ~s~n",[TODO]) || TODO <- TODOs]
	     end || {Mod, TODOs} <- FileMatches],
	    ok. 
Just using `io:format/2` to output is going to be fine.



To test the plugin, push it to a source repository somewhere. Pick one of your projects, and add something  to the rebar.config:

	 {plugins, [
	  {todo, {git, "git@bitbucket.org:ferd/rebar3-todo-plugin.git", {branch, "master"}}}
	]}. 
Then you can just call it directly:



```

→ rebar3 todo

===> Fetching todo

===> Compiling todo

Application merklet

    /Users/ferd/code/self/merklet/src/merklet.erl

      todo: consider endianness for absolute portability

```



Rebar3 will download and install the plugin, and figure out when to run it. Once compiled, it can be run at any time again.



### Optionally Search Deps ###



Let's extend things a bit. Maybe from time to time (when cutting a release), we'd like to make sure none of our dependencies contain 'TODO:'s either.



To do this, we'll need to go parse command line arguments a bit, and change our execution model. The `?DEPS` macro will now need to specify that the `todo` provider can only run *after* dependencies have been installed:

	 -define(DEPS, [install_deps]). 
We can add the option to the list we use to configure the provider in `init/1`:

	 {opts, [                 % list of options understood by the plugin
	    {deps, $d, "deps", undefined, "also run against dependencies"}
	]}, 


And then we can implement the switch to figure out what to search:

	 do(State) ->
	    Apps = case discovery_type(State) of
	        project -> rebar_state:project_apps(State);
	        deps -> rebar_state:project_apps(State) ++ lists:usort(rebar_state:all_deps(State))
	    end,
	    lists:foreach(fun check_todo_app/1, Apps),
	    {ok, State}.
	
	[...]
	
	discovery_type(State) ->
	    {Args, _} = rebar_state:command_parsed_args(State),
	    case proplists:get_value(deps, Args) of
	        undefined -> project;
	        _ -> deps
	    end. 
The `deps` option is found using `rebar_state:command_parsed_args(State)`, which will return a proplist of terms on the command-line after 'todo', and will take care of validating whether the flags are accepted or not. The rest can remain the same.



Push the new code for the plugin, and try it again on a project with dependencies:



```

===> Fetching todo

===> Compiling todo

===> Fetching bootstrap

===> Fetching file_monitor

===> Fetching recon

[...]

Application dirmon

    /Users/ferd/code/self/figsync/apps/dirmon/src/dirmon_tracker.erl

      TODO: Peeranha should expose the UUID from a node.

Application meck

    /Users/ferd/code/self/figsync/_deps/meck/src/meck_proc.erl

      TODO: What to do here?

      TODO: What to do here?

```



Rebar3 will now go pick dependencies before running the plugin on there.



you can also see that the help will be completed for you:



```

→ rebar3 help todo

Scans top-level application source and find instances of TODO: in commented out content to report it to the user.



Usage: rebar todo [-d]



```



That's it, the todo plugin is now complete! It's ready to ship and be included in other repositories.



### Adding More Commands ###



To add more commands to the same plugin, simply add entries to the `init` function in the main module:

	 -module(todo).
	
	-export([init/1]).
	
	-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
	init(State) ->
	    %% initialize all commands here
	    {ok, State1} = todo_prv:init(State),
	    {ok, State2} = todo_other_prv:init(State1),
	    {ok, State2}. 
And rebar3 will pick it up from there.
