---
title: "Releases"
excerpt: ""
weight: 8
---

{{% blocks/callout type="info" title="What are releases and target systems anyway?" %}}
A release is the set of applications needed for booting an Erlang VM and starting your project. This is described through a release resource file (`.rel`) which is used to generate a `.script` and `.boot`. The boot file is the binary form of the script file and is what is used by the Erlang Run Time System (ERTS) to start an Erlang node, sort of like booting an operating system. Even running `erl` on the command line is using a boot script.

A target system is an Erlang system capable of being booted on another machine (virtual or otherwise), often ERTS is bundled along with the target system.

For more information checkout the [chapter on releases](https://adoptingerlang.org/docs/production/releases/) from _Adopting Erlang_.

{{% /blocks/callout %}}

## Getting Started

Add a `relx` section to your project's `rebar.config`:

```erlang
{relx, [{release, {<release name>, "0.0.1"},
         [<app>]},
        {release, {<release name>, "0.1.0"},
         [<app>]},

        {dev_mode, true},
        {include_erts, false},

        {extended_start_script, true}]}.
```

Running `rebar3 release` will build the release and provide a script for starting a node under `_build/<profile>/rel/<release name>/bin/<release name>`.

`<release_name>` must be an atom, same for each `<app>` in the list of applications to include in the release.

`<vsn>` can be one of:

| Version type | Result |
| --------------------- | ------------------------------------------------------------- |
| `string()` | A string is used as is for the version. Example: `"0.1.0"`|
| `git | semver`  | Uses the latest git tag on the repo to construct the version. |
| `{cmd, string()}`     | Uses the result of executing the contents of `string()` in a shell. Example to use a file `VERSION`: `{cmd, "cat VERSION | tr -d '[:space:]'"}` |
| `{git, short | long}` | Uses either the short (8 characters) or the full git ref of the current commit. |
| `{file, File}` | Uses the content of a file. For example, a better way to use a `VERSION` file than using `cmd`: `{file, "VERSION"}` |

You can add multiple `release` sections to your project's `rebar.config` under `relx`. 

You can either just specify different releases sharing the same configuration:

```erlang
{relx, [{release, {<release name>, "0.0.1"},
         [<app>]},         
          
        {release, {<other release name>, "0.1.0"},
         [<app>]},
         
         {dev_mode, false},
         {include_erts, true},
       ]}.
```

Or you can also specify releases with independent configurations by using a
4-tuple `release` definition:

```erlang
{relx, [{release, {<release name>, "0.0.1"},
         [<app>],
         [{dev_mode, false},
          {include_erts, true}]},
        {release, {<release name>, "0.1.0"},
         [<app>],
         [{dev_mode, true}]}
       ]}.
```
You can build specific releases using `rebar3 release -n <release_name>`

## Modes

Other modes include `prod` and `minimal`:

```erlang
{relx, [...
        {mode, <mode>},
        ...
       ]
}.
```

| Mode | Expanded Options |
| --------------------- | ------------------------------------------------------------- |
| `dev` | `[{dev_mode, true}, {include_src, true}, {debug_info, keep}, {include_erts, false}]` |
| `prod`  | `[{include_src, false}, {debug_info, strip}, {include_erts, true}, {dev_mode, false}]` |
| `minimal` | `[{include_src, false}, {debug_info, strip}, {include_erts, false}, {dev_mode, false}]` |

While developing you'll likely want all your changes to applications be
immediately available in the release. `relx` provides multiple modes, including
a mode `dev` for this particular use case. Instead of copying the applications
that make up the release to the release structure it creates symlinks, so
compiling and restarting or loading the changed modules, is all that is necessary.

The `prod` mode expands to options that are commonly used for production
releases: `include_src` is `false` so source code is not included in the release,
`debug_info` is set to `strip` which makes BEAM files smaller and only removes
data that is used by tools you more than likely aren't including in your
release, and sets `include_erts` to `true` to bundle in the current Erlang
runtime, making a release you can copy to a compatible target and run without
first installing Erlang.

The `minimal` mode is the same as `prod` except it does not include the Erlang
runtime.

You can override options that the modes expand to by including explicit setting
them. For example, if you did want to keep the debug info in the BEAM modules
then you can use a configuration like:

```erlang
[
  {mode, prod},
  {debug_info, keep}
]
```

## Verification Checks

### Missing Functions

The `exref` option to relx will utilize
[xref](https://erlang.org/doc/apps/tools/xref_chapter.html) to validate that
functions and modules called in a release exist:

```erlang
{exref, true}
```

### Stale Modules

The option `src_tests` will issue a warning if the source code for a module is
missing or is newer than the object code:

```
{src_tests, true}
```

This is useful to catch any modifications to dependency source files. Since
`rebar3 release` will automatically compile all changes to the Applications in
your project the dependencies should be the only modules that could possibly be
stale.

## Configuration

## VM Configuration

By default `relx` will give a basic `vm.args` file that sets a node name and cookie. For a complete list of options and their use check the [Erlang documentation](http://erlang.org/doc/man/erl.html).

```erlang
## Name of the node
-name {{release_name}}@127.0.0.1

## Cookie for distributed erlang
-setcookie {{release_name}}
```
To provide a custom `vm.args`, usually placed in the `config/` directory at the root of your project, add this line to the `relx` section of your `rebar.config`:

```erlang
{vm_args, "config/vm.args"} 
```

## Application Configuration

For passing Application configuration at release runtime there is `sys.config` and `sys.config.src`:

```erlang
[
  {<app_name>, [{<key>, <val>}, ...]}
].
```

If either file `config/sys.config.src` or `config/sys.config` exists in the
project then `relx` will automatically include one of them (`.src` takes
precedence if both exist) in the release.

To set a specific file to use as the Application configuration file it can be
set with either `sys_config` or `sys_config_src`:

```erlang
{sys_config, "config/sys_prod.config"} 
```

```erlang
{sys_config_src, "config/sys_prod.config.src"} 
```

The files will be renamed to `sys.config` or `sys.config.src` when included into
the release.

If none exists then a file with an empty list is used.

Read more about Erlang configuration in the [config
docs](http://erlang.org/doc/man/config.html) and in the [systools
docs](https://erlang.org/doc/man/systools.html).

## Dynamic Configuration

##  With OTP-21+ and rebar3 3.6+

Starting with Erlang/OTP 21 and rebar3 3.6.0 the configuration options
`sys_config_src` and `vm_args_src` are available for explicitly including
templates that will be rendered at runtime, substituting variables defined
as `${VARIABLE}` with their equivalent value in the shell environment.

As of rebar3 3.14.0 the configs will be included if they exist, so only if the
files are not named `config/sys.config.src` and `config/vm.args.src` do you
need to include `{sys_config_src, <filename>}` or `{vm_args_src, <filename>}`
in the relx config.

*sys.config.src*
```erlang
[
  {appname, [{port, ${PORT}}]}
].
```

*vm.args.src*
```erlang
-name ${NODE_NAME} 
```

*rebar.config*
```erlang
{relx, [{release, {<release name>, "0.0.1"},
         [<app>]},

        {mode, dev}]}.
```

There is no need to set `RELX_REPLACE_OS_VARS=true` when using `.src` files for
configuration. In the following section we'll see older forms of runtime
configuration.

## Before OTP-21 and rebar3 3.6

By setting `RELX_REPLACE_OS_VARS=true` both `vm.args` and `sys.config` files may contain OS environment variables that will be replaced with the current value from the environment the node is started in. This means a `vm.args` and `sys.config` for a release that starts a web server listening on a port could look like:

*vm.args*
```erlang
-name ${NODE_NAME} 
```

*sys.config*
```erlang
[
 {appname, [{port, "${PORT}"}]}
].
```

And then be used to start multiple nodes of the same release with different name.

```shell
#!/bin/bash

export RELX_REPLACE_OS_VARS=true

for i in `seq 1 10`;
do
    NODE_NAME=node_$i PORT=808$i _build/default/rel/<release>/bin/<release> foreground &
    sleep 1
done
```
## Overlays

Overlays provide the ability to define files and templates to include in the target system. For example, custom scripts for managing your node or the Procfile needed for running on Heroku. 

```erlang
{relx, [
    ...
    {overlay_vars, "vars.config"},
    {overlay, [{mkdir, "log/sasl"},
               {template, "priv/app.config", "etc/app.config"},
               {copy, "Procfile", "Procfile"}]}
]}.
```

The supported actions are:

- `mkdir` to create a directory within the release

- `copy` to copy a file from a local directory to a location within the release

- `template` to behave the way `copy` does, but with variable expansion in it.

Relx's templating exposes variables along with the full power of a Mustache templating system (see [mustache](https://github.com/soranoba/mustache)). You can look at the documentation there for the full syntax supported. 

There is a set of variables made available by default which are described in the next session, and custom variables can otherwise be declared in the file specified in `{overlay_vars, "vars.config"}`, which should have the following format:

```
%% some variables
{key, value}.
{other_key, other_val}.
%% includes variables from another file
"./some_file.config". 
```
    
The default variables are defined below.

## Predefined variables

* *log* : The current log level in the format of `(<logname>:<loglevel>)`

* *output_dir* : The current output directory for the built release

* *target_dir* : The same as `output_dir`, exists for backwards compatibility

* *overridden* : The current list of overridden apps (a list of app names)

* *goals* : The list of user specified goals in the system

* *lib_dirs* : The list of library directories, both user specified and derived

* *config_file* : The list of config file used in the system

* *providers* : The list of provider names used for this run of relx

* *sys_config* : The location of the sys config file

* *root_dir* : The root dir of the current project

* *default_release_name* : The current default release name for the relx run

* *default_release_version* : The current default release version for the relx run

* *default_release* : The current default release for the relx run

* *release_erts_version* :  The version of the Erlang Runtime System in use

* *erts_vsn* : The same as `release_erts_version` (for backwards compatibility)

* *release_name* : The currently executing release

* *release_version* : the currently executing version

* *rel_vsn* : Same as `release_version`. Exists for backwards compatibility

* *release_applications* : A list of applications included in the release

## Splitting configurations

It is possible to split overlay files to deal with more complex situations. To explain this lets look at the a simplified example:

We build our application and want to distinguish between production and developing builds by having the overlay variable `build` spell out either `"prod"` or `"dev"` so the app.config file could include it in it's configuration and we can either enable or disable features.

For this we build three overlay files:

* a `dev.config` - that is dev branch specific

* a `prod.config` - that is prod branch specific

* a `base.config` - that is not branch specific

For dev builds we will use `dev.config` as `overlay_vars` and for prod we will be using `prod.config`.

*base.config*
```erlang
{data_dir, "/data/yolo_app"}.
{version, "1.0.0"}.
{run_user, "root"}.
```

*dev.config*
```erlang
%% Include the base config
"./base.config".
%% The build we have
{build, "dev"}.
```

*prod.config*
```erlang
%% Include the base config
"./base.config".
%% The build we have
{build, "prod"}.
```

## Deployable

To build a release capable of being copied to other nodes we must turn off `dev_mode` so applications are copied to the release `lib` dir instead of being symlinks.

```shell
$ rebar3 release -d false 
```

Or create a profile that turns off `dev_mode` by setting the `relx` mode to `prod`:

```erlang
{profiles, [{prod, [{mode, prod}]}]}]}. 
```

## Target System

A target system can not have symlinks like those created when using `dev_mode` and often we want to include ERTS along with the system so it does not need to be previously installed on the target.

Rebar3 will automatically add `{mode, prod}` to the relx configuration if the
`prod` profile is used to build the release. For example:

```shell
$ rebar3 as prod tar
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling relx_overlays
===> Assembling release myrel-0.1.0...
===> Release successfully assembled: _build/prod/rel/myrel
===> Building release tarball myrel-0.1.0.tar.gz...
===> Tarball successfully created: _build/prod/rel/myrel/myrel-0.1.0.tar.gz
```

Now a tarball `myrel-0.1.0.tar.gz` can be copied to another compatible system and booted:

```shell
$ mkdir myrel
$ mv myrel-0.1.0.tar.gz myrel/
$ cd myrel
$ tar -zxvf myrel-0.1.0.tar.gz
$ bin/myrel console
```

## Without ERTS

When it is required to leave ERTS out of a release the `prod` profile
configuration can be set in `rebar.config` under `profiles`. For example, to use
the ERTS and base applications like `kernel` and `stdlib` on the target, set
`mode` to `minimal` and `system_libs` to `false` in the `relx` configuration tuple:

```erlang
{profiles, [{prod, [{relx, [{mode, minimal},
                            {system_libs, false}]}]}]}.
```

Or manually set `include_erts` to `false`:

```erlang
{profiles, [{prod, [{relx, [{include_erts, false},
                            {system_libs, false}]}]}]}
```

Now when running `rebar3 as prod tar` the generated tarball will not include
ERTS or Applications like `kernel` and `stdlib`.

## Source Code Inclusion in Release

By default the release will include source files of your applications, if present.

If you don't want to include the source files, set `include_src` to false.


```erlang
{include_src, false} 
```

## Application exclusions

The following allows you to remove specific applications from the output release.

```erlang
{exclude_apps, [app1, app2]} 
```

The Applications will be removed from the `.app` file of any Application in the
release that had them listed under `applications`.

## Module exclusions

The following directive allows you to remove application modules from the output release.

```erlang
{exclude_modules, [
    {app1, [app1_mod1, app1_mod2]},
    {app2, [app2_mod1, app2_mod2]}
]}.
```

The modules will be removed from the Application's `.app` file's `module` list. 

## Cross Compiling

If you wish to include an Erlang Run Time System that is not the version you are using to run `rebar3`, for example you are building on MacOSX but wish to include an ERTS that was built for a version of GNU/Linux, you can supply a path instead of a boolean for `include_erts` and provide a path for `system_libs`, still within the `relx` configuration tuple:

```
{include_erts, "/path/to/erlang"},
{system_libs, "/path/to/erlang"},

```
Using these paths with profiles can yield easier ways to set up cross-compiling.

## Hooks

It is possible to define hooks on specific operations of the extended start script, the operations are `start`, `stop`, `install_upgrade`, `pre` and `post` hooks for each of these operations are available.

The hooks can either be builtin (ie. they are already included in the release) or custom (scripts written by the user for custom functionality), the builtin scripts offer pre-packaged functionality, they are:

* *pid* : Writes the beam pid to a configurable file location

(`/var/run/<rel_name>.pid` by default).

* *wait_for_vm_start* : Waits for the vm to start (ie. when it can be pinged).

* *wait_for_process* : Waits for a configurable name to appear in the Erlang process registry.

```erlang
{extended_start_script_hooks, [
  {pre_start, [{custom, "hooks/pre_start"}]},
  {post_start, [
    {pid, "/tmp/foo.pid"},
    {wait_for_process, some_process},
    {custom, "hooks/post_start"}
  ]},
  {pre_stop, [
    {custom, "hooks/pre_stop"}]},
    {post_stop, [{custom, "hooks/post_stop"}]},
  ]},
  {post_stop, [{custom, "hooks/post_stop"}]}
]}.
```

## Extensions

The extended start script that is generated comes with a builtin set of commands that allows you to manage your release, these are `foreground`, `stop`, `restart`, etc.

Sometimes it's useful to expose some custom commands that are specific to your application. For example if you're running a game server it would be convenient to just call `bin/gameserver games` that outputs useful information.

Extended start script extensions allow you to create a custom shell script that gets appended to the list of commands available to your start script. The extension shell script can take arguments and has access to all shell variables defined in the start script itself. You begin by defining the extension in your `rebar.config`, for example:

```erlang
% start script extensions
{extended_start_script_extensions, [
   {status, "extensions/status"}
]}.

```

Here you are adding the `status` script extension that will invoke an `extensions/status` shell script.

This path is relative to the location of the start script on the generated release so you probably will want to use a `overlay` to place it at the correct location:

```erlang
{copy, "scripts/extensions/status", "bin/extensions/status"},
```

The extension script itself is standard shell script, the game server example described could be implemented in the following way:

```shell
#!/bin/bash

case $1 in
    help)
        echo "bin/gameserver status"
        ;;
    *)
        ;;
esac

# get the status tuple from gameserver
Status=$(relx_nodetool eval "pool_debug:status(json).")

# now print it out
code="Json = binary_to_list($Status),
      io:format(\"~p~n\", [Json]),
      halt()."
echo $(erl -boot no_dot_erlang -sasl errlog_type error -noshell -eval "$code")
```

## Booting, Upgrade and Inspecting

The extended start script that comes with `relx` provides a few ways of starting
and connecting to your release.

For local development you'll likely use `console`. In production you'll want
`foreground`, no matter if you are starting manual in something like `tmux`,
using an init system like `systemd` or running the release in a Docker container.

To open a console with a node started with `foreground` use `remote_console`.

### Appup generation

#### Using relflow

For a detailed workflow including version increments and appup generation checkout Richard Jones [relflow](https://github.com/RJ/relflow) tool built around `rebar3`.

### Deploying an upgrade

For the basic release upgrade after install of a release assume we have a release named `myrel` with a version `0.0.1` and `0.0.2`:

  * Installing: Installing a release on a running system will unpack and upgrade the version: `bin/myrel install 0.0.1`

  * Listing: you can inspect what versions are currently available: `bin/myrel versions`

  * Upgrading: If the version is already unpacked you can simply call `upgrade` to upgrade to the version: `bin/myrel upgrade 0.0.2`

  * Downgrading: To downgrade to the previous version use the `downgrade` command: `bin/myrel downgrade 0.0.1` 

## References

  * [relx](http://github.com/erlware/relx)
  * [relflow](http://github.com/RJ/relflow)
  * [Releases](https://adoptingerlang.org/docs/production/releases/) chapter from Adopting Erlang
  * [Releases](https://learnyousomeerlang.com/release-is-the-word) chapter from Learn You Some Erlang
  * OTP [release](http://www.erlang.org/doc/design_principles/release_structure.html) documentation
  * OTP [target system](http://www.erlang.org/doc/system_principles/create_target.html) documentation
