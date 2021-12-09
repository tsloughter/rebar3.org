---
title: "Hex Package Management"
excerpt: ""
weight: 11
---

For package management `rebar3` uses [hex.pm](https://hex.pm), a package manager for Erlang and Elixir packages. To install the Hex plugin see the [plugin installation instructions](/docs/configuration/plugins/#hex-package-management).

For instructions on publishing your package see the [Publishing Packages](/docs/package_management/publishing-packages) tutorial. Below is an entry for each subtask of the `hex` namespace and what it does.

## config

`rebar3 hex <key> [<value>]`

Reads or updates hex configuration file.

| Key         | Description        |
| ----------- | ------------------ |
| username    | Hex username       |
| key         | Hex API key        |
| api_url     | Hex API base URL   |
| cdn_url     | Hex CDN base URL   |
| http_proxy  | HTTP proxy server  |
| https_proxy | HTTPS proxy server |

## cut

`rebar3 hex cut [-i major|minor|patch]`

Increment version number and publish package.

You'll be asked what type of increment to make to the version or it can be supplied as an argument to the command `-i major|minor|patch`.

## docs

`rebar3 hex docs`

Options: 

| Key         | Description        |
| ----------- | ------------------ |
| --dry-run    | Executes task without actually publishing docs|


Publishes documentation for the current project and version.

The documentation will be accessible at `https://hexdocs.pm/my_package/1.0.0`, `https://hexdocs.pm/my_package` will always redirect to the latest published version.

In order to automatically generate docs when running the docs command and likewise when publishing, we must configure a docs provider within `rebar.config`. Doc provider configuration can be setup for an entire project, but can also be handled on a per repo basis. 

Supported doc configuration options: 

| Key         | Description        |
| ----------- | ------------------ |
| provider    | The name of a valid doc provider in atom form|


Supported doc providers: 
 
 - edoc 

### Global config

Note that when we mean globally within the scope of a project, since a project may have many reposconfigured. Below is an example of such a configuration: 

```erlang
{erl_opts, [debug_info]}.
{deps, []}.
{hex, [{doc, #{provider => edoc}]}}.
```

### Repo specific config

In the example below we configure a doc provider for a specific repo. If we had a global project hex doc provider 
configuation, the below would override it : 

```
{erl_opts, [debug_info]}.
{deps, []}.
{hex,  [{repos, [ #{name => <<"my_private_hex">>,
                      repo_url => <<"https://my_private_hex.foo">>,
                      doc => #{provider => edoc}
                    }
                ]
        }
   ]
}.
```

With one of the above configurations in place documentation will now be generated when running the `rebar3 hex docs` task, which publishes documentation it generates to hex.pm by default or to the configured hex instance for a repo. You should also notice a `docs/` directory in the root of your project now containing at least an `index.html` file.  Note that an alias can be used to extend the documentation generation in the case of edoc.


See [Base Config/edoc](/docs/configuration/configuration/#edoc) for more information on configuring edoc itself.

### Without a doc provider

 One may choose not to use use edoc to generate their documentation and we support this. In this case, none of the above configuration is needed. All one has to do is have a `doc/` directory present in the root of the project directory with at least an `index.html` file present. 


## info

`rebar3 hex [<package> [<version>]]`

Prints hex package or system information.

If `package` is not given, print system information. This includes when registry was last updated and current system version.

If `package` is given, print information about the package. This includes all released versions and package metadata.

If `package` and version is given print release information. This includes remote Git URL and Git ref, and all package dependencies.

## key

Remove or list API keys associated with your account.

### Remove Key

Remove given API key from account. The key can no longer be used to authenticate API requests.

`rebar3 hex key remove <key_name>`

### List Keys

List all API keys associated with your account.

`rebar3 hex key list`

## publish

Publish a new version of your package and update the package.

`rebar3 hex publish`

If it is a new package being published it will be created and the user specified in `username` will be the package owner. Only package owners can publish.

## owner

Add, remove or list package owners.

A package owner have full permissions to the package. They can publish and revert releases and even remove other package owners.

### Add owner

Add an owner to package by specifying the package name and email of the new owner.

`rebar3 hex owner add <package> <email>`

### Remove owner

Remove an owner to package by specifying the package name and email of the new owner.

`rebar3 hex owner remove <package> <email>`

### List owners

List all owners of given package.

`rebar3 hex owner list <package>`

## user

Hex user tasks.

### Registers a new user

`rebar3 hex user register`

### Prints the current user

`rebar3 hex user whoami`

### Authorize a new user

Authorizes a new user on the local machine by generating a new API key and storing it in the hex config.

`rebar3 hex user auth`

### Deauthorize the user

Deauthorizes the user from the local machine by removing the API key from the hex config.

`rebar3 hex user deauth`

### Reset user password

`rebar3 hex user reset_password`

## search

Display packages matching the given search query.

`rebar3 hex search <term>`
