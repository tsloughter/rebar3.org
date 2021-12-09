---
title: "Publishing Packages"
excerpt: ""
weight: 51
---
{{% blocks/callout type="warning" title="Hex.pm account setup" %}}
This tutorial assumes you have followed instructions to setup and login to your [hex.pm](https://hex.pm) account. See the [Hex Package Management User subsection](/docs/package_management/hex_package_management/#user) for details.
{{% /blocks/callout %}}

Metadata for a package is found in the application's specification and the dependencies found in `rebar.config`/`rebar.lock`.

- `name`: The name of the package is the name of the application found in your `.app.src` or `.app` file.
- `version`: The `vsn` entry in `.app.src` or `.app` is used.
- `description`: The `description` entry in `.app.src` or `.app` is used.
- `requirements`: Level 0 locked dependencies that are package (Hex) dependencies are used as requirements.

Additional metadata that can be provided in `.app.src` includes:

- `licenses`: a list of licenses applying to code in the application
- `links`: tuple-list of links related to the project.
- `exclude_files`: a list of file-paths, relative to the project root, which should not be published

Example `.app.src` file:

```erlang
{application, rebar3_hex,
 [{description, "Hex.pm plugin for Rebar3"}
 ,{vsn, "0.1.0"}
 ,{registered, []}
 ,{applications,
   [kernel
   ,stdlib
   ,providers
   ,erlware_commons
   ,jsx
   ]}
 ,{env,[]}
 ,{modules, []}
 ,{licenses, ["MIT"]}
 ,{links, [{"GitHub", "https://github.com/tsloughter/rebar3_hex"}]}
 ]}.
```

## Example

For the example let's look at the plugin itself, [rebar3_hex](https://github.com/tsloughter/rebar3_hex), starting with the `rebar.config`:

```erlang
{deps, [{erlware_commons, "0.12.0"}
       ,{ssl_verify_hostname, "1.0.4"}
       ,{jsx, "2.6.1"}
       ,{providers, "1.3.1"}
       ]}.
```

If we want to automatically generate and publish documentation we can add hex doc provider config: 
```erlang
{hex, {doc, #{provider => edoc}}}.
```

See [Hex Package Management/docs](/docs/package_management/hex_package_management/#docs) for more information on configuring a docs provider.

Now `rebar3_hex.app.src`:

```erlang
{application, rebar3_hex,
 [{description, "Hex.pm plugin for Rebar3"}
 ,{vsn, "0.1.0"}
 ,{registered, []}
 ,{applications,
   [kernel
   ,stdlib
   ,providers
   ,erlware_commons
   ,jsx
   ]}
 ,{env,[]}
 ,{modules, []}

 ,{licenses, ["MIT"]}
 ,{links, [{"GitHub", "https://github.com/tsloughter/rebar3_hex"}]}
 ]}.
```

Running `hex publish`:

```shell
$ rebar3 hex publish
Publishing rebar3_hex 0.1.0
  Dependencies:
    providers 1.3.1
    erlware_commons 0.12.0
    ssl_verify_hostname 1.0.4
    jsx 2.6.1
  Excluded dependencies (not part of the Hex package):

  Included files:
    src/rebar3_hex.app.src
    src/rebar3_hex.erl
    src/rebar3_hex.hrl
    src/rebar3_hex_cacerts.erl
    src/rebar3_hex_config.erl
    src/rebar3_hex_docs.erl
    src/rebar3_hex_http.erl
    src/rebar3_hex_info.erl
    src/rebar3_hex_key.erl
    src/rebar3_hex_owner.erl
    src/rebar3_hex_pkg.erl
    src/rebar3_hex_search.erl
    src/rebar3_hex_tar.erl
    src/rebar3_hex_user.erl
    src/rebar3_hex_utils.erl
    priv/ca-bundle.crt
    rebar.config
    README.md
    LICENSE
Proceed? ("Y")> Y
===> Published rebar3_hex 0.1.0
===> Published rebar3_hex docs 0.1.0
```

You can also opt to publish a package without publishing docs using the `--without-docs` switch.

Lastly and if you don't setup a docs provider we can still publish the documentation in a few seperate steps manually. The example below uses edoc:

```shell
$ rebar3 edoc 
$ rebar3 hex docs                  

===> Verifying dependencies...
===> No valid hex docs configuration found. Docs will will not be generated.
===> Published docs for rebar3_hex 0.1.0
```

Alternatively, you may generate docs using any tool you see fit. rebar3_hex will check for the existence of `doc`
directory in the the root of your project which should contain at least an `index.html` file and publish the contents, devoid of configuration and edoc.

See [Hex Package Management/docs](/docs/package_management/hex_package_management/#docs) for more information on configuring a docs provider.
