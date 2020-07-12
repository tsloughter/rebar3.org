---
title: "Publishing Packages"
excerpt: ""
---
{{% blocks/callout type="warning" title="Hex.pm account setup" %}}
  This tutorial assumes you have followed instructions to setup and login to your [hex.pm](https://hex.pm) account. See the [Hex Package Management User subsection](doc:hex-package-management#user) for details. 
{{% /blocks/callout %}}

Metadata for a package is found in the application's specification and the dependencies found in `rebar.config`/`rebar.lock`.



* `name`: The name of the package is the name of the application found in your `.app.src` or `.app` file.

* `version`: The `vsn` entry in `.app.src` or `.app` is used.

* `description`: The `description` entry in `.app.src` or `.app` is used.

* `requirements`: Level 0 locked dependencies that are package (hex) dependencies are used as requirements.



Additional metadata that can be provided in `.app.src` includes:

- `licenses`: a list of licenses applying to code in the application

- `links`: tuple-list of links related to the project.

- `exclude_files`: a list of file-paths, relative to the project root, which should not be published



Example `.app.src` file:

	 {application, <NAME>,
	 [{description, "<DESCRIPTION>"}
	 ,{vsn, "<VSN>"}
	 ,{registered, []}
	 ,{applications,
	    [kernel,stdlib]}
	 ,{env,[]}
	 ,{modules, []}
	   
	 ,{licenses, ["MIT"]}
	 ,{links, [{"Github", "https://github.com/tsloughter/rebar3_hex"}]}
	 ,{exclude_files, ["src/generated_by_script.erl"]}
	 ]}.
	 


## Example

For the example let's look at the plugin itself, [rebar3_hex](https://github.com/tsloughter/rebar3_hex), starting with the `rebar.config`:

	 {deps, [{erlware_commons, "0.12.0"}
	       ,{ssl_verify_hostname, "1.0.4"}
	       ,{jsx, "2.6.1"}
	       ,{providers, "1.3.1"}
	       ]}. 
Now `rebar3_hex.app.src`:

	 {application, rebar3_hex,
	 [{description, "Hex.pm plugin for rebar3"}
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
	 ,{links, [{"Github", "https://github.com/tsloughter/rebar3_hex"}]}
	 ]}.
	 
Running `hex publish`:

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
	 
Lastly, publish the documentation for your application:

	 $ rebar3 hex docs                  
	===> Verifying dependencies...
	===> Running edoc for rebar3_hex
	===> Published docs for rebar3_hex 0.1.0 
