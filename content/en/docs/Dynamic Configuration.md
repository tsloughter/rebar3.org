---
title: "Dynamic Configuration"
excerpt: ""
---

With `rebar.config`, `*.app.src`, and overlay vars files you can make use of dynamic configuration based on [file:script/2](http://www.erlang.org/doc/man/file.html#script-2).


If a `<name>.script` exists in the same directory as the original file (in the case of `rebar.config` that would be `rebar.config.script`), the script file will be evaluated and the result used as configuration.



For convenience two bindings (variables) are available in a script during evaluation:



* `CONFIG` - result of [file:consult/1](http://www.erlang.org/doc/man/file.html#consult-1) if the script file being evaluated also exists  

without the ``.script`` extension. Otherwise, `[]`.

* `SCRIPT` - filename of the script being evaluated



In all cases, the data returned by the script, that is the last thing evaluated in the file, must be data in the same format as the original non-script file. For example, if I have `rebar.config.script` that script must return rebar configuration data, if it is an `<app-name>.app.src.script` it must return data in the Application Metadata Format.



Each script may be executed more than once within each rebar execution. It is a good idea to avoid scripts that have side-effects that are not [idempotent](https://en.wikipedia.org/wiki/Idempotence).

## Simple Example

If you are building fairly complex systems, going to github to fetch deps each time will slow down your development cycle. Serving dependencies locally may be a much faster route, but you don't want to modify the `rebar.config` and suffer merge conflicts as a result.



The following `rebar.config.script` file can be kept centrally, and linked into your application directory:

	 case os:getenv("REBAR_DEPS") of
	    false -> CONFIG; % env var not defined
	    []    -> CONFIG; % env var set to empty string
	    Dir ->
		lists:keystore(deps_dir, 1, CONFIG, {deps_dir, Dir})
	end. 
Whenever you want to build 'properly' (which you should, regularly), simply call  `unset REBAR_DEPS` (or equivalent), and perform a clean build.



Note that `file:script/2` differs from `file:consult/1` in that only the result of the  last expression is returned. You must therefore take care to return a list of config items. Before that, you may do any form of IO (including network), checking OS environment variables, reading files (perhaps calling `file:script/2` on other files) or writing files. You can basically use all OTP libs. As in `file:eval/2`, each expression is terminated with a full stop.
