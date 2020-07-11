---
title: "Custom Compiler Modules"
excerpt: ""
---

This is a new feature in rebar3 3.7.0 which allows to write custom compilers to be used with Rebar3. It is useful whenever you have files of a different language that you want to build alongside Erlang resources.

This interface is currently used internally for .xrl, .yrl, and .mib files, but currently few plugins have tried it.

{{% blocks/callout type="warning" title="This is an unstable interface" %}}

Since we have not had many plugin authors try this interface yet, it is marked as unstable and is suspect to change.

We are looking for help of contributors to further stabilize it before marking it stable. You should use this if you are willing to enter in contact with us to help iterate on the features available in [Custom Compiler Plugins](doc:custom-compiler-plugins) 

It is possible that your custom compiler requires something more complex. For example, the facilities provided by this interface are insufficient to build projects that run with `mix` as a buildtool, and the plugin for that uses [a custom compiler plugin](doc:custom-compiler-plugins) 

{{% /blocks/callout %}}

## Current Interface

The following callbacks are defined:

	 -type extension() :: string().
	-type out_mappings() :: [{extension(), file:filename()}].
	
	%% Returns context that the rebar3 compiler needs regarding the project
	%% and files supported by this compiler
	-callback context(rebar_app_info:t()) ->
	            %% source directories to look into for files
	            #{src_dirs     => [file:dirname()],
	            %% directory for include files
	              include_dirs => [file:dirname()],
	            %% file extension for the source files (".yrl")
	              src_ext      => extension(),
	            %% mapping of output extensions to output directories.
	            %% For example, .yrl files are compiled to .erl files
	            %% and so the outmapping is [{".erl", "path/to/app/src/"}]
	              out_mappings => out_mappings()}.
	
	%% Files required to be built in any specific priority before others
	                    %% directed graph of application names
	-callback needed_files(digraph:graph(),
	                    %% files found by the rebar3 compiler matching the context
	                       [file:filename()],
	                    %% previously provided filetype to directory mappings
	                       out_mappings(),
	                    %% Information about the current OTP app
	                       rebar_app_info:t()) ->
	            %% list of files that need to be built first, with their options.
	            %% used for things like parse transforms, for example
	            {{[file:filename()], term()},
	            %% list of files that can be built last, with their options
	             {[file:filename()], term()}}.
	
	%% Specify file which are required dependencies of the current file,
	%% allowing the compiler to build a graph of build order required.
	%% An example for this might be include files.
	%% If no specific dependencies exist (any order is good), return
	%% an empty list.
	-callback dependencies(Source :: file:filename(),
	                       SourceDir :: file:dirname(), 
	                       Directories :: [file:dirname()]) ->
	            [file:filename()].
	
	%% Compile the actual file
	                  %% file to compile
	-callback compile(file:filename(),
	                  %% mappings defined earlier for directories for each file type
	                  out_mappings(),
	                  %% a dictionary of rebar3 configuration values
	                  rebar_dict(),
	                  %% compiler-specific options list
	                  list()) ->
	            ok
	          | {ok, [string()]} % same but with filenames
	          | {ok, [string()], [string()]}. % same but with filenames & warnings
	
	%% Clean up after built files
	%%        files found by compiler,  app-specific info
	-callback clean([file:filename()], rebar_app_info:t()) -> _.
	 


## Initializing a Compiler Module as a Plugin

Register the compiler module in the same place where you would register [a custom compiler plugin](doc:custom-compiler-plugins):

	 %% Note: the name of the module matches the name of the plugin application
	-module(my_compiler_plugin).
	-export([init/1]).
	
	
	%% Called when rebar3 first boots, before even parsing the arguments
	%% or commands to be run. Purely initiates the provider, and nothing
	%% else should be done here.
	-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
	init(State) ->
	    %% Optional:
	  	%% Provider = providers:create([Options]),
	  	%% State1 = rebar_state:add_provider(State, Provider),
	  
	  	%% This adds the new compiler module:
	    State1 = rebar_state:append_compilers(State, [my_compiler_mod]),
	    %% If needing the new compiler module to take precedence over
	  	%% other ones (i.e. generating .erl files from another format):
	    State2 = rebar_state:append_compilers(State1, [translator_mod]),
	    {ok, State2}.
	
	 
