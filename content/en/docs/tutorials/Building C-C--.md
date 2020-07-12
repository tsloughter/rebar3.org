---
title: "Building C/C++"
excerpt: ""
---

{{< blocks/callout type="warning" title="No port compiler">}}
In rebar3 it is required to have a Makefile or other instructions for building your C/C++ code outside of rebar itself.
{{< /blocks/callout >}}


## Using the Makefile Template

We'll start by making a new lib named `test_nif` and then using the `cmake` template from the root of the `test_nif` project.

	 $ rebar3 new lib test_nif
	===> Writing test_nif/src/test_nif.erl
	===> Writing test_nif/src/test_nif.app.src
	===> Writing test_nif/rebar.config
	===> Writing test_nif/.gitignore
	===> Writing test_nif/LICENSE
	===> Writing test_nif/README.md
	$ cd test_nif
	$ rebar3 new cmake
	===> Writing c_src/Makefile 
In `test_nif`'s `rebar.config`, add the [pre_hooks](http://www.rebar3.org/v3.0/docs/configuration#hooks) line so that `make` is called when `compile` is run. Furthermore, add the `post_hooks` entry for cleaning up the built C object files.



The `Makefile` written by `rebar3 new cmake` is a GNU Makefile, which means you will need to have GNU Make installed on the system. In the example, we provide a handler for the FreeBSD operating system, which assumes GNU Make is called `gmake`.

	 {erl_opts, [debug_info]}.
	{deps, []}.
	
	{pre_hooks,
	  [{"(linux|darwin|solaris)", compile, "make -C c_src"},
	   {"(freebsd)", compile, "gmake -C c_src"}]}.
	{post_hooks,
	  [{"(linux|darwin|solaris)", clean, "make -C c_src clean"},
	   {"(freebsd)", clean, "gmake -C c_src clean"}]}. 
Below is a NIF which has a function `repeat` that will take a `pid` and an Erlang term to send to that `pid`.

	 #include "erl_nif.h"
	
	ERL_NIF_TERM
	mk_atom(ErlNifEnv* env, const char* atom)
	{
	    ERL_NIF_TERM ret;
	
	    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
	    {
	        return enif_make_atom(env, atom);
	    }
	
	    return ret;
	}
	
	ERL_NIF_TERM
	mk_error(ErlNifEnv* env, const char* mesg)
	{
	    return enif_make_tuple2(env, mk_atom(env, "error"), mk_atom(env, mesg));
	}
	
	static ERL_NIF_TERM
	repeat(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
	{
	    ErlNifEnv* msg_env;
	    ErlNifPid pid;
	    ERL_NIF_TERM copy;
	
	    if(argc != 2)
	    {
	        return enif_make_badarg(env);
	    }
	
	    if(!enif_is_pid(env, argv[0]))
	    {
	        return mk_error(env, "not_a_pid");
	    }
	
	    if(!enif_get_local_pid(env, argv[0], &pid))
	    {
	        return mk_error(env, "not_a_local_pid");
	    }
	
	    msg_env = enif_alloc_env();
	    if(msg_env == NULL)
	    {
	        return mk_error(env, "environ_alloc_error");
	    }
	
	    copy = enif_make_copy(msg_env, argv[1]);
	
	    if(!enif_send(env, &pid, msg_env, copy))
	    {
	        enif_free(msg_env);
	        return mk_error(env, "error_sending_term");
	    }
	
	    enif_free_env(msg_env);
	    return mk_atom(env, "ok");
	}
	
	static ErlNifFunc nif_funcs[] = {
	    {"repeat", 2, repeat}
	};
	
	ERL_NIF_INIT(test_nif, nif_funcs, NULL, NULL, NULL, NULL);
	 
Modify `test_nif.erl` to load the `test_nif` shared library from `priv` and export `repeat/2`.

	 -module(test_nif).
	-export([repeat/2]).
	-on_load(init/0).
	
	-define(APPNAME, test_nif).
	-define(LIBNAME, test_nif).
	
	repeat(_, _) ->
	    not_loaded(?LINE).
	
	init() ->
	    SoName = case code:priv_dir(?APPNAME) of
	        {error, bad_name} ->
	            case filelib:is_dir(filename:join(["..", priv])) of
	                true ->
	                    filename:join(["..", priv, ?LIBNAME]);
	                _ ->
	                    filename:join([priv, ?LIBNAME])
	            end;
	        Dir ->
	            filename:join(Dir, ?LIBNAME)
	    end,
	    erlang:load_nif(SoName, 0).
	
	not_loaded(Line) ->
	    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
	 
Note that the `exit()` function will cause Dializer errors.  `erlang:nif_error/1` will no and is preferred here:  https://erlang.org/doc/man/erlang.html#nif_error-1



Run `rebar3 shell` and give the NIF a try.

	 $ rebar3 shell
	===> Verifying dependencies...
	===> Compiling test_nif
	Erlang/OTP 17 [erts-6.3] [source] [64-bit] [smp:4:4] [async-threads:0] [kernel-poll:false]
	
	Eshell V6.3  (abort with ^G)
	1> test_nif:repeat(self(), hello).
	ok
	2> receive X -> X end.
	hello 


## References

  * [Erlang/OTP Interoperability Tutorial](http://www.erlang.org/doc/tutorial/introduction.html)
