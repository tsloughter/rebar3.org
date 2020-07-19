---
title: "Getting Started"
weight: 1
excerpt: ""
---

## Installing Erlang

See the [Adopting Erlang](https://adoptingerlang.org/) section [Installing Erlang/OTP](https://adoptingerlang.org/docs/development/setup/#installing-erlang-otp) for detailed steps of installing Erlang for Windows, Mac, Linux and FreeBSD.

## Installing from the Rebar3 escript

Download the latest stable release as an executable escript [here](https://s3.amazonaws.com/rebar3/rebar3). This executable can be called from anywhere and it is advised not to keep it in your project's repository.

With the escript it is recommended to then install the extracted form of rebar3:

```shell
$ ./rebar3 local install
===> Extracting rebar3 libs to ~/.cache/rebar3/lib...
===> Writing rebar3 run script ~/.cache/rebar3/bin/rebar3...
===> Add to $PATH for use: export PATH=$PATH:~/.cache/rebar3/bin 
```

Be sure to follow the instructions the command outputs and add to your systems `$PATH`, like `export PATH=$PATH:~/.cache/rebar3/bin`. Then, you can delete the `rebar3` escript used to run `local install`. 
    
When a new stable release of `rebar3` is available simply run `rebar3 local upgrade` and the new version will fetched and installed the same way: 

```shell
$ rebar3 local upgrade
===> Extracting rebar3 libs to ~/.cache/rebar3/lib...
===> Writing rebar3 run script ~/.cache/rebar3/bin/rebar3...
===> Add to $PATH for use: export PATH=$PATH:~/.cache/rebar3/bin 
```

## Installing from Source

The `rebar3` project's repo is hosted on Github and comes with a `bootstrap` script for building from source:

```shell
$ git clone https://github.com/erlang/rebar3.git
$ cd rebar3
$ ./bootstrap 
```

This will compile a `rebar3` escript to the top level of the `rebar3` directory which you can then install globally:

```shell
$ ./rebar3 local install
```

## Creating a New Project

```shell
$ rebar3 new umbrella myproj
===> Writing apps/myproj/src/myproj_app.erl
===> Writing apps/myproj/src/myproj_sup.erl
===> Writing apps/myproj/src/myproj.app.src
===> Writing rebar.config
===> Writing config/sys.config
===> Writing config/vm.args
===> Writing .gitignore
===> Writing LICENSE
===> Writing README.md 
```

Continue on to [Basic Usage](/docs/basic-usage) to learn more on how to use `rebar3`.
