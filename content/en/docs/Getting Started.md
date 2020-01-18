---
title: "Getting Started"
excerpt: ""
---
#  Getting Started


## ! Backwards Compatibility !

	 We attempt very limited backwards compatibility with previous rebar versions' configurations and will try to call out important changes in the documentation. 



## Installing Erlang

Install Erlang as you usually would; rebar3 does not bundle it. People using Windows can get the binaries from erlang.org, and most linux or unix-like distributions can get those from package managers. OSX is fine with Homebrew.



People wanting to run multiple versions or a source compilation can use either [kerl ](https://github.com/kerl/kerl), [erln8](https://github.com/kerl/kerl), or the instructions from [erlang.org](http://erlang.org/doc/installation_guide/INSTALL.html).



Debian and Ubuntu derivatives have a history of having broken up the Erlang standard release into sub-libraries packages that are not all installed by default and will end up with missing packages compared to other installs. Be sure to install the extra Erlang packages and development packages for everything to work fine, or switch to one of the methods for source installs.

## ! Windows Users !

	 At present the Erlang installer does not create a PATH entry. For users unaware of how to create this, you may need local or network administrator permissions.

* open a file explorer window
* right-click "This PC" which appears in the left-hand side menu
* left click "Properties" in the menu that appears
* left-click "Advanced system settings" in the new window
* left-click on Environment Variables at the bottom of the new window
* Scroll to "Path", "path" or "PATH" and left click that line
* left-click "Edit..."
* Add two entries
 * %USERPROFILE%\bin
 * C:\Program Files\erl10.5\bin
* download and install erlang binary
* download rebar3
* make a `bin` folder in your %USERPROFILE% directory.
  this can be reached using the bar which tells you where you are in an explorer window, replacing content with `%USERPROFILE%` and pressing the return key. From there right-click and create a new folder
* copy rebar3 file to %USERPROFILE%\bin
* make a file using notepad.exe with the contents of the .cmd invoker for escript to run rebar3 (provided in this document below)
* open a new command-line and type rebar3 



## Installing Binary

Download a nightly binary [here](https://s3.amazonaws.com/rebar3/rebar3). Ensure it is executable (`chmod +x`) and simply copy to a directory in your `$PATH`. 



It is common to create a directory `~/bin/` to place commands like `rebar3` and add it to your path with `export PATH=~/bin/:$PATH` in your `~/.bashrc`, `~/.zshrc` or equivalent.



Windows users who want to use the code from PowerShell or cmd.exe (rather than a terminal emulator) must ensure that a `rebar3.cmd`file is added:

	 @echo off
	setlocal
	set rebarscript=%~f0
	escript.exe "%rebarscript:.cmd=%" %* 
The source installation that follows creates the file automatically.

## Installing from Source



	 $ git clone https://github.com/erlang/rebar3.git
	$ cd rebar3
	$ ./bootstrap 
And now you have the script `rebar3` and can copy it to somewhere in your `$PATH` as described in the previous section. Windows users should also include the `rebar3.cmd` for the util to work in PowerShell or cmd.exe.

## Extracting and Upgrading

An additional way to install and run rebar3 can be found under the `local` namespace. `rebar3 local install` will extract the contents of the escript to `~/.cache/rebar3/lib` and create a shell script `~/.cache/rebar3/bin/rebar3`:

	 $ ./rebar3 local install
	===> Extracting rebar3 libs to ~/.cache/rebar3/lib...
	===> Writing rebar3 run script ~/.cache/rebar3/bin/rebar3...
	===> Add to $PATH for use: export PATH=$PATH:~/.cache/rebar3/bin 
Plus, `rebar3 local upgrade` will fetch the latest stable escript from s3 and install the same way: 

	 $ rebar3 local upgrade
	===> Extracting rebar3 libs to ~/.cache/rebar3/lib...
	===> Writing rebar3 run script ~/.cache/rebar3/bin/rebar3...
	===> Add to $PATH for use: export PATH=$PATH:~/.cache/rebar3/bin 


## Creating a New Project



	 $ rebar3 new release myrelease
	===> Writing apps/myrelease/src/myrelease_app.erl
	===> Writing apps/myrelease/src/myrelease_sup.erl
	===> Writing apps/myrelease/src/myrelease.app.src
	===> Writing rebar.config
	===> Writing config/sys.config
	===> Writing config/vm.args
	===> Writing .gitignore
	===> Writing LICENSE
	===> Writing README.md 
Continue on to [Basic Usage](/docs/basic-usage) to learn more on how to use `rebar3`.
