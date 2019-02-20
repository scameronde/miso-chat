# How to install, compile and run this chat client

## Installing, compiling and running the chat server

See the readme of the server.

## Install the tools neccessary for building, compiling and running the client

You need a Linux system to be able to compile and run this demo.

First install Nix:

```shell
curl https://nixos.org/nix/install | sh
```

If you get an error like `error: cloning builder process: Operation not permitted` you have to execute

```shell
sudo sysctl kernel.unprivileged_userns_clone=1
```

and repeat the curl command.

If Nix has successfully been installed, run the following command. Depending on the state of the global nix caches this can take a very long time.

```shell
nix-build
```

When finished, open two shells. One is for running a small webserver for delivering the client, the second is used to build the client.

In the server shell run the following command:

```shell
nix-shell -A server-shell
```

You should now be dropped into a new shell loaded with the haskell environment. 
Now start the small server with

```shell
cabal new-run server
```

Now start a haskell development environment in the client shell with

```shell
nix-shell -A client-shell
```

You should now be dropped into a new shell loaded with the haskell environment.
Now compile the client with

```shell
cabal new-build --ghcjs
```

The first time you also have to link the output of the build into the folder for the webserver

```shell
./symlink.sh
```

Now open a browser at http://localhost:8080/index.html

Have fun!
