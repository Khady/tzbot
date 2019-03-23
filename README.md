# tzbot

Simple executable to show the timezones of all users on a slack
workspace.

## Example

Display an ascii table:

```
$ dune exec ./tzbot.exe -- table $STOKEN
         Pacific Standard Time |     2019-02-12 06:30:14.501341 | slackbot
           Greenwich Mean Time |     2019-02-12 14:30:14.501341 | louis
```

Start the http server:

```
dune exec ./tzbot.exe -- server -p 8080 $STOKEN
```

## Build

```bash
opam init --bare
opam sw -y create . --deps-only --locked
make
```

If you already have an opam switch, you can simply call `make init`.
