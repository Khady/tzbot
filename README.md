# tzbot

Simple executable to show the timezones of all users on a slack
workspace.

## Example

```
$ dune exec -- ./tzbot.exe -t $STOKEN
         Pacific Standard Time |     2019-02-12 06:30:14.501341 | slackbot
           Greenwich Mean Time |     2019-02-12 14:30:14.501341 | louis
```

## Build

```bash
opam init --bare
opam sw -y create . --deps-only --locked
make
```

If you already have an opam switch, you can simply call `make init`.
