# hajas

hajas is a JavaScript deobfuscator written in haskell.

## Building

Use stack:

```bash
stack build
```

There's also a nix derivation available, so it should build with:

```bash
nix build
```

Or enter a shell with hajas ready for use:

```bash
nix run
```

## Usage

If using stack, print usage with

```bash
stack exec hajas -- -h
```

```
Usage: hajas ([-a] | [-l]) [-f FILE] [-r FILE] [-e PASS]
  JavaScript deobfuscator

Available options:
  -a                       Print AST
  -l                       List passes
  -f FILE                  Input file. stdin if not given
  -r FILE                  Replacements file
  -e PASS                  Exclude pass
  -h,--help                Show this help text

```

## Known issues

* `language-ecmascript` library fails to parse some valid language
  features like octal numbers.
* Pretty printing is not very pretty.
