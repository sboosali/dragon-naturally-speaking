# `natlink-client`

a lightweight wrapper around the `natlink` API.

## example `natlink` plugins

they help out with the following: 

- show how to use this project, and `natlink` generally;
- and let you test your plugins, with the `natlink` stubs (see below).

they can test not just that a template for a `natlink`-plugin parses, but that it (as a module) can be loaded successfully by `Dragon NaturallySpeaking`. i.e. their imports are found, and their top-level declarations are evaluated successfully. this is useful, since simple errors (e.g. a `ParseError`), sometimes, cause `Dragon NaturallySpeaking` to crash and/or hang.

they are the following:

- `./sources/_commands.py`: a "Command & Control" grammar, the most frequent one.
- `./sources/_selection.py`: a "Select & Say" grammar. i.e. `"select <sequence>"`, where `<sequence>` is a continguous sequence of words in a given buffer (the current window, the words that are visible on the screen, etc). this buffer must be managed. (TODO haven't gotten it to work yet.)
- `./sources/_dictation.py`: a "Dictation" grammar. (TODO).

they can be run with:

    make execute
    
## `natlink` modules

these modules are **stubs**:

- `natlinkutils`
- `natlink`
- `natlinkmain`

## 

