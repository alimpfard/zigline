# Zigline

A partial port of Serenity's [LibLine](https://github.com/SerenityOS/serenity/tree/master/Userland/Libraries/LibLine) in Zig.

Have you ever looked at the current zig line editor options and thought "huh, these are all linenoise"?
Well, look no further, this is not linenoise!

LibLine is a full-featured terminal line editor with support for:

- Flexible autocompletion
- Live prompt and buffer update/stylisation
- Multiline editing
- and more.

This port of LibLine is a work in progress, and as such, is incomplete. The following features are not yet implemented:
- Styling
- Masks
- Signal handling (resize handler + interrupt handler)
- Autocomplete / suggestions
- History load/save
- History in general
- Many more small features
