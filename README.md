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
- Masks
- Terminal size detection; this is currently fixed at 24x80.
- Autocomplete / suggestions
- Many more small features
