# Vision-Basic-IDE
This utility is (currently) a Windows application that assists in writing Commodore Basic code.

## Basic features
- No line numbering while editing
- Immediate Basic Code with Difference view
- Custom commands to assist in renumbering
- Labeling

Example:

```@Label1:print "hello world!":goto @Label1```

Results:

```10 print "hello world!":goto 10```

## TO DO
- History stored every SAVE and every REGEN
- Inline importing of files

# Rules
Install Lazarus, get this code, build from scratch, the application runs.

I don't want to rely on any massive external 3rd party library that may get antequated or potentially break future builds. 

If any additional library is included, it must be included within this repository in a sub directory and the configuration must be adjusted to read the library.
