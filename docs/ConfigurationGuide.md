# Writing Configuration Files for Keypadder <!-- omit in toc -->
Keypadder requires a [TOML](https://toml.io/) configuration ("conf") file to be specified when it is launched (via the `--config` option).

The conf file tells Keypadder what port number it should run on, and describes the layout and function of each keypad tab.

A sample configuration [keypad.toml](../examples/keypad.toml) is provided in the `examples` subdirectory. 
It makes use of all of the features currently implemented by the application.

- [Overall Structure](#overall-structure)
- [The `[keypadder]` Section](#the-keypadder-section)
- [The `[[tab]]` Section(s)](#the-tab-sections)
- [The `keys` Section](#the-keys-section)
  - [The `label` Item](#the-label-item)
  - [The `colspan` and `rowspan` Items](#the-colspan-and-rowspan-items)
  - [The `bg` Item](#the-bg-item)
  - [The `send` Item](#the-send-item)
    - [Special Keys and Modifiers](#special-keys-and-modifiers)
      - [`Shift+`](#shift)
      - [`Ctrl+`](#ctrl)
      - [`Alt+`](#alt)
      - [`BLANK`](#blank)
      - [Unicode (`@`)](#unicode-)


## Overall Structure
A conf file must contain at least two main sections.
The first section  `[keyppader]` is compulsory, it is followed by one or more `[[tab]]` sections.

You may also include blank lines and comments - these are not required but may be useful to break your conf file into readable sections etc.

Note that the layout of your conf file has no effect on the width of the keypad layout that is produced.  See the `cols` details below for how to control the number of columns of keys.

## The `[keypadder]` Section
This section contains overall configuration items for Keypadder.
Currently, two items are defined - the `port` definition is mandatory, and an optional `tabswitch` setting.

Eg. 
```
[keypadder]
port = 8082
tabswitch = "dropdown"
```

The port item tells keypadder what TCP/IP port to run on.  It will form part of the URL for accessing Keypadder from your mobile device.  For example, with the above port definition, you might access keypadder via the URL `http://mydesktop:8082/` where the number following the colon corresponds to your specified port.

The `tabswitch` item is optional and can contain one of two values `"tabs"` or `"dropdown"`.
It affects the style of tab selector; either shaped tabs, or a dropdown select box.
If omitted, Keypadder assumes the tabbed style if there are seven (7) or fewer tabs, and a dropdown
if there are eight (8) or more.

## The `[[tab]]` Section(s)
You add one `[[tab]]` section for each tab (page) of buttons you want to display.
There are a couple of compulsory config items for the tab itself, then a table of key definitions (see below).

Every tab must contain a `label`, a `cols`, and a `keys` item.
There is an optional `fontsize` option.

The `label` item is a simple string displayed at the top of the tab of keys.

The `cols` item is a positive integer telling Keypadder how many columns to use for the keypad layout.
N.B. It is this `cols` item that defines how many keys are in each row.

Eg.
```
[[tab]]
label = "French"
cols = 3
fontsize = "20pt"	
...
```
The optional `fontsize` item is a string consisting of a number and unit-specifier.

Eg. `fontsize = "12pt"` or `fontsize = "22mm"`.

Next comes the meat of the config file: the ``keys`` defintions.

## The `keys` Section
This is where the keys are defined, in order, from left to right.
Each key definition contains two compulsory items: a `label` and a `send` string.

Eg.
```
keys = [
  { label = "&agrave;", send = "RIGHTALT,GRAVE,A" },
  ...
```

There are also some optional layout options: `colspan`, `rowspan` and `bg`.

### The `label` Item
This defines what appears on your keypad.
It may be a simple character, a string, or an [HTML entity](https://developer.mozilla.org/en-US/docs/Glossary/Entity).

Eg. `label = ">"` (character), `label = "Esc"` (string), `label = "&agrave;"` (HTML entity)

Note that HTML entities have been added to the standard over the years - older devices may not display newer entities.

### The `colspan` and `rowspan` Items
These option items are used tell Keypadder to use extra grid positions for a key.  
They should be positive integers and they behave like the similarly-named HTML table attributes.
You will need to take account of the space used by a key with `colspan` greater than one later in the same row.
Similarly, you should take account of the space used by a key with `rowspan` greater than one in later rows.

Take a look at the sample "NumPad" layout to see a practical example.  Eg.
```
   { label = "&crarr;", rowspan = 2, send = "KPENTER" },
#
   { label = "0", colspan = 2, send = "KP0" },
   ...
```
The enter key is double-height, and the zero key is double-width.

### The `bg` Item
This option lets you override the background colour of the button using either a standard CSS colour name, 
or a six-digit hex value preceded by a `#` symbol.

Eg. `bg = "Red"` or `bg = "#FF0000"`

### The `send` Item
This defines what keyboard events Keypadder sends when a key is pressed.

It is a single string of one or more comma-separated keynames.

To see a list of the keynames that Keypadder recognises use the `--dumpkeys` option.
These names are based on the USB standard low-level key names (not character names).
See page 53 of the [Universal Serial Bus HID Usage Tables](https://www.usb.org/sites/default/files/documents/hut1_12v2.pdf) document for a complete explanation.  Unfortunately the standard is based on a US keyboard layout, you might have to make allowances for that in your configuration. Eg. French users may need to specify a "Q" where an "a" is required.

You must use upper case for these strings.

Simple Examples:
* `send = "W"` will send a keystroke (actually a keypress and then keyup) for the `w` character.
* `send = "KP3"` will send a keypad 3 keystroke
* `send = "RIGHTALT,GRAVE,A"` will send three keystrokes, possibly* generating an `à`

*Your system will need to have the right-ALT key configured as the 'compose' key for this to work.

#### Special Keys and Modifiers

There are some additional strings - currently `Shift+`, `Ctrl+`, `Alt+`, `BLANK`, and `@` - which have special meanings defined below.

##### `Shift+`

The `Shift+` string directs Keypadder to send the next keystroke shifted.

Examples:
* `send = "Shift+W"` will send a Shift-W keystroke
* `send = "Shift+F,R,E,D"` with send Shift-F, then unshifted r, e and d keys

##### `Ctrl+`

The `Ctrl+` string directs Keypadder to send the next keystroke with the "control" modifier.
You may combine it with the `Shift+` shift modifier - always in the order `Ctrl+Shift+`.

Examples:
* `send = "Ctrl+S"` will send the Ctrl-S keystroke
* `send = "Ctrl+Shift+S"` will send the Ctrl-Shift-S keystroke

##### `Alt+`
The `Alt+` string directs Keypadder to send the next keystroke with the "left-alt" modfier.
You may combine it with the `Ctrl+` modfier - always in the order `Ctrl+Alt+`.
You may combine it with both the `Ctrl+` and `Shift+` modfiers - always in the order `Ctrl+Alt+Shift+`.

Examples:
* `send = "Alt+F7"` will send the Left-Alt-F7 keystroke
* `send = "Ctrl+Alt+C"` will send the Ctrl-LeftAlt-C keystroke
* `send = "Ctrl+Alt+Shift+V"` will send the Ctrl-LeftAlt-Shift-V keystroke

##### `BLANK`

The `BLANK` string simply causes Keypadder to leave the key unlabelled.
It might be useful for creating large layouts.

##### Unicode (`@`)

The `@` symbol is shorthand for the Unicode prefix.  It is analagous to the Ctrl-Shift-U keyboard combination used to type Unicode characters in Linux.

It should be followed by keys for the Unicode character you desire.  This also works for emojis.
The application to which you are sending keystrokes must be Unicode-aware for these to be useful.

Examples:
* `send = "@,0,0,E,2,SPACE"` will send keystrokes for Ctrl-Shift-U 00e2, which produces `â`
* `send = "@,1,F,6,0,0,SPACE"` will produce a grinning face emoji `😀`
