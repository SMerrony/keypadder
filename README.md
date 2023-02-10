# Keypadder
Keypadder is a programmable virtual keypad you can access via a mobile phone or tablet.

![Screenshot1](./Screenshots/v0_1_0_French.png)
![Screenshot2](./Screenshots/v0_1_0_Musescore.png)
![Screenshot3](./Screenshots/v0_1_0_Emojis.png)

## Key Features

* Simple TOML definition for pads
* Flexible layout possibilities to suit portrait or landscape modes
* Keytops can be strings &/or HTML entities eg. `&agrave;`
* No app required on phone/tablet - it uses the browser
* Keys can send multiple keystrokes, can use for macros
* Keystrokes are sent to the currently focused application

(Keypadder currently only runs on 64-bit Linux systems)

## Build and Install
Keypadder is built using the [Alire](https://alire.ada.dev/) build system.

A binary (.deb) version for 64-bit Debian-based systems *may* be available as part of a release.

### Source
Keypadder is [hosted on GitHub](https://github.com/SMerrony/keypadder).
It is written in GNU Ada (GNAT). 

** TODO  - Add more detail here once Alire crate is available **

## Running Keypadder
`keypadder -h` will give a list of all options. 
```
$ ./bin/keypadder -h
Usage of keypadder:
  --config=<config-file>  Configuration file for keypadder (required)
  --dumpkeys              List all defined key mnemonics
  -h | --help             This help
  -V | --version          Show the version of keypadder and exit
  -v | --verbose          Show lots of detail when running
  ```

**Keypadder must be run with root privileges in order to inject keystrokes into your system.**

Eg: `sudo ./bin/keypadder --config=examples/keypad.toml`

