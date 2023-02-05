# Development Notes

## Simulating keystrokes
For at least the initial version, we are only considering Linux(-like) systems

We will simulate keystrokes by sending events to `/dev/uinput`

According to https://kernel.org/doc/html/v4.12/input/uinput.html we will need to use the following system calls...

1. ioctl - eg. `ioctl(fd, UI_SET_EVBIT, EV_KEY);`
2. open  - eg. `int fd = open("/dev/uinput", O_WRONLY | O_NONBLOCK);`
3. write - eg. `write(fd, &ie, sizeof(ie));`
4. close - eg. `close(fd);`

A guide to handling Linux syscalls: https://www.pegasoft.ca/resources/boblap/16.html

### IOCTL
...is variadic, and a non-POSIX call.  We need several variations with 2 and 3 varying parameters.

1. ioctl(fd, UI_DEV_CREATE);
2. ioctl(fd, UI_SET_EVBIT, EV_KEY);
3. ioctl(fd, UI_DEV_SETUP, &usetup);


### OPEN & CLOSE
We need to use the syscalls in order to set the correct flags (which are not otherwise available in Ada).

### WRITE
We must use our own version due to the fact we are using our own `open`.

## Key Mapping Configuration

There's a compromise to be made between keeping the configuration TOML format user-friendly and having it programmer-friendly.

For this `send` string: `"V"` we need to send...
1. Keydown V, REPORT
2. Keyup V, REPORT

Could store Down-V-val, Up-V-val

For this `send` string: `"COMPOSE,GRAVE,A"` we need to send...
1. Keydown COMPOSE, REPORT
2. Keyup COMPOSE, REPORT
3. Keydown GRAVE, REPORT
4. Keyup GRAVE, REPORT
5. Keydown A, REPORT
6. Keyup A, REPORT

So, a comma or end of string implies a key-up event.

Could store Down-COMPOSE-val, Up-COMPOSE-val, Down-GRAVE-val, etc...

For this `send` string: `"COMPOSE,^6,A"` we need to send...
1. Keydown COMPOSE, REPORT
2. Keyup COMPOSE, REPORT
3. Keydown LEFTSHIFT, REPORT
4. Keydown 6, REPORT
5. Keyup 6, REPORT
6. Keyup LEFTSHIFT, REPORT
7. Keydown A, REPORT
8. Keyup A, REPORT

So, an up-arrow (`^`) means we have to send and report a LEFTSHIFT down, then send the next character (down, report, up, report), then send and report a LEFTSHIFT up.

Could store Down-COMPOSE-val, Up-COMPOSE-val, Down-LEFTSHIFT-val, Down-6-val, Up-6-val, Up-LEFTSHIFT-val, etc...