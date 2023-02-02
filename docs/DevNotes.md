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