# VCR - datagram recorder and replay

VCR is a system for recording, storing and replaying UDP traffic over
IP4 and IP6 communication network.

# Terminology

- **packet** or **datagram** is a UDP unicast or multicast packet sent over
  IPv4 or IPv6, or any other binary string of data.

- **channel** is a name (string) associated with a specific data flow.
  For example: An administrator might decide to assign the following
  naming schema:
    - UDP traffic ip=127.0.0.1, port=56001 will be recorded as channel="ch1"
    - UDP traffic ip=127.0.0.1, port=56002 will be recorded as channel="ch2"
    - ...

- **UTC time** - Universal Time Coordinated

- **monotonic time** ever incrementing time. This time is independent of
  the real wall-clock time and is guaranteed to always increments,
  regardless of any irregular changes of UTC time, for example during
  NTP synchronization, leap second compensations, manual clock setting...

    It has an arbitrary starting point, for example the system boot time.
    The absolute value of monotonic time has no particular meaning,
    however, the difference between 2 monotonic times represents a true
    time difference between 2 distinct events.

- **session** is a one time invocation of a capture module (recorder).
  The 'session' identifier is important to compare monotonic times
  between different events.
  It is only valid to compare monotonic times between events from the
  same 'session'.

- **track** represents a continous recording of a single channel.
  The 'track' identifier is important to verify 'sequence numbers' in
  the stream of events.
  It is only valid to compare sequence numbers between events from the
  same 'track'.

    Capture module allows dynamic reconfiguration (adding or removing channels).
    Each time a channel is removed and added again, the 'track' number
    changes and the sequence numbering starts from zero.

- **event** is a packet augmented with the following metadata:
    - channel name,
    - monotonic time of the moment when the packet was recorded,
    - UTC time of the moment when the packet was recorded,
    - session identifier,
    - track identifier,
    - packet sequence number,
    - packet data itself (UDP datagram and source address).

# VCR Modules

The VCR system consists of several modules, where the most important
is the capture module to store network traffic to recording files.

General principle is that events are recorded safely to files first.
Additional processing (analysis, replay,...) is performed from the
recording files.

All modules are implemented within the same command `vcr`.
See command line help for general information:

`vcr --help`.

See command line help for individual sub-commands (modules):

`vcr <sub-command> --help`.

## Capture module

`vcr capture ...`

Features:

- recording datagrams to file(s);
- support for UDP unicast and multicast;
- optional logging to syslog;
- optional logging to console;
- support for recording file rotation;
- dynamic reconfiguration via http;
- dynamic reconfiguration via re-reading configuration file (sigHUP);

This is a long-running module, assuring that all events are safely recorded.

### Configuration

(To be defined...)

## Server module

`vcr server ...`

Features:

- serving recording data over http;
- optional logging to syslog;
- optional logging to console;
- event filtering, to reduce required bandwidth;

This module handles sequential access to the recording files.

```
/recording/dir/files  --> server module --> http interface
```

A user of this module does not need to care for (or be aware of)
the fact that a recording is spread over several rotating files.
Instead, the timeline on the http interface appears continuously.

Event access principle introduces an *index*. This is a pointer
to the recording or a position in time.

Streaming data from http is a 2-step process:

- The user first requests `nextIndex`, based on requested *UTC* time.
  The server returns the *index* of the first event, matching this criterion.
- The user then requests a stream of `events`, starting from this index.
  The user is free to interrupt/pause the transfer at any point, resume
  the transfer or start the transfer process again.

## Custom module

`vcr custom ...`

Features:

- recompiles and runs custom *haskell* program in the *vcr* environment;

This module is required to handle a complicated configurations which
require a programming language. A *haskell* programming language
is used in this case.

An example includes a packet custom manipulation during packet replay.
The manipulation required is generally not known in advance, therefore
it can not be a part of the recorder's core.

The custom program has a full control over the program execution,
including the `main` function.

Running a custom program:

```bash
vcr custom --program "/path/to/program.hs --args..." \
    --ghcOpts "-i/path/to/other/includes"
```

## Supporting library

Supporting modules are available for writing custom *vcr* programs:

- `lib/Replay.hs`   - replay engine and GUI
- `lib/Time.hs`     - utc and monotonic time functions
- `lib/Udp.hs`      - UDP related functions
- `lib/Vcr.hs`      - definition of 'Event' data type

See documentation in `lib/` directory for details.

# Replay

A `vcr custom` module is used for the replay.
See `replay/example.hs` file.

# Testing tools

## `vcr-generator`

Features:

- command line tool
- generates UDP unicast or multicast
- generates simple sequence number in the first byte of the datagram
- configurable datagram size
- configurable multicast time-to-leave
- configurable packet rate

## `vcr-receiver`

Features:

- command line tool
- verifies sequence number from the datagram
- receives UDP unicast or multicast
- configurable statistic reporting interval

