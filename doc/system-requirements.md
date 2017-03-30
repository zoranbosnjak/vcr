# VCR

VCR is a system for recording, storing and replaying events generated
by different sources.

## Basic definitions

A raw event is a sequence of bytes.

A stream of raw events is either a sequence of UDP (unicast or multicast)
packets where each packet represents one event or a file where events
are separated by a specified separator.

A source is a device or a program capable of producing a stream of raw events.

A channel is a symbol name given by the VCR system to a stream of raw
events produced by a certain source.  The stream of raw events
produced by a certain source may have different names within the VCR
system.

A (recorded) event is a raw event augmented with the following data:
* the channel symbolically identifying the source,
* the recording session,
* the UTC time and the monotonic time when the event was recorded by
  the VCR system.

## Main functions

### Storing events

Events recorderd by the VCR system must be stored in a database.

The system must support database replication where a specified number
of databases are used simultaneously so that every event is stored
into a specified number of these databases.  In other words, every
event must be kept stored in at least specified number of databases
but no individual database is required to keep a copy of all recorded
events.  If some database becomes unavailable, the VCR system must
ensure as soon as possible that available databases contain at least
specified number of copies of every recorded event.

The database subsystem must be able
* store replicated copies of events,
* mark and unmark events as quarantined or unquarantined,
* delete unquarantined events,
* return events recorded within a specified time interval.

TODO: A database becomes unavailable.  Replicas of events are
quarantined/deleted.  The databse becomes available again.  How do we
maintain consistency?

The raw events generated in a single day are estimated to amount to 1
Gbyte of data.  No significant bursts of events within short time
intervals are expected.

### Recording events



### Replaying events

## Supporting functions

### Monitoring

### Archiving

## Security

Security is not a major issue because of the followig reasons:

* the VCR system will be run on the intranet;

* the VCR system will be run by acredited engineers.