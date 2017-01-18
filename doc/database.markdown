# database tables

## events table

    * hash    - VARCHAR(255), prim key
    * ch      - VARCHAR(255)
    * srcId   - VARCHAR(255)
    * utcTime - DATETIME
    * sesId   - VARCHAR(255)
    * monoTime - BIGINT (nanoseconds since some time in the past)
    * value   - BLOB

## selections table
(subset of events, according to some criteria)

    * setName - VARCHAR(255)
    * event - foreign (events)

## backends table

    * server name - VARCHAR(255)
    * active  - BOOL

## replicas table

    * event   - foreign (events)
    * srv     - foreign (stores)

## derivedEvents table
    
(to be defined)
