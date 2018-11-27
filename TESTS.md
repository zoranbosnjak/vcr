# automatic unit tests

```bash
nix-shell
rm -rf dist/
cabal configure --enable-tests
cabal build -j
./dist/build/test-vcr/test-vcr
```

# manual test scenarios

Prepare:

```bash
mkdir recording
mkdir overflow
watch ls -l recording/ overflow/

watch "date | socat - udp-sendto:127.0.0.1:56001"

export db="host=<ip> dbname=<name> user=<name>"
ln -s dist/build/vcr/vcr vcr
```

## basic

Use `vcr record args` as basic command...

- console logging
- syslog
- ekg monitor
- memory limit, for example, run with `+RTS -M1m -RTS`, then increase

## vcr record

- record `stdin` -> `stdout`
```bash
date | ./vcr record --stdin test --stdout args
```

- udp input (unicast and multicast)
```bash
export localIp=<ip>
export mcast=<mcast>
./vcr record --stdout args udpin --channel "ch1" --ip "127.0.0.1" --port "56001" udpin --channel "ch2" --ip "$ip" --port 56002 --multicast "$mcast"
```

- basic file output and rotation
```bash
./vcr -v INFO record args udpin --channel "test" --ip "127.0.0.1" --port "56001" fileout --path recording/recording --json --rotateKeep 3 --rotateSize 100M --rotateTime 10s
```

- prepare postgres database
```bash
# in psql... "drop table events;"
./vcr record prepare postgres --connect "$db"
```

- prepare sqlite database
```bash
rm test.db
./vcr record prepare sqlite --filename test.db
```

- record to a postgres database, drop to file, http server

```bash
./vcr -v INFO record http args udpin --channel "test" --ip "127.0.0.1" --port "56001" databaseout --dropFile --path overflow/overflow --json --rotateKeep 3 --rotateTime 10 --dropAt 10 --sendEvents 5 postgres --connect "$db"
# count events in database "select count (*) from events;"

service postgresql stop
# wait until overflow/* file(s)

service postgresql start
# auto reconnect
```

- record to a sqlite database
```bash
./vcr -v INFO record http args udpin --channel "test" --ip "127.0.0.1" --port "56001" databaseout --dropFile --path overflow/overflow --json --rotateKeep 3 --rotateTime 10 --dropAt 10 --sendEvents 5 sqlite --filename test.db
sqlite3 test.db "select count(*) from events;"
```

- bootstrap arguments, prepare config file, run with config file
```bash
./vcr record http args udpin --channel "ch1" --ip "127.0.0.1" --port "56001" udpin --channel "ch2" --ip "127.0.0.1" --port "56002" databaseout --dropFile --path overflow/overflow --json --rotateKeep 3 --rotateSize 1M --rotateTime 10 --dropAt 10 --sendEvents 5 postgres --connect "$db" fileout --path recording/recording --json --rotateKeep 10 --rotateSize 100M --rotateTime 1day --bootstrap | tee vcr.config

# dump config file
./vcr record dump vcr.config

# run vcr record using config file
./vcr -v INFO record http config vcr.config
```

- dynamic reconfiguration

    - with high load
    - change something in configuration
        - database parameters
        - output file parameters
        - add udp input
        - remove udp input
    - send signal to the process: `kill -s SIGHUP <pid>`
    - for each reconfiguration, verify that no event had been lost
      in the database or in the recording files(s)

- long term run, without memory leak

    - prepare config file
    - setup high load
    - record to a file and to postgres database
    - run with memory limitation
    - observe ekg monitor
    - check uptime in syslog

## vcr archive

- file1 -> file2 (same encoding, expect no diff)

- file1 -> file2 (other encoding) -> file3 (same encoding as file1)
    Expect no diff between file1 and file3

- postgres -> sqlite

- sqlite -> postgres

- postgres -> file

- sqlite -> file

- check with inserted encoding error in the file

- check with inserted sequence error in the file or database

- check with inserted monotonic time error in the file or database

