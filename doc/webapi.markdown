# Server http API

## GET /ping
Returns `pong`, just to check status.

## GET /info/channels
(optional timelimit t1, t2)
Return all distinct channels in the recording.

## GET /info/recorders
(optional timelimit t1, t2)
Return all distinct recorders.

## GET /info/oldest
## GET /info/youngest
(optional list of channels to consider)
Return UTC time of the oldest/youngest event in the recording.

## GET /events

Accept header:

    * application/json
    * application/bson
    * text/plain; charset=utf-8

query params:

    * pretty (bool) - pretty print output (for some contents)
    * t1 (start time) - t >= t1 (required),     format: YYYY-MM-DDTHH:MM:SS.SSSZ
    * t2 (end time) - t < t2 (required),        format: YYYY-MM-DDTHH:MM:SS.SSSZ
    * limit (n)     - max. number of rows returned (optional)
    * channel (name) - channel(s) to include
    * recorder (name) - recorder(s) to include

## PUT /events

Content types:
    * application/json
    * application/bson
    * text/plain; charset=utf-8

    [list of records]

## DELETE /events
query params: (see GET method)

## GET, DELETE /events/{hash}
Individual event by hash.
