CREATE TABLE streams(
    stream_id uuid NOT NULL PRIMARY KEY
);

CREATE TABLE events(
    stream_id uuid NOT NULL REFERENCES streams (stream_id),
    version integer NOT NULL DEFAULT 0,
    time_stamp timestamp NOT NULL DEFAULT now(),
    data json NOT NULL,
    PRIMARY KEY (stream_id, version)
);

CREATE FUNCTION publish_events()
RETURNS void AS
'   SELECT pg_notify(''events'', CONCAT(''['',version::varchar(255),'','',data #>> ''{}'','']'')) 
    FROM events
    ORDER BY time_stamp ASC;
' LANGUAGE sql STABLE;
