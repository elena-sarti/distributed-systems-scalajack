-- Schema of the lobby database.
-- Every statement is idempotent (IF NOT EXISTS), so this script can be run at every start-up:
-- the first run creates the schema, the following ones change nothing and never touch the data.

CREATE TABLE IF NOT EXISTS users (
    id            BIGSERIAL   PRIMARY KEY,
    username      VARCHAR(32) NOT NULL,
    password_hash TEXT        NOT NULL,                   -- never the plain password
    created_at    TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- Usernames are unique ignoring the case: "Alice" and "alice" are the same user.
CREATE UNIQUE INDEX IF NOT EXISTS users_username_unique ON users (lower(username));

-- The money of every user, kept between sessions. A new user starts with no money and has to deposit some to play.
ALTER TABLE users ADD COLUMN IF NOT EXISTS balance NUMERIC(12, 2) NOT NULL DEFAULT 0;

ALTER TABLE users ALTER COLUMN balance SET DEFAULT 0;

-- The table where the user is sitting, NULL when none. A user sits at one table at a time, so two
-- tables never play with the same money and overwrite each other's balance.
ALTER TABLE users ADD COLUMN IF NOT EXISTS table_id TEXT;
