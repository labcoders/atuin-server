/*
BEGIN;
DROP TABLE IF EXISTS block_data;
DROP TABLE IF EXISTS block;
DROP TABLE IF EXISTS object;
DROP TABLE IF EXISTS chunk;

CREATE TABLE chunk (
    id SERIAL PRIMARY KEY,
    chunk_x INTEGER NOT NULL,
    chunk_y INTEGER NOT NULL,
    chunk_z INTEGER NOT NULL,
    chunk_type TEXT NOT NULL,
    UNIQUE (chunk_x, chunk_y, chunk_z)
);

CREATE TABLE object (
    id SERIAL PRIMARY KEY,
    chunk_id INTEGER REFERENCES chunk(id) NOT NULL,
    object_name TEXT NOT NULL
);

CREATE TABLE block (
    id SERIAL PRIMARY KEY,
    block_x INTEGER NOT NULL,
    block_y INTEGER NOT NULL,
    block_z INTEGER NOT NULL,
    chunk_id SERIAL REFERENCES chunk(id) NOT NULL,
    object_id INTEGER REFERENCES object(id),
    UNIQUE (block_x, block_y, block_z)
);

CREATE TABLE block_data (
    id SERIAL PRIMARY KEY,
    block_id SERIAL REFERENCES block(id),
    mod_name TEXT,
    block_name TEXT,
    block_data TEXT,
    UNIQUE (block_id)
);

COMMIT;
*/
