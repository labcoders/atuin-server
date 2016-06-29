BEGIN;
DROP TABLE IF EXISTS BlockData;
DROP TABLE IF EXISTS Block;
DROP TABLE IF EXISTS Object;
DROP TABLE IF EXISTS Chunk;

CREATE TABLE Chunk (
    id SERIAL PRIMARY KEY,
    chunk_x INTEGER NOT NULL,
    chunk_y INTEGER NOT NULL,
    chunk_z INTEGER NOT NULL,
    type TEXT,
    UNIQUE (chunk_x, chunk_y, chunk_z)
);

CREATE TABLE Object (
    id SERIAL PRIMARY KEY,
    chunk_id INTEGER REFERENCES Chunk(id) NOT NULL,
    object_name TEXT
);

CREATE TABLE Block (
    id SERIAL PRIMARY KEY,
    block_x INTEGER NOT NULL,
    block_y INTEGER NOT NULL,
    block_z INTEGER NOT NULL,
    chunk_id SERIAL REFERENCES Chunk(id) NOT NULL,
    object_id INTEGER REFERENCES Object(id),
    UNIQUE (block_x, block_y, block_z)
);

CREATE TABLE BlockData (
    id SERIAL PRIMARY KEY,
    block_id SERIAL REFERENCES Block(id),
    mod_name TEXT,
    block_name TEXT,
    block_data TEXT,
    UNIQUE (block_id)
);

COMMIT;
