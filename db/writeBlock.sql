/* script for writing a block to the db */
BEGIN;
INSERT INTO chunk (chunk_x, chunk_y, chunk_z)
  VALUES (1, 2, 3)
ON CONFLICT (chunk_x, chunk_y, chunk_z) 
  DO NOTHING;

INSERT INTO block (block_x, block_y, block_z, chunk_id)
  VALUES (4,
          5,
          6,
          (SELECT id FROM chunk WHERE
             chunk_x = 1 AND
             chunk_y = 2 AND
             chunk_z = 3)
  )
ON CONFLICT (block_x, block_y, block_z) DO NOTHING;

INSERT INTO block_data (block_id, mod_name, block_name, block_data)
  VALUES (
    (SELECT id FROM block WHERE 
        block_x = 4 AND
        block_y = 5 AND
        block_z = 6),
    'mod',
    'air',
    '{key:oldValue}'
  )
ON CONFLICT (block_id) DO
  UPDATE SET
    mod_name='mod',
    block_name='air',
    block_data='{key:newValue}'
  WHERE block_data.block_id = (SELECT id FROM block WHERE
  block_x = 4 AND
  block_y = 5 AND
  block_z = 6);
COMMIT;
