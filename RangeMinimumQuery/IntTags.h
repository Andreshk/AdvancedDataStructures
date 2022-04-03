#pragma once
#include "TaggedInt.h"

// Not a scoped enum - will increase clutter (also see comment in TaggedInt.h)
enum Tag {
    ValueIdx,  // This is an index of a value
    RMQIdx,    // This is a 01RMQ-local index
    Depth,     // This is a cartesian tree depth
    LevelIdx,  // This is an index of a level
    BV,        // This is a bitvector
    Chunk,     // This is a chunk-local index
    ChunkIdx,  // This is an index of a chunk
    ChunkSize, // This is the size of a chunk (should be const)
};
