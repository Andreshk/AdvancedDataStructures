#pragma once
#include "IntTags.h"

#ifndef DISABLE_TAGGED_INTS
// Offset an RMQ index of a chunk beginning with a chunk-local index
Int<RMQIdx> operator+(const Int<RMQIdx>& chunkStart, const Int<Chunk>& idx) {
    return Int<RMQIdx>(int(chunkStart) + int(idx));
}
// Get the chunk index for a given RMQ index
Int<ChunkIdx> operator/(const Int<RMQIdx>& idx, const Int<ChunkSize>& chunk) {
    return Int<ChunkIdx>(int(idx) / int(chunk));
}
// Get the chunk-local index for a given RMQ index
Int<Chunk> operator%(const Int<RMQIdx>& idx, const Int<ChunkSize>& chunk) {
    return Int<Chunk>(int(idx) % int(chunk));
}
// Get the index of a chunk's beginning by converting "back"
Int<RMQIdx> operator*(const Int<ChunkIdx>& idx, const Int<ChunkSize>& chunk) {
    return Int<RMQIdx>(int(idx) * int(chunk));
}
// By exception, chunk-local indices can be compared to a chunk size
bool operator==(const Int<Chunk>& idx, const Int<ChunkSize>& chunk) {
    return (int(idx) == int(chunk));
}
auto operator<=>(const Int<Chunk>& idx, const Int<ChunkSize>& chunk) {
    return (int(idx) <=> int(chunk));
}
// By exception, the chunk size can be converted to a chunk index, but not vice versa (!)
Int<Chunk> toIndex(const Int<ChunkSize>& chunk) { return Int<Chunk>(int(chunk)); } // Note: this cast to int is dangerous (!)
#else
#define toIndex(x) (x)
auto toIndex = [](auto x) { return x; }; // \x -> x
#endif // !DISABLE_TAGGED_INTS
