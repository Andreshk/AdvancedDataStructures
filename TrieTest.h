#pragma once
#include <fmt/core.h>
#include "Trie.h"
#include "CompressedTrie.h"

template <typename T>
void testTrie();

template <>
void testTrie<const char*>() {
    Trie<const char*> t;
	int numInserted = 0;
	for (const char* str : { "andi", "", "and", "andy", "baba" }) {
		if (t.insert(str)) {
			++numInserted;
		}
	}
	fmt::print("numInserted={}\ncount={}\n", numInserted, t.size());
	for (const char* str : t.values()) {
		fmt::print("\"{}\"\n", str);
	}
	for (const char* str : { "", "andi", "andrey", "ba", "c" }) {
		fmt::print("\"{}\" : {}\n", str, t.contains(str));
	}
	int numRemoved = 0;
	for (const char* str : { "andi", "", "and", "andy", "baba" }) {
		if (t.remove(str)) {
			++numRemoved;
		}
	}
	fmt::print("numRemoved={}\ncount={}\n", numRemoved, t.size());
	for (const char* str : t.values()) {
		fmt::print("\"{}\"\n", str);
	}
	fmt::print("\n");
}

template <>
void testTrie<uint64_t>() {
	Trie<uint64_t> t;
	int numInserted = 0;
	for (const uint64_t value : { 68, 20, 35, 32, 14, 0, 12, 20, 300, 0, 301, 420/*, -1, -2*/ }) {
		if (t.insert(value)) {
			++numInserted;
		}
	}
	fmt::print("numInserted={}\n", numInserted);
	fmt::print("count={} maxBits={}\n", t.size(), t.maxBits());
	for (const uint64_t value : t.values()) {
		// Print arg0 in binary, padding up to arg1 with zeroes; then print arg0 normally
		fmt::print("{0:0{1}b} ({0})\n", value, t.maxBits());
	}
	for (const uint64_t value : { 0, 5, 12, 20, 28, 68, 420 }) {
		fmt::print("{} : {}\n", value, t.contains(value));
	}
	int numRemoved = 0;
	for (const uint64_t value : { 68, 20, 35, 32, 14, 0, 12, 20, 300, 0, 301, 420/*, -1, -2*/ }) {
		if (t.remove(value)) {
			++numRemoved;
		}
	}
	fmt::print("numRemoved={}\n", numRemoved);
	fmt::print("count={} maxBits={}\n", t.size(), t.maxBits());
	for (const uint64_t value : t.values()) {
		// Print arg0 in binary, padding up to arg1 with zeroes; then print arg0 normally
		fmt::print("{0:0{1}b} ({0})\n", value, t.maxBits());
	}
	fmt::print("\n");
}

template <typename T>
void testTrieCompress();

template <>
void testTrieCompress<const char*>() {
	Trie<const char*> t{ "and", "ax", "", "bot", "v", "vw" };
	CompressedTrie<const char*> ct{ t };
	auto v1 = t.values();
	auto v2 = ct.values();
	for (auto it1 = v1.begin(), it2 = v2.begin(); it1 != v1.end() && it2 != v2.end(); ++it1, ++it2) {
		fmt::print("\"{}\" \"{}\"\n", *it1, *it2);
	}
	for (const char* str : { "", "and", "andi", "ax", "ay", "bo", "cc", "v", "vw" }) {
		fmt::print("\"{}\" : {} {}\n", str, t.contains(str), ct.contains(str));
	}
}
