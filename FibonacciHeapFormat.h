#pragma once
#include <fmt/format.h>
#include "FibonacciHeap.h"

// Fibonacci heaps can be formatted either horizontally (by default)
// or vertically to accent the hierarchy, using the {:v} modifier:
//                                    [1]
// [1] [2 [3] [4* [5]]] [6 [7]]  <->  [2 [3]
//                                       [4* [5]]]
//                                    [6 [7]]
template <typename T>
struct fmt::formatter<FibonacciHeap<T>> {
    bool horizontal = true;

    template <typename ParseContext>
    constexpr auto parse(ParseContext& ctx) {
        auto it = ctx.begin(), end = ctx.end();
        if (it != end && *it == 'v') {
            horizontal = false;
            ++it;
        }
        if (it != end && *it != '}') { throw fmt::format_error("invalid format"); }
        return it;
    }
    template <typename FormatContext>
    auto format(const FibonacciHeap<T>& fh, FormatContext& ctx) {
        if (horizontal) {
            [&](this auto&& self, const DList<FNode<T>>& nodes, const bool b) -> void {
                bool first = true;
                for (const auto& n : nodes) {
                    if (b || !first) { *ctx.out()++ = ' '; }
                    first = false;
                    fmt::format_to(ctx.out(), "[{}", n.val);
                    if (n.marked) { *ctx.out()++ = '*'; }
                    self(n.subtrees, true);
                    *ctx.out()++ = ']';
                }
            }(fh.roots, false); // Immediately invoked recursive lambda, lol
        } else {
            [&](this auto&& self, const DList<FNode<T>>& nodes, const size_t pad) -> void {
                bool first = true;
                for (const auto& n : nodes) {
                    if (!first) { *ctx.out()++ = '\n'; }
                    fmt::format_to(ctx.out(), "{:>{}}{}", '[', (first ? 0 : pad) + 1, n.val);
                    if (n.marked) { *ctx.out()++ = '*'; }
                    const size_t newPad = fmt::formatted_size("{}", n.val);
                    if (!n.subtrees.empty()) {
                        *ctx.out()++ = ' ';
                        self(n.subtrees, pad + newPad + 2 + n.marked); // for the '[', ' ', and possibly '*' already printed
                    }
                    *ctx.out()++ = ']';
                    first = false;
                }
            }(fh.roots, 0); // Another one, lol
        }
        return ctx.out();
    }
};
