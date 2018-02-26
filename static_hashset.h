#pragma once
#include <vector>

// Hashing a static set of n values in
// O(n) storage with no collisions (!).
class static_hashset
{
    // All values should be in the range [0..p-1] and p must be a
    // prime number. Otherwise, the no-collision guarantee is waived.
    static const int p = 1'009;
    // c0 is the constant for the first-level hashing with n buckets
    const int c0, n;
    // For each bucket there is another 'c' used to hash within the bucket
    std::vector<int> c;
    // The n buckets are concatenated into a single vector. For any n,
    // this vector will have no more than 3n-2 elements => O(n) total storage
    std::vector<int> table;
    // The offsets, at which the buckets are placed inside the main vector.
    // The size of a bucket can be deduced from the offsets of that buckets and the next one.
    std::vector<int> offsets;

    // The simple hashing function: c is a precalculated constant,
    // m is the bucket size, and x is the element being hashed.
    static int h(int c, int m, int x) { return (((c*x) % p) % m); }

    // Selects an appropriate constant c, such that hashing values
    // into m buckets gives no more than maxColls collisions.
    int selectc(const std::vector<int>& values, const int m, const int maxColls)
    {
        const int n = int(values.size());
        // TO-DO: try c in a more randomized order (!)
        for (int c = 1; c < p; ++c)
        {
            int colls = 0;
            for (int i = 0; colls < maxColls && i < n; ++i)
                for (int j = i + 1; colls < maxColls && j < n; ++j)
                    if (h(c, m, values[i]) == h(c, m, values[j]))
                        ++colls; // no more cycling after we hit maxColls
            // if we did hit maxColls, continue to the next c
            if (colls < maxColls)
                return c;
        }
        return 42; // this should be unreachable
    }

    // Empty, "helper" constructor, which receives the number of values
    // as a separate argument and only initializes the member data.
    static_hashset(const std::vector<int>& values, int n)
        : c0{ selectc(values, n, n) } // c0 is expected to be found on the second try
        , n{ n }
        , c(n, -1) // careful with uniform initialization for certain vectors...
        , table(3 * n - 2, -1)
        , offsets(n + 1, -1)
    {}
public:
    // Do not construct with an empty vector!
    static_hashset(const std::vector<int>& values)
        : static_hashset(values, int(values.size()))
    {
        std::vector<std::vector<int>> buckets(n);
        for (int x : values)
            buckets[h(c0, n, x)].push_back(x);
        // Now each element of buckets contains the values, for
        // which the first-level hash function makes collisions.
        int currOff = 0;
        // This would almost certainly be inlined
        auto sq = [](int x) { return x*x; };
        for (int i = 0; i < n; ++i)
        {
            offsets[i] = currOff;
            // We can hash n values into n^2 buckets with
            // no collisions, given an appropriate c.
            const int wi2 = sq(int(buckets[i].size()));
            if (wi2 == 0)
                continue;
            // No guarantee how much time this would take...
            c[i] = selectc(buckets[i], wi2, 1);
            int* bucket = table.data() + currOff;
            for (int x : buckets[i])
                bucket[h(c[i], wi2, x)] = x;
            currOff += wi2; // the offset for the next bucket
        }
        offsets.back() = currOff;
    }

    bool find(int x) const
    {
        // Bucket index from the first-level hash function
        const int i = h(c0, n, x);
        // Size of the bucket - note that it may be empty (!)
        const int wi2 = offsets[i + 1] - offsets[i];
        return (wi2 && table[offsets[i] + h(c[i], wi2, x)] == x);
    }
};
