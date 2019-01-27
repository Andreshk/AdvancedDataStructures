#pragma once
#include <vector>
#include <string>
#include <limits>    // std::numeric_limits::{min,max}
#include <algorithm> // std::sort
#include <string_view>
#include "bitvector.h" // use this from the Andreshk/SnippySnippets repo

enum class SuffixMode { Naive, ManberMyers, DC3Skew, SAIS_Original, SAIS_Yuta, InPlace };

template<SuffixMode mode>
std::vector<size_t> buildSuffixArray(std::string_view);

template<> // O(n^2 * lgn) due to slow comparison of substrings (think strcmp)
std::vector<size_t> buildSuffixArray<SuffixMode::Naive>(std::string_view str) {
    const size_t n = str.size();
    std::vector<size_t> SA(n);
    for (size_t i = 0; i < n; ++i)
        SA[i] = i;
    std::sort(SA.begin(), SA.end(), [=](size_t i, size_t j) { return (str.substr(i) < str.substr(j)); });
    return SA;
}

template<> // O(nlgn)
std::vector<size_t> buildSuffixArray<SuffixMode::ManberMyers>(std::string_view str) {
    const size_t n = str.size();
    std::vector<size_t> SA(n), iSA(n), count(n), next(n);
    BitVector bh(n), b2h(n);
    // Sort the suffixes by their first character
    for (size_t i = 0; i < n; ++i)
        SA[i] = i;
    std::sort(SA.begin(), SA.end(), [=](size_t i, size_t j) {return str[i] < str[j]; });
    // Mark the beginnings of each bucket
    bh.set(0);
    for (size_t i = 1; i < n; ++i)
        if (str[SA[i]] != str[SA[i - 1]])
            bh.set(i);
    
    // Prefix-doubling: sort the suffixes by a twice the amount of characters
    for (size_t h = 1; h < n; h <<= 1) {
        // bh[i] == false if the first h characters of SA[i-1] == the first h characters of SA[i]
        size_t buckets = 0;
        for (size_t i = 0, j; i < n; i = j) {
            j = i + 1;
            while (j < n && !bh[j])
                j++;
            next[i] = j;
            ++buckets;
        }
        if (buckets == n)
            break; // Shorter prefixes turned out to be enough for sorting
        // Suffixes are now separated in buckets by their first h characters
        for (size_t i = 0; i < n; i = next[i]) {
            count[i] = 0;
            for (size_t j = i; j < next[i]; ++j) {
                iSA[SA[j]] = i;
            }
        }
        // Here be magic
        count[iSA[n - h]]++;
        b2h.set(iSA[n - h]);
        for (size_t i = 0; i < n; i = next[i]) {
            for (size_t j = i; j < next[i]; ++j)
                if (SA[j] >= h) {
                    const size_t s = SA[j] - h;
                    const size_t head = iSA[s];
                    iSA[s] = head + count[head]++;
                    b2h.set(iSA[s]);
                }
            for (size_t j = i; j < next[i]; ++j)
                if (SA[j] >= h) {
                    const size_t s = SA[j] - h;
                    if (b2h[iSA[s]]) {
                        for (size_t k = iSA[s] + 1; k < n && !bh[k] && b2h[k]; k++)
                            b2h.clear(k);
                    }
                }
        }
        // Update the arrays with the new values
        bh |= b2h;
        for (size_t i = 0; i < n; ++i)
            SA[iSA[i]] = i;
    }
    /*for (size_t i = 0; i < n; ++i)
        iSA[SA[i]] = i;*/

    return SA;
}

namespace DC3Skew_private { // Helper functions for the DC3/Skew algorithm
// Lexicographic order for pairs and triples
bool leq(size_t a1, size_t a2, size_t b1, size_t b2) {
    return(a1 < b1 || a1 == b1 && a2 <= b2);
}
bool leq(size_t a1, size_t a2, size_t a3, size_t b1, size_t b2, size_t b3) {
    return(a1 < b1 || a1 == b1 && leq(a2, a3, b2, b3));
}
// Stably sort a[0..n-1] to b[0..n-1] with keys in {0..K} from r
void radixPass(const std::vector<size_t>& a, std::vector<size_t>& b, const size_t* const r, const size_t n, const size_t K) {
    std::vector<size_t> count(K + 1, 0);
    for (size_t i = 0; i < n; i++) // Count occurrences of each 'character'
        count[r[a[i]]]++;
    for (size_t i = 0, sum = 0; i <= K; i++) { // build the exclusive prefix sums
        const size_t t = count[i];
        count[i] = sum;
        sum += t;
    }
    for (size_t i = 0; i < n; i++) // Sort
        b[count[r[a[i]]]++] = a[i];
}
// Build (in SA) the suffix array of str[0..n-1], where all elements of str are in {1..K}.
// Requires str to be padded with three zeroes: str[n]=str[n+1]=str[n+2]=0, and n>=2
void suffixArray(const std::vector<size_t>& str, std::vector<size_t>& SA, const size_t n, const size_t K) {
    const size_t n0 = (n + 2) / 3, n1 = (n + 1) / 3, n2 = n / 3, n02 = n0 + n2; // note: n0 >= n1
    std::vector<size_t> str12(n02 + 3, 0);  // The mod1 and mod2 triples, numbered according to their lexicographic ordering
    std::vector<size_t> SA12(n02 + 3, 0); // The suffix array for the aforementioned triples
    // Generate positions of mod1 and mod2 suffixes
    // the "+(n0-n1)" adds a dummy mod1 suffix if n%3 == 1
    for (size_t i = 0, j = 0; i < n + (n0 - n1); i++)
        if (i % 3 != 0)
            str12[j++] = i;
    // LSB radix sort the mod1 and mod2 triples
    radixPass(str12, SA12, &str[2], n02, K);
    radixPass(SA12, str12, &str[1], n02, K);
    radixPass(str12, SA12, &str[0], n02, K);
    // Find lexicographic "names" (identifiers) of triples
    size_t names = 0, c0 = -1, c1 = -1, c2 = -1;
    for (size_t i = 0; i < n02; i++) {
        if (str[SA12[i]] != c0 || str[SA12[i] + 1] != c1 || str[SA12[i] + 2] != c2) {
            ++names;
            c0 = str[SA12[i]];
            c1 = str[SA12[i] + 1];
            c2 = str[SA12[i] + 2];
        }
        if (SA12[i] % 3 == 1) { // left half
            str12[SA12[i] / 3] = names;
        } else { // right half
            str12[SA12[i] / 3 + n0] = names;
        }
    }
    // Recurse if the names are not yet unique
    if (names < n02) {
        suffixArray(str12, SA12, n02, names);
        // Store unique names in s12 using the suffix array
        for (size_t i = 0; i < n02; i++)
            str12[SA12[i]] = i + 1;
    } else { // otherwise, generate the suffix array of s12 directly
        for (size_t i = 0; i < n02; i++)
            SA12[str12[i] - 1] = i;
    }
    // Stably sort the mod0 suffixes from SA12 by their first character
    std::vector<size_t> str0(n0), SA0(n0);
    for (size_t i = 0, j = 0; i < n02; i++)
        if (SA12[i] < n0)
            str0[j++] = 3 * SA12[i];
    radixPass(str0, SA0, &str[0], n0, K);
    // Finally, merge the sorted mod0 suffixes and sorted mod12 suffixes
    for (size_t p = 0, t = n0 - n1, k = 0; k < n; k++) {
#define GetI() (SA12[t] < n0 ? SA12[t]*3+1: (SA12[t] - n0) * 3 + 2)
        size_t i = GetI(); // pos of current offset 12 suffix
        size_t j = SA0[p]; // pos of current offset 0 suffix
        if (SA12[t] < n0 ? // different compares for mod1 and mod2 suffixes
            leq(str[i], str12[SA12[t] + n0], str[j], str12[j / 3]) :
            leq(str[i], str[i + 1], str12[SA12[t] - n0 + 1], str[j], str[j + 1], str12[j / 3 + n0]))
        { // suffix from SA12 is smaller
            SA[k] = i;
            ++t;
            if (t == n02) // ...done -> only SA0 suffixes left
                for (k++; p < n0; p++, k++)
                    SA[k] = SA0[p];
        } else {// suffix from SA0 is smaller
            SA[k] = j;
            ++p;
            if (p == n0) // ...done -> only SA12 suffixes left
                for (k++; t < n02; t++, k++)
                    SA[k] = GetI();
        }
#undef GetI
    }
}
} // namespace DC3Skew_private

template<> // O(n)
std::vector<size_t> buildSuffixArray<SuffixMode::DC3Skew>(std::string_view str) {
    const size_t n = str.size();
    std::vector<size_t> str3(n + 3, 0);
    for (size_t i = 0; i < n; ++i)
        str3[i] = str[i];
    std::vector<size_t> SA(n);
    DC3Skew_private::suffixArray(str3, SA, n, std::numeric_limits<char>::max());
    return SA;
}

#include "suffix_array_impl.h"
template<> // O(n)
std::vector<size_t> buildSuffixArray<SuffixMode::SAIS_Original>(std::string_view str) {
    const size_t n = str.size();
    // str is assumed to not be zero-terminated - otherwise, you can skip this copying
    std::vector<char> str1(n + 1, 0);
    for (size_t i = 0; i < n; ++i)
        str1[i] = str[i];
    std::vector<int> SA(n + 1, 0);
    sais_original_private::suffixsort(str1.data(), SA.data(), n + 1, std::numeric_limits<char>::max());
    return { SA.begin() + 1,SA.end() };
}

/**
 * @brief Constructs the suffix array of a given string in linear time.
 * @note Requires T[n-1] to be a unique character, lexicographically smaller than every other in T.
 * @param T[0..n-1] The input string.
 * @return SA[0..n-1] The output array of suffixes.
 */
template<> // O(n)
std::vector<size_t> buildSuffixArray<SuffixMode::SAIS_Yuta>(std::string_view str) {
    const size_t n = str.size();
    if (n > 1) {
        using IdxT = std::make_signed_t<size_t>;
        std::vector<size_t> SA(n, 0);
        const IdxT pidx = saisxx_private::suffixsort(str.data(), reinterpret_cast<IdxT*>(SA.data()), IdxT(0),
                                                     IdxT(n), IdxT(std::numeric_limits<char>::max()), false);
        if (pidx >= 0)
            return SA;
        else
            return {};
    } else if (n == 1) {
        return { 0 };
    } else {
        return {};
    }
}

/**
 * @brief Constructs the burrows-wheeler transformed string of a given string in linear time.
 * @note Requires T[n-1] to be a unique character, lexicographically smaller than every other in T.
 * @param T[0..n-1] The input string.
 * @return BWT[0..n-1] The output string.
 */
std::string buildBWT(std::string_view str) {
    const size_t n = str.size();
    if (n > 1) {
        using IdxT = std::make_signed_t<size_t>;
        std::vector<IdxT> A(n, 0); // Temporary array
        const IdxT pidx = saisxx_private::suffixsort(str.data(), A.data(), IdxT(0), IdxT(n),
                                                     IdxT(std::numeric_limits<char>::max()), true);
        if (pidx >= 0) {
            std::string BWT(n, '\0'); // TO-DO: reuse the memory for A
            for (IdxT i = 0; i < pidx; ++i)
                BWT[i] = char(A[i]);
            BWT[pidx] = str[n - 1];
            for (IdxT i = pidx + 1; i < IdxT(n); ++i)
                BWT[i] = char(A[i]);
            return BWT;
        } else {
            return {};
        }
    } else if (n == 1) {
        return { str[0] };
    } else {
        return {};
    }
}
