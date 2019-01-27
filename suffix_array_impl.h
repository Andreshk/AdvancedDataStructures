#pragma once
#include <limits> // std::numeric_limits::{min,max}
#include "bitvector.h" // use this from the Andreshk/SnippySnippets repo

/* 
 * This is an adaptation of the SAIS library by Yuta Mori: https://sites.google.com/site/yuta256/sais
 * All credit should therefore go to the original author, for his incredible implementations
 * of the Induced-Sorting suffix array algorithm by Ge Nong, Sen Zhang & Wai Hong Chan (2009).
 */

namespace sais_original_private {
// Find the start or end of each bucket
template<class CharT>
void getBuckets(const CharT* const str, const size_t n, const size_t K, std::vector<size_t>& bkt, const bool end) {
    bkt.assign(K + 1, 0);
    for (size_t i = 0; i < n; i++) // compute the size of each bucket
        ++bkt[str[i]];
    for (size_t i = 0, sum = 0; i <= K; i++) {
        sum += bkt[i];
        bkt[i] = (end ? sum : sum - bkt[i]);
    }
}

// compute SAl
template<class CharT>
void induceSAl(const CharT* const str, int* const SA, const size_t n,
               const size_t K, const BitVector& ls, std::vector<size_t>& bkt) {
    getBuckets(str, n, K, bkt, false); // find bucket starts
    for (size_t i = 0; i < n; i++)
        if (const size_t j = SA[i] - 1; SA[i] >= 1 && !ls[j])
            SA[bkt[str[j]]++] = int(j);
}

// compute SAs
template<class CharT>
void induceSAs(const CharT* const str, int* const SA, const size_t n,
               const size_t K, const BitVector& ls, std::vector<size_t>& bkt) {
    getBuckets(str, n, K, bkt, true); // find bucket ends
    for (size_t i = n - 1; i < n; i--) // i < n instead of i >= 0
        if (const size_t j = SA[i] - 1; SA[i] >= 1 && ls[j])
            SA[--bkt[str[j]]] = int(j);
}

// Build (in SA) the suffix array of str[0..n-1], where all elements of str are in {1..K}.
// Requires str[n-1]=0 (the sentinel!), n>=2. Uses a working space (excluding str and SA) of at most 2.25n+O(1)
template<class CharT>
void suffixsort(const CharT* const str, int* const SA, const size_t n, const size_t K) {
    BitVector ls(n); // LS-type array in bits
    auto isLMS = [&](const size_t i) { return (i > 0 && ls[i] && !ls[i - 1]); }; // pls inline this

    // Classify the type of each character
    ls.set(n - 1); // the sentinel must be in str1, important!!!
    for (size_t i = n - 3; i < n; i--) // i < n instead of i >= 0
        if (str[i] < str[i + 1] || (str[i] == str[i + 1] && ls[i + 1] == 1))
            ls.set(i);

    // Stage 1: reduce the problem by at least 1/2.
    // sort all the S-substrings
    std::vector<size_t> bkt(K + 1); // bucket array
    getBuckets(str, n, K, bkt, true); // find ends of buckets
    for (size_t i = 0; i < n; i++)
        SA[i] = -1;
    for (size_t i = 1; i < n; i++)
        if (isLMS(i))
            SA[--bkt[str[i]]] = int(i);

    induceSAl(str, SA, n, K, ls, bkt);
    induceSAs(str, SA, n, K, ls, bkt);

    // Compact all the sorted substrings into the first n1 items of SA
    // 2*n1 will never be larger than n (proveable)
    size_t n1 = 0;
    for (size_t i = 0; i < n; i++)
        if (isLMS(SA[i]))
            SA[n1++] = SA[i];

    // Find the lexicographic names of all substrings
    for (size_t i = n1; i < n; i++)
        SA[i] = -1; // reset the name array buffer
    int name = 0, prev = -1;
    for (size_t i = 0; i < n1; i++) {
        int pos = SA[i];
        bool diff = false;
        for (size_t d = 0; d < n; d++)
            if (prev == -1 || str[pos + d] != str[prev + d] || ls[pos + d] != ls[prev + d]) {
                diff = true;
                break;
            } else if (d > 0 && (isLMS(pos + d) || isLMS(prev + d)))
                break;
        if (diff) {
            ++name;
            prev = pos;
        }
        pos = ((pos % 2) ? (pos - 1) / 2 : pos / 2);
        SA[n1 + pos] = name - 1;
    }
    for (size_t i = n - 1, j = n - 1; i + 1 > n1; i--)
        if (SA[i] >= 0)
            SA[j--] = SA[i];

    // Stage 2: solve the reduced problem.
    int* const SA1 = SA;
    int* const str1 = SA + n - n1;
    if (name < n1) // Recurse if names are not yet unique
        suffixsort(str1, SA1, n1, name - 1);
    else // otherwise, generate the suffix array of str1 directly
        for (size_t i = 0; i < n1; i++)
            SA1[str1[i]] = int(i);

    // Stage 3: induce the result for the original problem.
    // Put all left-most S characters into their buckets
    getBuckets(str, n, K, bkt, true); // find ends of buckets
    for (size_t i = 1, j = 0; i < n; i++) // get p1
        if (isLMS(i))
            str1[j++] = int(i);
    for (size_t i = 0; i < n1; i++) // get index in str
        SA1[i] = str1[SA1[i]];
    for (size_t i = n1; i < n; i++) // init SA[n1..n-1]
        SA[i] = -1;
    for (size_t i = n1 - 1; i < n; i--) {
        const int j = SA[i];
        SA[i] = -1;
        SA[--bkt[str[j]]] = j;
    }
    induceSAl(str, SA, n, K, ls, bkt);
    induceSAs(str, SA, n, K, ls, bkt);
}
} // namespace SAIS_Original_private

namespace saisxx_private {

/* find the start or end of each bucket */
template<class CharT, class IdxT>
void getCounts(const CharT* const T, IdxT* const C, const IdxT n, const IdxT k) {
    for (IdxT i = 0; i < k; ++i)
        C[i] = 0;
    for (IdxT i = 0; i < n; ++i)
        ++C[T[i]];
}

template<class IdxT>
void getBuckets(const IdxT* const C, IdxT* const B, const IdxT k, const bool end) {
    IdxT sum = 0;
    if (end) {
        for(IdxT i = 0; i < k; ++i) {
            sum += C[i];
            B[i] = sum;
        }
    } else {
        for (IdxT i = 0; i < k; ++i) {
            sum += C[i];
            B[i] = sum - C[i];
        }
    }
}

template<class CharT, class IdxT>
void LMSsort1(const CharT* const T, IdxT* const SA, IdxT* const C, IdxT* const B,
              const IdxT n, const IdxT k, const bool recount) {
    IdxT* b;
    IdxT i, j;
    CharT c0, c1;

    /* compute SAl */
    if (recount) {
        getCounts(T, C, n, k);
    }
    getBuckets(C, B, k, false); /* find starts of buckets */
    j = n - 1;
    b = SA + B[c1 = T[j]];
    --j;
    *b++ = (T[j] < c1) ? ~j : j;
    for (i = 0; i < n; ++i) {
        if (0 < (j = SA[i])) {
            //assert(T[j] >= T[j + 1]);
            if ((c0 = T[j]) != c1) {
                B[c1] = IdxT(b - SA);
                b = SA + B[c1 = c0];
            }
            //assert(i < (b - SA));
            --j;
            *b++ = (T[j] < c1) ? ~j : j;
            SA[i] = 0;
        } else if (j < 0) {
            SA[i] = ~j;
        }
    }
    /* compute SAs */
    if (recount) {
        getCounts(T, C, n, k);
    }
    getBuckets(C, B, k, true); /* find ends of buckets */
    for (i = n - 1, b = SA + B[c1 = 0]; 0 <= i; --i) {
        if(0 < (j = SA[i])) {
            //assert(T[j] <= T[j + 1]);
            if((c0 = T[j]) != c1) {
                B[c1] = IdxT(b - SA);
                b = SA + B[c1 = c0];
            }
            //assert((b - SA) <= i);
            --j;
            *--b = (T[j] > c1) ? ~(j + 1) : j;
            SA[i] = 0;
        }
    }
}

template<class CharT, class IdxT>
IdxT LMSpostproc1(const CharT* const T, IdxT* const SA, const IdxT n, const IdxT m) {
    IdxT i, j, p, q, plen, qlen, name;
    CharT c0, c1;
    bool diff;

    /* compact all the sorted substrings into the first m items of SA
        2*m will never be larger than n (proveable) */
    //assert(0 < n);
    for (i = 0; (p = SA[i]) < 0; ++i) {
        SA[i] = ~p;
        //assert((i + 1) < n);
    }
    if (i < m)
        for (j = i, ++i;; ++i) {
            //assert(i < n);
            if ((p = SA[i]) < 0) {
                SA[j++] = ~p; SA[i] = 0;
                if (j == m)
                    break;
            }
        }

    /* store the length of all substrings */
    i = n - 1; j = n - 1; c0 = T[n - 1];
    do { c1 = c0; } while((0 <= --i) && ((c0 = T[i]) >= c1));
    while (0 <= i) {
        do { c1 = c0; } while ((0 <= --i) && ((c0 = T[i]) <= c1));
        if (0 <= i) {
            SA[m + ((i + 1) >> 1)] = j - i; j = i + 1;
            do { c1 = c0; } while ((0 <= --i) && ((c0 = T[i]) >= c1));
        }
    }

    /* find the lexicographic names of all substrings */
    for (i = 0, name = 0, q = n, qlen = 0; i < m; ++i) {
        p = SA[i], plen = SA[m + (p >> 1)], diff = true;
        if ((plen == qlen) && ((q + plen) < n)) {
            for (j = 0; (j < plen) && (T[p + j] == T[q + j]); ++j) {}
            if (j == plen)
                diff = false;
        }
        if (diff) {
            ++name;
            q = p;
            qlen = plen;
        }
        SA[m + (p >> 1)] = name;
    }
    return name;
}

template<class CharT, class IdxT>
void LMSsort2(const CharT* const T, IdxT* const SA, IdxT* const C, IdxT* const B,
              IdxT* const D, const IdxT n, const IdxT k) {
    IdxT* b;
    IdxT i, j, t, d;
    CharT c0, c1;

    /* compute SAl */
    getBuckets(C, B, k, false); /* find starts of buckets */
    j = n - 1;
    b = SA + B[c1 = T[j]];
    --j;
    t = (T[j] < c1);
    j += n;
    *b++ = (t & 1) ? ~j : j;
    for (i = 0, d = 0; i < n; ++i) {
        if ((j = SA[i]) > 0) {
            if (n <= j) {
                d += 1;
                j -= n;
            }
            //assert(T[j] >= T[j + 1]);
            if ((c0 = T[j]) != c1) {
                B[c1] = IdxT(b - SA);
                b = SA + B[c1 = c0];
            }
            //assert(i < (b - SA));
            --j;
            t = c0;
            t = (t << 1) | (T[j] < c1);
            if (D[t] != d) {
                j += n;
                D[t] = d;
            }
            *b++ = (t & 1) ? ~j : j;
            SA[i] = 0;
        } else if (j < 0) {
            SA[i] = ~j;
        }
    }
    for (i = n - 1; i >= 0; --i) {
        if (SA[i] > 0) {
            if (SA[i] < n) {
                SA[i] += n;
                for (j = i - 1; SA[j] < n; --j) {}
                SA[j] -= n;
                i = j;
            }
        }
    }

    /* compute SAs */
    getBuckets(C, B, k, true); /* find ends of buckets */
    for (i = n - 1, d += 1, b = SA + B[c1 = 0]; i >= 0; --i) {
        if ((j = SA[i]) > 0) {
            if (n <= j) {
                d += 1;
                j -= n;
            }
            //assert(T[j] <= T[j + 1]);
            if ((c0 = T[j]) != c1) {
                B[c1] = IdxT(b - SA);
                b = SA + B[c1 = c0];
            }
            //assert((b - SA) <= i);
            --j;
            t = c0;
            t = (t << 1) | (T[j] > c1);
            if (D[t] != d) {
                j += n;
                D[t] = d;
            }
            *--b = (t & 1) ? ~(j + 1) : j;
            SA[i] = 0;
        }
    }
}

template<class IdxT>
IdxT LMSpostproc2(IdxT* const SA, IdxT const n, IdxT const m) {
    IdxT i, j, d, name;

    /* compact all the sorted LMS substrings into the first m items of SA */
    //assert(0 < n);
    for (i = 0, name = 0; (j = SA[i]) < 0; ++i) {
        j = ~j;
        if(n <= j)
            ++name;
        SA[i] = j;
        //assert((i + 1) < n);
    }
    if (i < m) {
        for (d = i, ++i;; ++i) {
            //assert(i < n);
            if ((j = SA[i]) < 0) {
                j = ~j;
                if (n <= j)
                    ++name;
                SA[d++] = j;
                SA[i] = 0;
                if (d == m)
                    break;
            }
        }
    }
    if(name < m) { /* store the lexicographic names */
        for(i = m - 1, d = name + 1; i >= 0; --i) {
            if(n <= (j = SA[i])) {
                j -= n;
                --d;
            }
            SA[m + (j >> 1)] = d;
        }
    } else { /* unset flags */
        for (i = 0; i < m; ++i) {
            if (n <= (j = SA[i])) {
                j -= n;
                SA[i] = j;
            }
        }
    }
    return name;
}

/* compute SA and BWT */
template<class CharT, class IdxT>
void induceSA(const CharT* const T, IdxT* const SA, IdxT* const C, IdxT* const B,
              const IdxT n, const IdxT k, const bool recount) {
    IdxT* b;
    IdxT i, j;
    CharT c0, c1;
    /* compute SAl */
    if(recount)
        getCounts(T, C, n, k);
    getBuckets(C, B, k, false); /* find starts of buckets */
    b = SA + B[c1 = T[j = n - 1]];
    *b++ = ((0 < j) && (T[j - 1] < c1)) ? ~j : j;
    for (i = 0; i < n; ++i) {
        j = SA[i], SA[i] = ~j;
        if (0 < j) {
            if ((c0 = T[--j]) != c1) {
                B[c1] = IdxT(b - SA);
                b = SA + B[c1 = c0];
            }
            *b++ = ((0 < j) && (T[j - 1] < c1)) ? ~j : j;
        }
    }
    /* compute SAs */
    if(recount)
        getCounts(T, C, n, k);
    getBuckets(C, B, k, true); /* find ends of buckets */
    for (i = n - 1, b = SA + B[c1 = 0]; 0 <= i; --i) {
        if (0 < (j = SA[i])) {
            if ((c0 = T[--j]) != c1) {
                B[c1] = IdxT(b - SA);
                b = SA + B[c1 = c0];
            }
            *--b = ((j == 0) || (T[j - 1] > c1)) ? ~j : j;
        } else {
            SA[i] = ~j;
        }
    }
}

template<class CharT, class IdxT>
IdxT computeBWT(const CharT* const T, IdxT* const SA, IdxT* const C, IdxT* const B,
                const IdxT n, const IdxT k, const bool recount) {
    IdxT* b;
    IdxT i, j, pidx = -1;
    CharT c0, c1;
    /* compute SAl */
    if(recount)
        getCounts(T, C, n, k);
    getBuckets(C, B, k, false); /* find starts of buckets */
    b = SA + B[c1 = T[j = n - 1]];
    *b++ = ((0 < j) && (T[j - 1] < c1)) ? ~j : j;
    for (i = 0; i < n; ++i) {
        if (0 < (j = SA[i])) {
            SA[i] = ~(IdxT(c0 = T[--j]));
            if (c0 != c1) {
                B[c1] = IdxT(b - SA);
                b = SA + B[c1 = c0];
            }
            *b++ = ((0 < j) && (T[j - 1] < c1)) ? ~j : j;
        } else if (j != 0) {
            SA[i] = ~j;
        }
    }
    /* compute SAs */
    if(recount != false) {
        getCounts(T, C, n, k);
    }
    getBuckets(C, B, k, true); /* find ends of buckets */
    for (i = n - 1, b = SA + B[c1 = 0]; 0 <= i; --i) {
        if (0 < (j = SA[i])) {
            SA[i] = (c0 = T[--j]);
            if (c0 != c1) {
                B[c1] = IdxT(b - SA);
                b = SA + B[c1 = c0];
            }
            *--b = ((0 < j) && (T[j - 1] > c1)) ? ~((IdxT)T[j - 1]) : j;
        } else if (j != 0) {
            SA[i] = ~j;
        } else {
            pidx = i;
        }
    }
    return pidx;
}

template<class CharT, class IdxT>
std::pair<IdxT, IdxT> stage1sort(const CharT* const T, IdxT* const SA, IdxT* const C, IdxT* const B,
                                 const IdxT n, const IdxT k, const unsigned flags) {
    IdxT* b;
    IdxT i, j, name, m;
    CharT c0, c1;

    getCounts(T, C, n, k);
    getBuckets(C, B, k, true); /* find ends of buckets */
    for (i = 0; i < n; ++i)
        SA[i] = 0;
    b = SA + n - 1;
    i = n - 1;
    j = n;
    m = 0;
    c0 = T[n - 1];
    do { c1 = c0; } while((--i >= 0) && ((c0 = T[i]) >= c1));
    while (i >= 0) {
        do { c1 = c0; } while ((--i >= 0) && ((c0 = T[i]) <= c1));
        if (i >= 0) {
            *b = j;
            b = SA + --B[c1];
            j = i;
            ++m;
            //assert(B[c1] != (n - 1));
            do { c1 = c0; } while ((--i >= 0) && ((c0 = T[i]) >= c1));
        }
    }
    SA[n - 1] = 0;

    if (m > 1) {
        if (flags & (16 | 32)) {
            //assert((j + 1) < n);
            ++B[T[j + 1]];
            if (flags & 16) {
                IdxT* const D = new(std::nothrow) IdxT[k * 2];
                if (!D)
                    return std::make_pair(-2, -2);
                for (i = 0, j = 0; i < k; ++i) {
                    j += C[i];
                    if (B[i] != j) {
                        //assert(SA[B[i]] != 0);
                        SA[B[i]] += n;
                    }
                    D[i] = D[i + k] = 0;
                }
                LMSsort2(T, SA, C, B, D, n, k);
                delete[] D;
            } else {
                IdxT* const D = B - k * 2;
                for (i = 0, j = 0; i < k; ++i) {
                    j += C[i];
                    if (B[i] != j) {
                        //assert(SA[B[i]] != 0);
                        SA[B[i]] += n;
                    }
                    D[i] = D[i + k] = 0;
                }
                LMSsort2(T, SA, C, B, D, n, k);
            }
            name = LMSpostproc2(SA, n, m);
        } else {
            LMSsort1(T, SA, C, B, n, k, (flags & (4 | 64)) != 0);
            name = LMSpostproc1(T, SA, n, m);
        }
    } else if (m == 1) {
        *b = j + 1;
        name = 1;
    } else {
        name = 0;
    }
    return std::make_pair(m, name);
}

template<class CharT, class IdxT>
IdxT stage3sort(const CharT* const T, IdxT* const SA, IdxT* const C, IdxT* const B,
                const IdxT n, const IdxT m, const IdxT k, const unsigned flags, const bool isBWT) {
    IdxT i, j, p, q, pidx = 0;
    CharT c0, c1;
    if ((flags & 8) != 0)
        getCounts(T, C, n, k);
    /* put all left-most S characters into their buckets */
    if (m > 1) {
        getBuckets(C, B, k, 1); /* find ends of buckets */
        i = m - 1;
        j = n;
        p = SA[m - 1];
        c1 = T[p];
        do {
            q = B[c0 = c1];
            while (q < j) {
                SA[--j] = 0;
            }
            do {
                SA[--j] = p;
                if (--i < 0)
                    break;
                p = SA[i];
            } while ((c1 = T[p]) == c0);
        } while (i >= 0);
        while (j > 0)
            SA[--j] = 0;
    }
    if (isBWT) {
        pidx = computeBWT(T, SA, C, B, n, k, (flags & (4 | 64)) != 0);
    } else {
        induceSA(T, SA, C, B, n, k, (flags & (4 | 64)) != 0);
    }
    return pidx;
}

/* find the suffix array SA of T[0..n-1] in {0..k}^n
   use a working space (excluding s and SA) of at most 2n+O(1) for a constant alphabet */
template<class CharT, class IdxT>
IdxT suffixsort(const CharT* const T, IdxT* const SA, const IdxT fs,
                const IdxT n, const IdxT k, const bool isBWT) {
    IdxT *RA = nullptr, *C = SA, *B = SA, *Cp = nullptr, *Bp = nullptr;
    IdxT i, j, m, name, newfs;
    unsigned flags = 0;
    CharT c0, c1;

    /* stage 1: reduce the problem by at least 1/2
       sort all the S-substrings */
    if (k <= 256) {
        Cp = new(std::nothrow) IdxT[k];
        if (!Cp)
            return -2;
        if (k <= fs) {
            B = SA + (n + fs - k);
            flags = 1;
        } else {
            Bp = new(std::nothrow) IdxT[k];
            if (!Bp)
                return -2;
            flags = 3;
        }
    } else if (k <= fs) {
        C = SA + (n + fs - k);
        if (k <= (fs - k)) {
            B = C - k;
            flags = 0;
        } else if (k <= 1024) {
            Bp = new(std::nothrow) IdxT[k];
            if (!Bp)
                return -2;
            flags = 2;
        } else {
            B = C;
            flags = 64 | 8;
        }
    } else {
        Cp = new(std::nothrow) IdxT[k];
        if (!Cp)
            return -2;
        Bp = Cp;
        flags = 4 | 8;
    }
    if ((n <= (std::numeric_limits<IdxT>::max() / 2)) && (2 <= (n / k))) {
        if (flags & 1) {
            flags |= ((k * 2) <= (fs - k)) ? 32 : 16;
        } else if ((flags == 0) && ((k * 2) <= (fs - k * 2))) {
            flags |= 32;
        }
    }
    std::tie(m, name) = stage1sort(T, SA, (Cp ? Cp : C), (Bp ? Bp : B), n, k, flags);
    if (m < 0) {
        if (flags & (1 | 4))
            delete[] Cp;
        if (flags & 2)
            delete[] Bp;
        return -2;
    }

    /* stage 2: solve the reduced problem
       recurse if names are not yet unique */
    if (name < m) {
        if (flags & 4)
            delete[] Cp;
        if (flags & 2)
            delete[] Bp;
        newfs = (n + fs) - (m * 2);
        if ((flags & (1 | 4 | 64)) == 0) {
            if ((k + name) <= newfs)
                newfs -= k;
            else
                flags |= 8;
        }
        //assert((n >> 1) <= (newfs + m));
        RA = SA + m + newfs;
        for (i = m + (n >> 1) - 1, j = m - 1; m <= i; --i) {
            if (SA[i] != 0)
                RA[j--] = SA[i] - 1;
        }
        if (suffixsort(RA, SA, newfs, m, name, false) != 0) {
            if (flags & 1)
                delete[] Cp;
            return -2;
        }
        i = n - 1;
        j = m - 1;
        c0 = T[n - 1];
        do { c1 = c0; } while ((--i >= 0) && ((c0 = T[i]) >= c1));
        while (i >= 0) {
            do { c1 = c0; } while ((--i >= 0) && ((c0 = T[i]) <= c1));
            if (i >= 0) {
                RA[j--] = i + 1;
                do { c1 = c0; } while ((--i >= 0) && ((c0 = T[i]) >= c1));
            }
        }
        for (i = 0; i < m; ++i)
            SA[i] = RA[SA[i]];
        if (flags & 4) {
            Cp = new(std::nothrow) IdxT[k];
            if (!Cp)
                return -2;
            Bp = Cp;
        }
        if (flags & 2) {
            Bp = new(std::nothrow) IdxT[k];
            if (!Bp) {
                if (flags & 1)
                    delete[] Cp;
                return -2;
            }
        }
    }

    /* stage 3: induce the result for the original problem */
    const IdxT pidx = stage3sort(T, SA, (Cp ? Cp : C), (Bp ? Bp : B), n, m, k, flags, isBWT);
    if(flags & (1 | 4))
        delete[] Cp;
    if(flags & 2)
        delete[] Bp;

    return pidx;
}
} /* namespace saisxx_private */
