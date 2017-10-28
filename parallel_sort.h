#pragma once
#include <algorithm>	// std::sort, std::inplace_merge
#include <thread>
#include <atomic>
#include <iterator>		// std::iterator_traits
#include <exception>	// std::invalid_argument
#include <type_traits>	// std::enable_if, std::is_same

// Really looking forward to Concepts...
template <class RanIt>
using is_random_iterator = typename std::is_same<typename std::iterator_traits<RanIt>::iterator_category, std::random_access_iterator_tag>;

// Forward declaration
template<class RanIt, class Pred>
class impl;

template<class RanIt>
void parallel_sort(RanIt from, RanIt to, size_t nthreads)
{
    // std::less<> deduces parameter and return types (since c++14)
    // use std::less<typename std::iterator_traits<RanIt>::value_type> instead
    parallel_sort<RanIt, std::less<>>(from, to, nthreads);
}

template <class RanIt, class Pred>
std::enable_if_t<is_random_iterator<RanIt>::value>
    parallel_sort(RanIt from, RanIt to, size_t nthreads, Pred pr = Pred{})
{
    if (!impl<RanIt, Pred>::isPowOf2(nthreads))
        throw std::invalid_argument("Thread count for parallel_sort must be a power of 2!");
    // create an 'impl' instance, which does all the work
    impl<RanIt, Pred> sorter{ from,to,nthreads,pr };
}

// Encapsulates everything needed for parallel sorting and merging:
// thread array, per-thread subarray info, number of threads active, etc.
template<class RanIt, class Pred>
class impl
{
    friend void parallel_sort<RanIt, Pred>(RanIt, RanIt, size_t, Pred);

    // Threads array, total thread count & actually created count
    std::thread* const ths;
    const size_t nthreads;
    std::atomic<size_t> ths_created;
    // Per-thread subarray info
    RanIt* froms;
    RanIt* tos;
    Pred pr;

    static constexpr bool isPowOf2(size_t n) { return (!(n & (n - 1)) && n); }

    impl(RanIt from, RanIt to, size_t _nthreads, Pred _pr)
        // this memory will be initialized (i.e. actual threads created) later
        : ths{ reinterpret_cast<std::thread*>(malloc(_nthreads * sizeof(std::thread))) }
        , nthreads{ _nthreads }, ths_created{ 0 }
        , froms{ new RanIt[_nthreads] }, tos{ new RanIt[_nthreads] }
        , pr{ _pr }
    {
        const size_t subSize = (to - from) / nthreads;
        for (size_t idx = 0; idx < nthreads; ++idx)
        {
            // calculate the subarrays each thread has to sort
            froms[idx] = from + subSize*idx;
            tos[idx] = froms[idx] + subSize;
        }
        // the final subarray may be a little bit bigger (<nthreads more elements than the rest)
        tos[nthreads - 1] = to;
        // invariant: at any moment of time 'ths_created' is not greater than the # of constructed threads
        for (size_t idx = 0; idx < nthreads; ++idx)
        {
            new (ths + idx) std::thread{ &impl::sort, this, idx };
            ++ths_created;
        }
        // all other worker threads will be taken care of by ths[0] before it finishes
        ths[0].join();
        ths[0].~thread();
    }
    ~impl()
    {
        free(ths);
        delete[] froms;
        delete[] tos;
    }

    // per-thread work
    void sort(size_t idx)
    {
        // sort the subarray naively
        std::sort(froms[idx], tos[idx], pr);
        // then parallel merge
        merge_threads(idx);
    }

    /* tl;dr: parallel merge
    * Assume we have 8 worker threads. After all per-thread work is done,
    * the threads (and the array pieces) are merged pairwise _in parallel_
    * until there is only one thread left. (here i,j merging means thread
    * #i merges the subarray of thread #j into its own and kills thread #j, denoted i<-j)
    * 0<-1 2<-3 4<-5 6<-7, then
    * 0 <- 2    4 <- 6, then
    * 0    <-   4
    * and afterwards only thread #0 will be alive and have the entire array sorted.
    */
    void merge_threads(size_t idx)
    {
        for (size_t i = 1; i < nthreads; i *= 2)
            if (idx & i)
                return; // wait to die
            else
            {
                // find the victim (thread to merge)...
                size_t idx_to_kill = idx + i;
                // make sure the victim has actually been created...
                // (needed when per-thread work takes less time than thread creation)
                while (idx_to_kill >= ths_created) {}
                // ...then kill it (after it's done its job)
                ths[idx_to_kill].join();
                ths[idx_to_kill].~thread();
                // this thread's subarray and the victim's subarray are actually consecutive
                std::inplace_merge(froms[idx], froms[idx_to_kill], tos[idx_to_kill]);
                // some false cache sharing here, but no need for atomic operations
                // (no two threads write to a same location, and the read is after all writes to this location)
                tos[idx] = tos[idx_to_kill];
            }
    }
};
