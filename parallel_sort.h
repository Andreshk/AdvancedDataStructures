#pragma once
#include <algorithm>	// std::sort, std::inplace_merge
#include <vector>
#include <thread>
#include <atomic>
#include <iterator>		// std::iterator_traits
#include <type_traits>	// std::enable_if, std::is_same

// uncomment for detailed thread interaction info
//#define PARALLEL_SORT_DEBUG

#ifdef PARALLEL_SORT_DEBUG
#include <iostream>
#include <mutex>
std::mutex coutmtx;
#endif // PARALLEL_SORT_DEBUG

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
	// clamp nthreads to a valid value
	if (nthreads == 0)
		nthreads = 1;
	// not a narrowing conversion actually
	const size_t n = size_t(to - from);
	if (nthreads > n)
		nthreads = n;
    // create an 'impl' instance, which does all the work
    impl<RanIt, Pred> sorter{ from,to,nthreads,pr };
}

// Encapsulates everything needed for parallel sorting and merging:
// thread array, per-thread subarray info, number of threads active, etc.
template<class RanIt, class Pred>
class impl
{
    friend void parallel_sort<RanIt, Pred>(RanIt, RanIt, size_t, Pred);

    // Threads array, total thread count & smallest >=nthreads power of 2
    std::vector<std::thread> ths;
    const size_t nthreads;
	const size_t nthrpow2;
    std::atomic<size_t> ths_created;
#ifdef PARALLEL_SORT_DEBUG
	std::atomic<size_t> ths_joined;
#endif // PARALLEL_SORT_DEBUG

    // Per-thread subarray info
    RanIt* froms;
    RanIt* tos;
    Pred pr;

	static constexpr size_t smallestPowOf2(size_t n)
	{
		size_t res = 1;
		while (res < n) res *= 2;
		return res;
	}

    impl(RanIt from, RanIt to, size_t _nthreads, Pred _pr)
        : ths{}, nthreads{ _nthreads }, nthrpow2{ smallestPowOf2(_nthreads) }
		, ths_created{ 0 }, froms{ new RanIt[_nthreads] }, tos{ new RanIt[_nthreads] }
        , pr{ _pr }
#ifdef PARALLEL_SORT_DEBUG
		, ths_joined{ 0 }
#endif // PARALLEL_SORT_DEBUG
    {
		// reserve memory to guarantee that no vector reallocations will occur
		// we need this guarantee for the interactions between threads to work properly (!)
		ths.reserve(nthreads);
        const size_t subSize = (to - from) / nthreads;
        for (size_t idx = 0; idx < nthreads; ++idx)
        {
            // calculate the subarrays each thread has to sort
            froms[idx] = from + subSize*idx;
            tos[idx] = froms[idx] + subSize;
        }
        // the final subarray may be a little bit bigger (<nthreads more elements than the rest)
        tos[nthreads - 1] = to;
        // invariant: at _any discrete moment of time_ 'ths_created'
		// is not greater than the # of actually constructed threads
        for (size_t idx = 0; idx < nthreads; ++idx)
        {
            ths.emplace_back(&impl::sort, this, idx);
            ++ths_created;
        }
        // all other worker threads will be taken care of by ths[0] before it finishes
        ths[0].join();
#ifdef PARALLEL_SORT_DEBUG
		++ths_joined;
		std::cout << "nthreads=" << nthreads << "\nnthrpow2=" << nthrpow2
				  << "\nths_created=" << ths_created << "\nths_joined=" << ths_joined << "\n";
#endif // PARALLEL_SORT_DEBUG

    }
    ~impl()
    {
        delete[] froms;
        delete[] tos;
    }

    // per-thread work
    void sort(size_t idx)
    {
        // sort the subarray naively
        std::sort(froms[idx], tos[idx], pr);
        // then merge in parallel
        merge_threads(idx);
    }

    /* tl;dr: parallel merge
    * Suppose we have 8 worker threads. After all per-thread work is done,
    * the threads (and the subarrays) are merged pairwise _in parallel_
    * until there is only one thread left. (here i,j merging means thread
    * #i merges the subarray of thread #j into its own and kills thread #j, denoted i<-j)
    * 0<-1 2<-3 4<-5 6<-7, then
    * 0 <- 2    4 <- 6, then
    * 0    <-   4
    * and afterwards only worker thread #0 will be alive and have the entire array sorted.
    */
    void merge_threads(size_t idx)
    {
        for (size_t i = 1; i < nthrpow2; i *= 2)
            if (idx & i)
                return; // wait to die
            else
            {
                // find the victim (thread to merge)...
                size_t idx_to_kill = idx + i;
				// nobody left to kill, so wait to die as well
				if (idx_to_kill >= nthreads)
					return;
                // make sure the victim has actually been created...
                // (needed when per-thread work takes less time than thread creation)
                while (idx_to_kill >= ths_created) {}
                // ...then kill it (after it's done its job)
                ths[idx_to_kill].join();
#ifdef PARALLEL_SORT_DEBUG
				++ths_joined;
				coutmtx.lock();
				std::cout << "#" << idx << " <- #" << idx_to_kill << "\n";
				coutmtx.unlock();
#endif // PARALLEL_SORT_DEBUG
                // this thread's subarray and the victim's subarray are actually consecutive
                std::inplace_merge(froms[idx], froms[idx_to_kill], tos[idx_to_kill]);
                // some false cache sharing here, but no need for atomic operations
                // (no two threads write to a same location, and the read is after all writes to this location)
                tos[idx] = tos[idx_to_kill];
            }
    }
};
