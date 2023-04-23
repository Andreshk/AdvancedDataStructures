#pragma once
#include <cassert>
#include <utility> // std::exchange

#ifdef NDEBUG // Same macro that controls assert()
#define DEBUG_ONLY(x)
#else
#define DEBUG_ONLY(x) x
#endif // NDEBUG

// Circular doubly-linked list, with standard O(1) insertion/deletion & referential stability.
template <typename T>
class DList {
    // Important: list nodes must be as barebones as possible
    struct Node {
        T val;
        Node* prev DEBUG_ONLY(= nullptr);
        Node* next DEBUG_ONLY(= nullptr);
        Node(const T& val) : val{ val } {}
        Node(T&& val) : val{ std::move(val) } {}
    };

    Node* head = nullptr;
    int numValues = 0;
    // Checks whether a given node is part of this list, i.e. reachable from head.
    // Obviously O(n), use with care - f.e. for testing.
    bool reachable(const Node* ptr) const noexcept {
        Node* curr = head;
        do {
            if (curr == ptr) {
                return true;
            }
            curr = curr->next;
        } while (curr != head);
        return false;
    }
public:
    DList() = default;
    // Convenience ctor
    DList(std::initializer_list<T> il) : DList{} { for (const T& x : il) { insert(x); } }
    // No list copying, only moving
    DList(const DList&) = delete;
    DList& operator=(const DList&) = delete;
    DList(DList&& other) noexcept
        : head{ std::exchange(other.head, nullptr) }, numValues{ std::exchange(other.numValues, 0) } {}
    DList& operator=(DList&& other) noexcept {
        if (this != &other) {
            clear();
            head = std::exchange(other.head, nullptr);
            numValues = std::exchange(other.numValues, 0);
        }
        return *this;
    }
    ~DList() { clear(); }
    // We can iterate, but can't tell when we're done. Also as barebones as possible
    class iterator {
        Node* ptr = nullptr;
        friend class DList; // Only the list can construct non-empty iterators to itself
        iterator(Node* ptr) : ptr{ ptr } {}
    public:
        iterator() = default;
        T& operator*() const { return ptr->val; }
        T* operator->() const { return &ptr->val; }
        operator bool() const { return (ptr != nullptr); }
        iterator& operator++() { ptr = ptr->next; return *this; }
        iterator& operator--() { ptr = ptr->prev; return *this; }
        bool operator==(const iterator&) const = default;
    };
    // Inserts a value at the end of the list (so, immediately before head)
    iterator insert(const T& val) {
        return insert(handle{ new Node{val} });
    }
    iterator insert(T&& val) {
        return insert(handle{ new Node{std::move(val)} });
    }
    // Removes a value from the list, resetting the iterator to avoid use-after-free
    void remove(iterator& it) noexcept {
        handle h = extract(it);
        it.ptr = nullptr;
        // memory freed in ~handle()
    }
    // Checks whether the list is empty
    [[nodiscard("Did you mean .clear()?")]]
    bool empty() const noexcept {
        assert((head == nullptr) == (numValues == 0));
        return (head == nullptr);
    }
    // Returns the # of values in the list
    int size() const noexcept { return numValues; }
    // Empty the list, freeing all allocated memory
    void clear() noexcept {
        if (!head) { return; }
        Node* ptr = head;
        do {
            Node* tmp = ptr->next;
            delete ptr;
            ptr = tmp;
        } while (ptr != head);
        head = nullptr;
        numValues = 0;
    }
    // Append another list, leaving it empty afterwards
    void append(DList&& other) noexcept {
        assert(this != &other); // Copying nodes would be needed
        if (!other.head) {
            return; // Nothing to do
        }
        if (!head) {
            head = other.head;
            numValues = other.numValues;
        } else {
            Node* last = head->prev;
            Node* last2 = other.head->prev;
            last->next = other.head;
            other.head->prev = last;
            last2->next = head;
            head->prev = last2;
            numValues += other.numValues;
        }
        // This is a "destructive" operation for the appended list
        other.head = nullptr;
        other.numValues = 0;
    }
    // Returns an iterator to the first value in the list.
    iterator front() const noexcept { return { head }; }
    // "Rotate" the list to have a given node as the head
    // (reachable in O(1)) - trivial due to the circularity.
    void rotate(iterator it) noexcept {
        assert(head ? it.ptr && reachable(it.ptr) : !it.ptr);
        head = it.ptr;
    }

    // A handle to an orphaned node, used to move nodes (with the contained values) between lists.
    // Obtained only from extract(), owns the memory for the node until it's used in insert().
    class handle {
        friend class DList; // Only the list can manage handles, nothing for the user to do
        Node* ptr;
        handle(Node* ptr) : ptr{ ptr } {}
    public:
        handle() : ptr{ nullptr } {}
        handle(handle&& other) noexcept : ptr{ std::exchange(other.ptr, nullptr) } {}
        handle& operator=(handle&& other) noexcept {
            ptr = std::exchange(other.ptr, nullptr);
            return *this;
        }
        // Avoid memory leaks from un-inserted handles
        ~handle() {
            if (ptr) { delete ptr; }
        }
        T& operator*() const { return ptr->val; }
        T* operator->() const { return &ptr->val; }
        operator bool() const { return (ptr != nullptr); }
        // Convert to reguler, non-owning iterator - not for direct use (!)
        iterator toIter() const noexcept { return { ptr }; }
    };
    // Extracts a node from the list, without destoying the value or deallocating the memory.
    // This node can then be inserted into some (possibly other) list.
    handle extract(iterator it) noexcept {
        Node* ptr = it.ptr;
        // Mostly safe otherwise - unless it's the only node in its list! So, better safe than sorry
        assert(reachable(ptr));
        // Advance to reduce # of cases afterwards
        if (ptr == head) {
            head = head->next;
        }
        if (ptr == head) {
            assert(head->prev == head && head->next == head);
            head = nullptr;
        } else {
            Node* prev = ptr->prev;
            Node* next = ptr->next;
            assert(ptr == prev->next && ptr == next->prev);
            prev->next = next;
            next->prev = prev;
        }
        DEBUG_ONLY(ptr->prev = ptr->next = nullptr); // not relied on :)
        --numValues;
        return { ptr };
    }
    // Inserts a value at the end of the list (so, immediately before head)
    // Note that it resets the handle, so it doesn't attempt to free the memory on scope exit later.
    iterator insert(handle&& hnd) noexcept {
        // The "back" in a circular list is just before head, or the "front"
        assert(hnd.ptr->prev == nullptr && hnd.ptr->next == nullptr);
        if (head == nullptr) {
            head = hnd.ptr;
            head->prev = head->next = head;
        } else {
            head->prev->next = hnd.ptr;
            hnd.ptr->prev = head->prev;
            head->prev = hnd.ptr;
            hnd.ptr->next = head;
        }
        ++numValues;
        return { std::exchange(hnd.ptr, nullptr) };
    }

    // Note: looping over values requires a different kind of iterators, storing more data!
    class loop_iterator {
        iterator it;
        bool b = false;
        friend class DList;
        loop_iterator(iterator it, bool b) : it{ it }, b{ b } {}
    public:
        using value_type = T;
        loop_iterator() = default;
        T& operator*() const { return *it; }
        T* operator->() const { return &*it; }
        operator bool() const { return bool(it); } // Only a default-constructed can be empty
        loop_iterator& operator++() { ++it; b = false; return *this; }
        loop_iterator operator++(int) { auto copy = *this; ++*this; return copy; }
        bool operator==(const loop_iterator&) const = default;
    };
    loop_iterator begin() const { return { {head}, !empty() }; }
    loop_iterator end() const { return { {head}, false }; }
};