#pragma once
#include<cstddef>    // size_t
#include<random>
#include<functional> // std::less
#include<type_traits>

template<class T, class Compare = std::less<T>>
class treap
{
    struct node;
    struct node_base
    {
        node* parent;
        node* left;
        node* right;
        node_base(node* _parent) : parent(_parent), left(nullptr), right(nullptr) {}
    };
    struct node : node_base
    {
        T value;
        double pr;
        template<class... Args> // only constructor you need, lol
        node(node* _parent, double _pr, Args&&... args) : node_base(_parent), value(std::forward<Args>(args)...), pr(_pr) {}
    };
    /* dummy node, for which the following hold:
     - dummy.parent == &dummy, i.e. the dummy node is its own parent
     - dummy.left   points to the actual root node of the tree. see root() below
     - dummy.right  should (and will) always be null in order for ++(--end()) == end() */
    node_base dummy;
    /* Note: parent pointers will never be null - saves a LOT of runtime checks. Also ptr->parent == ptr
       only for a pointer to the dummy node - this is a beautiful way to check if an iterator == end().
       Finally, normally --begin() is undefined behaviour - here as a bonus --begin() == end() */
    size_t count;
    Compare comp;

    // _this_as_node()         <=> &dummy, from which we have
    // _this_as_node()->left   <=> dummy.left <=> root() and
    // _this_as_node()->parent <=> dummy.parent <=> &dummy <=> _this_as_node()
    // _this_as_node()         <=> root()->parent as long as root()!=nullptr
          node*& root() noexcept;
    node* const& root() const noexcept;
          node* _this_as_node() noexcept;
    const node* _this_as_node() const noexcept;

    void _copyFrom(const treap&);
    const node* _find(const T&) const;

    static node* _copy(const node*, node*);
    static void _free(const node*) noexcept;
    static void updateParent(const node*, node*) noexcept;
    static void rotateLeft(node*&) noexcept;
    static void rotateRight(node*&) noexcept;
public:
    class iterator
    {
        friend class treap<T, Compare>;
        const node* ptr;

        iterator(const node*) noexcept;
    public:
        const T& operator*() const noexcept;
        const T* operator->() const noexcept;
        iterator& operator++() noexcept;
        iterator operator++(int) noexcept;
        iterator& operator--() noexcept;
        iterator operator--(int) noexcept;
        operator bool() const noexcept;
        bool operator==(const iterator&) const noexcept;
        bool operator!=(const iterator&) const noexcept;

    };

    treap(const Compare& = Compare()) noexcept(std::is_nothrow_copy_constructible<Compare>::value);
    treap(const treap&);
    treap& operator=(const treap&);
    treap(treap&&) noexcept;
    treap& operator=(treap&&) noexcept;
    ~treap();

    void insert(const T&);
    void insert(T&&);
    template<class InputIt>
    void insert(InputIt, InputIt);
    void insert(std::initializer_list<T>);

    iterator erase(iterator);
    iterator erase(iterator, iterator);
    iterator find(const T&) const;
    template<class... Args>
    void emplace(Args&&...);

    iterator begin() const noexcept;
    iterator end() const noexcept;

    size_t size() const noexcept;
    bool empty() const noexcept;
    void clear() noexcept;

    void swap(treap&) noexcept;
};

// iterator stuff
template<class T, class Compare>
treap<T, Compare>::iterator::iterator(const node* _ptr) noexcept : ptr(_ptr) {}

template<class T, class Compare>
const T& treap<T, Compare>::iterator::operator*() const noexcept
{
    return ptr->value;
}

template<class T, class Compare>
const T* treap<T, Compare>::iterator::operator->() const noexcept
{
    return &ptr->value;
}

template<class T, class Compare>
auto treap<T, Compare>::iterator::operator++() noexcept -> iterator&
{
    if (ptr->right) // find the leftmost right ancestor (if there are any)
    {
        ptr = ptr->right;
        while (ptr->left)
            ptr = ptr->left;
    }
    else // otherwise, find the smallest right predecessor
    {    // ptr->parent will never be nullptr!
        while (/*ptr->parent &&*/ptr == ptr->parent->right)
            ptr = ptr->parent;
        ptr = ptr->parent;
    }
    return *this;
}

template<class T, class Compare>
auto treap<T, Compare>::iterator::operator++(int) noexcept -> iterator
{
    iterator copy = *this;
    ++(*this);
    return copy;
}

template<class T, class Compare>
auto treap<T, Compare>::iterator::operator--() noexcept -> iterator&
{
    // symmetrical procedure for decrementing an iterator
    if (ptr->left)
    {
        ptr = ptr->left;
        while (ptr->right)
            ptr = ptr->right;
    }
    else
    {
        while (ptr == ptr->parent->left)
            ptr = ptr->parent;
        ptr = ptr->parent;
    }
    return *this;
}

template<class T, class Compare>
auto treap<T, Compare>::iterator::operator--(int) noexcept -> iterator
{
    iterator copy = *this;
    --(*this);
    return copy;
}

template<class T, class Compare>
treap<T, Compare>::iterator::operator bool() const noexcept
{
    return (ptr->parent != ptr); // also not standard in STL
}

template<class T, class Compare>
bool treap<T, Compare>::iterator::operator==(const iterator& other) const noexcept
{
    return ptr == other.ptr;
}

template<class T, class Compare>
bool treap<T, Compare>::iterator::operator!=(const iterator& other) const noexcept
{
    return !((*this) == other);
}

template<class T, class Compare>
auto treap<T, Compare>::begin() const noexcept -> iterator
{
    const node* location = _this_as_node();
    while (location->left) // keep in mind, _this_as_node()->left is the same as this->root;
        location = location->left;
    return iterator(location);
}

template<class T, class Compare>
auto treap<T, Compare>::end() const noexcept -> iterator
{
    return iterator(_this_as_node());
}

// tree stuff
template<class T, class Compare>
auto treap<T, Compare>::root() noexcept -> node*&
{
    return dummy.left; // <=> _this_as_node()->left;
}

template<class T, class Compare>
auto treap<T, Compare>::root() const noexcept -> node* const&
{
    return dummy.left;
}

template<class T, class Compare>
auto treap<T, Compare>::_this_as_node() noexcept -> node*
{
    return reinterpret_cast<node*>(&dummy);
}

template<class T, class Compare>
auto treap<T, Compare>::_this_as_node() const noexcept -> const node*
{
    return reinterpret_cast<const node*>(&dummy);
}

template<class T, class Compare>
void treap<T, Compare>::_copyFrom(const treap& other)
{
    root() = _copy(other.root(), _this_as_node());
    count = other.count;
    comp = other.comp;
}

template<class T, class Compare>
auto treap<T, Compare>::_find(const T& _val) const -> const node*
{
    const node* location = root();
    while (location)
    {
        if (comp(_val, location->value))
            location = location->left;
        else if (comp(location->value, _val))
            location = location->right;
        else
            return location; //found it
    }
    return nullptr; // not found, same as return location;
}

template<class T, class Compare>
auto treap<T, Compare>::_copy(const node* ptr, node* _pred) -> node*
{
    if (!ptr)
        return nullptr;
    node* tmp = new node(ptr->value, ptr->pr, _pred);
    tmp->left = _copy(ptr->left, tmp);
    tmp->right = _copy(ptr->right, tmp);
    return tmp;
}

template<class T, class Compare>
void treap<T, Compare>::_free(const node* ptr) noexcept
{
    if (ptr)
    {
        _free(ptr->left);
        _free(ptr->right);
        delete ptr;
    }
}

template<class T, class Compare>
void treap<T, Compare>::updateParent(const node* location, node* newChild) noexcept
{
    // receives a pointer to a node with a single child (location)
    // and replaces its child with the node pointed by newChild
    // if (location->parent) - not needed
    if (location == location->parent->left)
        location->parent->left = newChild;
    else
        location->parent->right = newChild;
    if (newChild)
        newChild->parent = location->parent;
}

template<class T, class Compare>
void treap<T, Compare>::rotateLeft(node*& ptr) noexcept
{
    // if the tree is empty or has no right subtree, rotating left is impossible
    if (!ptr || !ptr->right)
        return;
    node* tmp = ptr->right;
    ptr->right = tmp->left;
    if (ptr->right)
        ptr->right->parent = ptr;
    tmp->parent = ptr->parent;
    tmp->left = ptr;
    ptr->parent = tmp;
    ptr = tmp;
}

template<class T, class Compare>
void treap<T, Compare>::rotateRight(node*& ptr) noexcept
{
    // if the tree is empty or has no left subtree, rotating right is impossible
    if (!ptr || !ptr->left)
        return;
    node* tmp = ptr->left;
    ptr->left = tmp->right;
    if (ptr->left)
        ptr->left->parent = ptr;
    tmp->parent = ptr->parent;
    tmp->right = ptr;
    ptr->parent = tmp;
    ptr = tmp;
}

template<class T, class Compare>
treap<T, Compare>::treap(const Compare& _cmp) noexcept(std::is_nothrow_copy_constructible<Compare>::value)
    : dummy(_this_as_node()), count(0), comp(_cmp) {}

template<class T, class Compare>
treap<T, Compare>::treap(const treap& other) : treap()
{
    _copyFrom(other);
}

template<class T, class Compare>
auto treap<T, Compare>::operator=(const treap& other) -> treap&
{
    if (this != &other)
    {
        clear();
        _copyFrom(other);
    }
    return *this;
}

template<class T, class Compare>
treap<T, Compare>::treap(treap&& other) noexcept : treap()
{
    swap(other);
}

template<class T, class Compare>
auto treap<T, Compare>::operator=(treap&& other) noexcept -> treap&
{
    if (this != &other)
    {
        clear();
        swap(other);
    }
    return *this;
}

template<class T, class Compare>
treap<T, Compare>::~treap()
{
    clear();
}

template<class T, class Compare>
void treap<T, Compare>::insert(const T& _val)
{
    emplace(_val);
}

template<class T, class Compare>
void treap<T, Compare>::insert(T&& _val)
{
    emplace(std::forward<T>(_val));
}

template<class T, class Compare>
template<class InputIt>
void treap<T, Compare>::insert(InputIt _from, InputIt _to)
{
    for (auto it = _from; it != _to; ++it)
        insert(*it);
}

template<class T, class Compare>
void treap<T, Compare>::insert(std::initializer_list<T> _il)
{
    insert(_il.begin(), _il.end());
}

template<class T, class Compare>
auto treap<T, Compare>::erase(iterator _it) -> iterator
{
    if (_it == end()) // bonus: in STL erase(end()) is undefined behaviour
        return _it;
    const node* location = _it.ptr;
    ++_it;
    // След като намерим търсената стойност (location->value),
    // имаме три случая - възелът има 0, 1 или 2 наследника.
    // И в трите случая намаляваме броя стойности в дървото.
    --count;
    if (!location->left && !location->right)
    {
        // Ако възелът няма наследници, значи е листо и го "отрязваме", 
        // като единствено известяваме родителя си че вече няма дете.
        updateParent(location, nullptr);
    }
    else if (!location->left || !location->right)
    {
        // Ако има само един наследник, то родителят ни го "осиновява".
        // Проверяваме дали детето е ляво или дясно
        node* onlyChild = ((location->left) ? location->left : location->right);
        updateParent(location, onlyChild);
    }
    else
    {
        // Намираме най-голямата стойност, по-малка от търсената.
        // Този възел "изваждаме" от дървото (аналогично на предишния случай)
        // и вмъкваме на мястото на премахваната стойност.
        node* candidate = location->left;
        while (candidate->right)
            candidate = candidate->right;
        // знаем, че ако тази най-голяма по-малка стойност има дете, то може да е само ляво
        updateParent(candidate, candidate->left);

        // актуализираме указателите към и от децата на "кандидата"
        candidate->left = location->left;
        candidate->right = location->right;
        // (!) ако кандидатът е ляво дете на location и няма свое ляво дете,
        // то след updateParent location->left става nullptr!!!
        if (location->left)
            location->left->parent = candidate;
        // проверяваме само лявото дете, понеже търсим в лявото поддърво и дясното не се променя
        location->right->parent = candidate;

        // казваме на родителя на location, че вече candidate му е син
        // това актуализира и parent указателя на candidate
        updateParent(location, candidate);
    }
    // най-накрая освобождаваме паметта
    delete location;
    return _it;
}

template<class T, class Compare>
auto treap<T, Compare>::erase(iterator _from, iterator _to) -> iterator
{
    iterator res = end();
    for (auto it = _from; it != _to; ++it)
        res = erase(it);
    return res;
}

template<class T, class Compare>
auto treap<T, Compare>::find(const T& _val) const -> iterator
{
    const node* location = _find(_val);
    return (location ? iterator(location) : end());
}

template<class T, class Compare>
template<class ...Args>
void treap<T, Compare>::emplace(Args&&... _args)
{
    node* newNode = new node(nullptr, 0, std::forward<Args>(_args)...);
    const T& newVal = newNode->value;
    node* location = root();
    node* parent = _this_as_node(); // <=> root()->parent, but faster
    bool isLeft = true;
    while (location)
    {
        parent = location;
        if (comp(newVal, location->value))
        {
            location = location->left;
            isLeft = true;
        }
        else
        {
            location = location->right;
            isLeft = false;
        }
    }
    location = newNode;
    location->parent = parent;
    if (isLeft)
        parent->left = location;
    else
        parent->right = location;

    ++count;
}

template<class T, class Compare>
size_t treap<T, Compare>::size() const noexcept
{
    return count;
}

template<class T, class Compare>
bool treap<T, Compare>::empty() const noexcept
{
    return (size() == 0);
}

template<class T, class Compare>
void treap<T, Compare>::clear() noexcept
{
    _free(root());
    root() = nullptr;
    count = 0;
}

template<class T, class Compare>
void treap<T, Compare>::swap(treap& other) noexcept
{
    using std::swap;
    swap(root(), other.root());
    swap(count, other.count);
    swap(comp, other.comp);
}

template<class T, class Compare>
void swap(treap<T, Compare>& lhs, treap<T, Compare>& rhs) noexcept
{
    lhs.swap(rhs);
}
