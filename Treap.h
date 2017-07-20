#pragma once
#include<cstddef>    // size_t, offsetof
#include<random>
#include<functional> // std::less
#include<type_traits>

template<class T, class Compare = std::less<T>>
class Treap
{
    struct Node
    {
        Node* parent;
        Node* left;
        Node* right;
        T value;
        double pr;

        Node(const T& _val, double _pr, Node* _parent) : value(_val), pr(_pr),
            parent(_parent), left(nullptr), right(nullptr) {}
    };

    Node* const dummy_parent; // will always point to itself, i.e. the dummy node is its own parent
    Node*       root;         // dummy "left", actually points to the root node
    Node* const dummy_right;  // should always be null in order for ++(--end()) == end()
        // note: parent pointers will never be null - saves a LOT of runtime checks. Also 
        // ptr->parent == ptr only for this dummy node - this is a beautiful way to check if an iterator == end() 
        // Finally, normally --begin() is undefined behaviour - here as a bonus --begin() == end()
    size_t count;
    Compare comp;

    void _copyFrom(const Treap&);
    const Node* _find(const T&) const;
    // _this_as_node()->parent <=> this->dummy_parent
    // _this_as_node()->left   <=> this->root (!)
    // _this_as_node()->right  <=> this->dummy_right
          Node* _this_as_node() noexcept;
    const Node* _this_as_node() const noexcept;

    static Node* _copy(const Node*, Node*);
    static void _free(const Node*) noexcept;
    static void updateParent(const Node*, Node*) noexcept;
    static void rotateLeft(Node*&) noexcept;
    static void rotateRight(Node*&) noexcept;
public:
    class iterator
    {
        friend class Treap<T, Compare>;
        const Node* ptr;

        iterator(const Node*) noexcept;
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

    Treap(const Compare& = Compare()) noexcept(std::is_nothrow_copy_constructible<Compare>::value);
    Treap(const Treap&);
    Treap& operator=(const Treap&);
    Treap(Treap&&) noexcept;
    Treap& operator=(Treap&&) noexcept;
    ~Treap();

    void insert(const T&);
    iterator erase(iterator);
    iterator find(const T&) const;

    iterator begin() const noexcept;
    iterator end() const noexcept;

    size_t size() const noexcept;
    bool empty() const noexcept;
    void clear() noexcept;

    void swap(Treap&) noexcept;
};

// iterator stuff
template<class T, class Compare>
Treap<T, Compare>::iterator::iterator(const Node* _ptr) noexcept : ptr(_ptr) {}

template<class T, class Compare>
const T& Treap<T, Compare>::iterator::operator*() const noexcept
{
    return ptr->value;
}

template<class T, class Compare>
const T* Treap<T, Compare>::iterator::operator->() const noexcept
{
    return &ptr->value;
}

template<class T, class Compare>
auto Treap<T, Compare>::iterator::operator++() noexcept -> iterator&
{
    if (ptr->right)
    {
        // find the leftmost right ancestor (if there are any)
        ptr = ptr->right;
        while (ptr->left)
            ptr = ptr->left;
    }
    else
    {
        // otherwise, find the smallest right predecessor
        // ptr->parent will never be nullptr!
        while (/*ptr->parent &&*/ptr == ptr->parent->right)
            ptr = ptr->parent;
        ptr = ptr->parent;
    }
    return *this;
}

template<class T, class Compare>
auto Treap<T, Compare>::iterator::operator++(int) noexcept -> iterator
{
    iterator copy = *this;
    ++(*this);
    return copy;
}

template<class T, class Compare>
auto Treap<T, Compare>::iterator::operator--() noexcept -> iterator&
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
auto Treap<T, Compare>::iterator::operator--(int) noexcept -> iterator
{
    iterator copy = *this;
    --(*this);
    return copy;
}

template<class T, class Compare>
Treap<T, Compare>::iterator::operator bool() const noexcept
{
    return (ptr->parent != ptr); // also not standard in STL
}

template<class T, class Compare>
bool Treap<T, Compare>::iterator::operator==(const iterator& other) const noexcept
{
    return ptr == other.ptr;
}

template<class T, class Compare>
bool Treap<T, Compare>::iterator::operator!=(const iterator& other) const noexcept
{
    return !((*this) == other);
}

template<class T, class Compare>
auto Treap<T, Compare>::begin() const noexcept -> iterator
{
    const Node* location = _this_as_node();
    while (location->left) // keep in mind, _this_as_node()->left is the same as this->root;
        location = location->left;
    return iterator(location);
}

template<class T, class Compare>
auto Treap<T, Compare>::end() const noexcept -> iterator
{
    return iterator(_this_as_node());
}

// tree stuff
template<class T, class Compare>
auto Treap<T, Compare>::_copy(const Node* ptr, Node* _pred) -> Node*
{
    if (!ptr)
        return nullptr;
    Node* tmp = new Node(ptr->value, ptr->pr, _pred);
    tmp->left = _copy(ptr->left, tmp);
    tmp->right = _copy(ptr->right, tmp);
    return tmp;
}

template<class T, class Compare>
void Treap<T, Compare>::_free(const Node* ptr) noexcept
{
    if (ptr)
    {
        _free(ptr->left);
        _free(ptr->right);
        delete ptr;
    }
}

template<class T, class Compare>
void Treap<T, Compare>::_copyFrom(const Treap& other)
{
    root = _copy(other.root, _this_as_node());
    count = other.count;
    comp = other.comp;
}

template<class T, class Compare>
auto Treap<T, Compare>::_find(const T& _val) const -> const Node*
{
    const Node* location = root;
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
void Treap<T, Compare>::updateParent(const Node* location, Node* newChild) noexcept
{
    // receives a pointer to a node with a single child (location)
    // and replaces its with the node pointed by newChild
    // if (location->parent) - not needed
    if (location == location->parent->left)
        location->parent->left = newChild;
    else
        location->parent->right = newChild;
    if (newChild)
        newChild->parent = location->parent;
}

template<class T, class Compare>
auto Treap<T, Compare>::_this_as_node() noexcept -> Node*
{
    return reinterpret_cast<Node*>(this);
}

template<class T, class Compare>
auto Treap<T, Compare>::_this_as_node() const noexcept -> const Node*
{
    return reinterpret_cast<const Node*>(this);
}

template<class T, class Compare>
void Treap<T, Compare>::rotateLeft(Node*& ptr) noexcept
{
    // if the tree is empty or has no right subtree, rotating left is impossible
    if (!ptr || !ptr->right)
        return;
    Node* tmp = ptr->right;
    ptr->right = tmp->left;
    if (ptr->right)
        ptr->right->parent = ptr;
    tmp->parent = ptr->parent;
    tmp->left = ptr;
    ptr->parent = tmp;
    ptr = tmp;
}

template<class T, class Compare>
void Treap<T, Compare>::rotateRight(Node*& ptr) noexcept
{
    // if the tree is empty or has no left subtree, rotating right is impossible
    if (!ptr || !ptr->left)
        return;
    Node* tmp = ptr->left;
    ptr->left = tmp->right;
    if (ptr->left)
        ptr->left->parent = ptr;
    tmp->parent = ptr->parent;
    tmp->right = ptr;
    ptr->parent = tmp;
    ptr = tmp;
}

template<class T, class Compare>
Treap<T, Compare>::Treap(const Compare& _cmp) noexcept(std::is_nothrow_copy_constructible<Compare>::value)
    : dummy_parent(_this_as_node()), root(nullptr), dummy_right(nullptr), count(0), comp(_cmp) {}

template<class T, class Compare>
Treap<T, Compare>::Treap(const Treap& other) : Treap()
{
    _copyFrom(other);
}

template<class T, class Compare>
auto Treap<T, Compare>::operator=(const Treap& other) -> Treap&
{
    if (this != &other)
    {
        clear();
        _copyFrom(other);
    }
    return *this;
}

template<class T, class Compare>
Treap<T, Compare>::Treap(Treap&& other) noexcept : Treap()
{
    swap(other);
}

template<class T, class Compare>
auto Treap<T, Compare>::operator=(Treap&& other) noexcept -> Treap&
{
    if (this != &other)
    {
        clear();
        swap(other);
    }
    return *this;
}

template<class T, class Compare>
Treap<T, Compare>::~Treap()
{
    clear();
}

template<class T, class Compare>
void Treap<T, Compare>::insert(const T& _val)
{
    //_insert(_val, 0, root, _this_as_node());
    Node* location = root;
    Node* parent = _this_as_node();
    bool isLeft = true;
    while (location)
    {
        parent = location;
        if (comp(_val, location->value))
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
    location = new Node(_val, 0, parent);
    if (isLeft)
        parent->left = location;
    else
        parent->right = location;

    ++count;
}

template<class T, class Compare>
auto Treap<T, Compare>::erase(iterator _it) -> iterator
{
    if (_it == end()) // bonus: in STL erase(end()) is undefined behaviour
        return _it;
    const Node* location = _it.ptr;
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
        Node* onlyChild = ((location->left) ? location->left : location->right);
        updateParent(location, onlyChild);
    }
    else
    {
        // Намираме най-голямата стойност, по-малка от търсената.
        // Този възел "изваждаме" от дървото (аналогично на предишния случай)
        // и вмъкваме на мястото на премахваната стойност.
        Node* candidate = location->left;
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
auto Treap<T, Compare>::find(const T& _val) const -> iterator
{
    const Node* location = _find(_val);
    return (location ? iterator(location) : end());
}

template<class T, class Compare>
size_t Treap<T, Compare>::size() const noexcept
{
    return count;
}

template<class T, class Compare>
bool Treap<T, Compare>::empty() const noexcept
{
    return (size() == 0);
}

template<class T, class Compare>
void Treap<T, Compare>::clear() noexcept
{
    _free(root);
    root = nullptr;
    count = 0;
}

template<class T, class Compare>
void Treap<T, Compare>::swap(Treap& other) noexcept
{
    using std::swap;
    swap(root, other.root);
    swap(count, other.count);
    swap(comp, other.comp);
}

template<class T, class Compare>
void swap(Treap<T, Compare>& lhs, Treap<T, Compare>& rhs) noexcept
{
    lhs.swap(rhs);
}
